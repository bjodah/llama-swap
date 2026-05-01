;;; llama-swap-sse.el --- Long-lived SSE connection for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Manages the persistent curl process for /api/events.
;; Parses SSE frames and dispatches to llama-swap-state mutators.
;; Handles reconnect with bounded exponential backoff.
;;
;; SSE frame format (blank-line separated):
;;   event:message
;;   data:{"type":"modelStatus","data":"[...]"}
;;
;; The outer data value is a JSON string whose content is itself JSON.

;;; Code:

(require 'json)
(require 'llama-swap-auth)
(require 'llama-swap-state)

;;; --- Internal state ---

(defvar llama-swap-sse--process nil
  "The current long-lived SSE curl process, or nil.")

(defvar llama-swap-sse--remainder ""
  "Incomplete SSE frame accumulated between process filter calls.")

(defvar llama-swap-sse--reconnect-timer nil
  "Timer object for the next reconnect attempt, or nil.")

(defvar llama-swap-sse--reconnect-delay 1.0
  "Current reconnect delay in seconds.")

(defconst llama-swap-sse--reconnect-delay-min 1.0
  "Minimum reconnect delay in seconds.")

(defconst llama-swap-sse--reconnect-delay-max 5.0
  "Maximum reconnect delay in seconds (matches Svelte store cap).")

(defvar llama-swap-sse--intentional-stop nil
  "Non-nil when the stop was requested by the user (no reconnect).")

;;; --- Public API ---

(defun llama-swap-sse-connected-p ()
  "Return non-nil if the SSE stream is currently connected."
  (eq llama-swap-state--connection 'connected))

(defun llama-swap-sse-start ()
  "Start the SSE event stream.  No-op if already running."
  (when (and llama-swap-sse--process
             (process-live-p llama-swap-sse--process))
    (message "llama-swap: SSE stream already running")
    (cl-return-from llama-swap-sse-start nil))
  (setq llama-swap-sse--intentional-stop nil)
  (llama-swap-sse--connect))

(defun llama-swap-sse-stop ()
  "Stop the SSE event stream and cancel any pending reconnect."
  (setq llama-swap-sse--intentional-stop t)
  (llama-swap-sse--cancel-reconnect-timer)
  (when (and llama-swap-sse--process
             (process-live-p llama-swap-sse--process))
    (delete-process llama-swap-sse--process))
  (setq llama-swap-sse--process nil
        llama-swap-sse--remainder "")
  (llama-swap-state-set-connection 'disconnected))

;;; --- Connection ---

(defun llama-swap-sse--connect ()
  "Start the curl process for /api/events."
  (llama-swap-state-next-generation)
  (let* ((gen        llama-swap-state--event-generation)
         (url        (concat (symbol-value 'llama-swap-base-url) "/api/events"))
         (auth-hdr   (llama-swap-auth-header))
         (curl-prog  (symbol-value 'llama-swap-curl-program))
         (args       (append
                      (list "--silent" "--show-error" "--no-buffer" "-N")
                      (when auth-hdr (list "-H" auth-hdr))
                      (list "-H" "Accept: text/event-stream"
                            url)))
         (proc-buf   (generate-new-buffer " *llama-swap-sse*"))
         (proc       (apply #'start-process "llama-swap-sse" proc-buf
                            curl-prog args)))
    (setq llama-swap-sse--process proc
          llama-swap-sse--remainder "")
    (llama-swap-state-set-connection 'connecting)
    (set-process-filter proc
                        (llama-swap-sse--make-filter gen))
    (set-process-sentinel proc
                          (llama-swap-sse--make-sentinel gen))
    proc))

;;; --- Process filter ---

(defun llama-swap-sse--make-filter (gen)
  "Return a process filter for generation GEN."
  (lambda (proc output)
    (unless (= gen llama-swap-state--event-generation)
      (delete-process proc)
      (cl-return-from nil nil))
    (setq llama-swap-sse--remainder
          (concat llama-swap-sse--remainder output))
    (llama-swap-sse--process-remainder gen)))

(defun llama-swap-sse--process-remainder (gen)
  "Parse and dispatch all complete SSE frames in `llama-swap-sse--remainder'.
Leaves incomplete trailing data in place."
  (let ((text llama-swap-sse--remainder))
    ;; Normalise CRLF to LF
    (setq text (replace-regexp-in-string "\r\n" "\n" text))
    ;; Process frames (blank-line separated)
    (while (string-match "\n\n" text)
      (let* ((end   (match-end 0))
             (frame (substring text 0 end)))
        (setq text (substring text end))
        (when (= gen llama-swap-state--event-generation)
          (llama-swap-sse--dispatch-frame frame))))
    (setq llama-swap-sse--remainder text)))

(defun llama-swap-sse--dispatch-frame (frame)
  "Parse one complete SSE FRAME and update state."
  ;; Mark as connected on first real data
  (unless (eq llama-swap-state--connection 'connected)
    (llama-swap-state-set-connection 'connected)
    (setq llama-swap-sse--reconnect-delay llama-swap-sse--reconnect-delay-min)
    ;; Fetch version on first connect
    (llama-swap-sse--fetch-version))
  ;; Collect data: lines (may be multiple per frame, per SSE spec)
  (let ((data-lines '()))
    (dolist (line (split-string frame "\n"))
      (cond
       ;; Skip comment/keepalive lines
       ((string-prefix-p ":" line))
       ;; Collect data lines
       ((string-prefix-p "data:" line)
        (push (substring line 5) data-lines))
       ;; Ignore event:/id:/retry: lines
       (t nil)))
    (when data-lines
      (let ((payload (string-join (nreverse data-lines) "\n")))
        (llama-swap-sse--handle-payload payload)))))

(defun llama-swap-sse--handle-payload (payload)
  "Parse the outer SSE envelope in PAYLOAD and dispatch by type."
  (condition-case err
      (let* ((envelope (json-parse-string payload
                                          :object-type 'alist
                                          :array-type 'list))
             (type     (alist-get 'type envelope))
             (raw-data (alist-get 'data envelope)))
        (when (and type raw-data)
          (llama-swap-sse--dispatch-event type raw-data)))
    (error
     (message "llama-swap: SSE parse error: %s" (error-message-string err)))))

(defun llama-swap-sse--dispatch-event (type raw-data)
  "Dispatch parsed SSE event of TYPE with RAW-DATA (inner JSON string)."
  (condition-case err
      (cond
       ((string= type "modelStatus")
        (let ((models (json-parse-string raw-data
                                         :object-type 'alist
                                         :array-type 'list)))
          (llama-swap-state-set-models models)))
       ((string= type "logData")
        (let* ((obj    (json-parse-string raw-data
                                          :object-type 'alist
                                          :array-type 'list))
               (source (alist-get 'source obj))
               (text   (alist-get 'data obj)))
          (when (and source text)
            (llama-swap-state-append-log source text))))
       ((string= type "metrics")
        (let ((entries (json-parse-string raw-data
                                          :object-type 'alist
                                          :array-type 'list)))
          (llama-swap-state-prepend-metrics entries)))
       ((string= type "inflight")
        (let* ((obj   (json-parse-string raw-data
                                         :object-type 'alist
                                         :array-type 'list))
               (total (alist-get 'total obj)))
          (when (integerp total)
            (llama-swap-state-set-in-flight total)))))
    (error
     (message "llama-swap: event dispatch error [%s]: %s"
              type (error-message-string err)))))

;;; --- Process sentinel ---

(defun llama-swap-sse--make-sentinel (gen)
  "Return a process sentinel for generation GEN."
  (lambda (proc _event)
    ;; Ignore sentinels from old generations
    (unless (= gen llama-swap-state--event-generation)
      (cl-return-from nil nil))
    ;; Only handle the final exit
    (unless (process-live-p proc)
      (when (buffer-live-p (process-buffer proc))
        (kill-buffer (process-buffer proc)))
      (setq llama-swap-sse--process nil
            llama-swap-sse--remainder "")
      (unless (eq llama-swap-state--connection 'connected)
        ;; Never successfully connected; count as disconnected
        )
      (llama-swap-state-set-connection 'disconnected)
      (unless llama-swap-sse--intentional-stop
        (llama-swap-sse--schedule-reconnect)))))

;;; --- Reconnect ---

(defun llama-swap-sse--cancel-reconnect-timer ()
  "Cancel any pending reconnect timer."
  (when llama-swap-sse--reconnect-timer
    (cancel-timer llama-swap-sse--reconnect-timer)
    (setq llama-swap-sse--reconnect-timer nil)))

(defun llama-swap-sse--schedule-reconnect ()
  "Schedule a reconnect attempt after exponential backoff."
  (llama-swap-sse--cancel-reconnect-timer)
  (let ((delay llama-swap-sse--reconnect-delay))
    (message "llama-swap: reconnecting in %.0fs…" delay)
    (setq llama-swap-sse--reconnect-timer
          (run-at-time delay nil #'llama-swap-sse--do-reconnect
                       llama-swap-state--event-generation))
    ;; Double delay, cap at max
    (setq llama-swap-sse--reconnect-delay
          (min (* llama-swap-sse--reconnect-delay 2)
               llama-swap-sse--reconnect-delay-max))))

(defun llama-swap-sse--do-reconnect (gen)
  "Reconnect if generation GEN is still current and stop was not intentional."
  (setq llama-swap-sse--reconnect-timer nil)
  (when (and (= gen llama-swap-state--event-generation)
             (not llama-swap-sse--intentional-stop))
    (llama-swap-sse--connect)))

;;; --- Version fetch helper ---

(defun llama-swap-sse--fetch-version ()
  "Fetch /api/version and update state.  Fire and forget."
  (require 'llama-swap-http)
  (llama-swap-http-get "/api/version" nil
                       (lambda (status data _err)
                         (when (and (= status 200) data)
                           (llama-swap-state-set-version data)))))

(provide 'llama-swap-sse)
;;; llama-swap-sse.el ends here
