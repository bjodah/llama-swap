;;; llama-swap-activity.el --- Activity table and capture viewer  -*- lexical-binding: t; -*-

;;; Commentary:

;; tabulated-list-mode activity table backed by SSE metrics.
;; Supports capture inspection with base64 decoding.

;;; Code:

(require 'tabulated-list)
(require 'llama-swap-http)
(require 'llama-swap-state)

(defvar llama-swap-activity--buffer-name "*llama-swap-activity*"
  "Name of the activity table buffer.")

(defvar llama-swap-activity-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'llama-swap-activity-refresh)
    (define-key map (kbd "RET") #'llama-swap-activity-open-capture)
    (define-key map (kbd "C")   #'llama-swap-activity-copy-id)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `llama-swap-activity-mode'.")

(define-derived-mode llama-swap-activity-mode tabulated-list-mode "llama-swap-activity"
  "Major mode for the llama-swap activity table."
  (setq tabulated-list-format
        [("ID"       5  (lambda (a b) (< (car a) (car b))))
         ("Time"    20  t)
         ("Model"   25  t)
         ("Path"    25  t)
         ("Status"   6  t)
         ("Prompt"   7  t)
         ("Gen"      7  t)
         ("Spd(t/s)" 9  t)
         ("ms"       6  t)
         ("Cap"      3  nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("ID" . t))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm) (llama-swap-activity-refresh)))
  (add-hook 'llama-swap-state-metrics-changed-hook
            #'llama-swap-activity--refresh-if-visible nil t))

;;; --- Entry conversion ---

(defun llama-swap-activity--format-speed (val)
  "Format tokens-per-second VAL for display."
  (cond
   ((not (numberp val))             "?")
   ((< val 0)                       "?")
   ((> val 1000) (format "%.0fk" (/ val 1000.0)))
   (t            (format "%.0f"  val))))

(defun llama-swap-activity--format-time (timestamp-str)
  "Format TIMESTAMP-STR (ISO-8601) as HH:MM:SS for display."
  (if (or (null timestamp-str) (string-empty-p timestamp-str))
      ""
    (condition-case _
        (let* ((time (date-to-time timestamp-str))
               (decoded (decode-time time)))
          (format "%02d:%02d:%02d"
                  (nth 2 decoded)
                  (nth 1 decoded)
                  (nth 0 decoded)))
      (error timestamp-str))))

(defun llama-swap-activity--entry-to-row (metric)
  "Convert METRIC alist to a tabulated-list row (ID . COLS)."
  (let* ((id      (or (alist-get 'id metric) 0))
         (ts      (llama-swap-activity--format-time
                   (alist-get 'timestamp metric)))
         (model   (or (alist-get 'model metric) ""))
         (path    (or (alist-get 'req_path metric) ""))
         (status  (number-to-string (or (alist-get 'resp_status_code metric) 0)))
         (tokens  (or (alist-get 'tokens metric) '()))
         (prompt  (number-to-string (or (alist-get 'input_tokens tokens) 0)))
         (gen     (number-to-string (or (alist-get 'output_tokens tokens) 0)))
         (speed   (llama-swap-activity--format-speed
                   (alist-get 'tokens_per_second tokens)))
         (dur     (number-to-string (or (alist-get 'duration_ms metric) 0)))
         (cap     (if (eq (alist-get 'has_capture metric) t) "Y" "")))
    (list id
          (vector
           (number-to-string id)
           ts
           model
           path
           status
           prompt
           gen
           speed
           dur
           cap))))

;;; --- Refresh ---

(defun llama-swap-activity-refresh ()
  "Refresh activity table from /api/metrics snapshot."
  (interactive)
  (llama-swap-http-get
   "/api/metrics" nil
   (lambda (status data _err)
     (when (and (= status 200) (listp data))
       (llama-swap-state-set-metrics data)))))

(defun llama-swap-activity--refresh-if-visible ()
  "Refresh the activity buffer if it is currently visible."
  (let ((buf (get-buffer llama-swap-activity--buffer-name)))
    (when (and buf (get-buffer-window buf))
      (with-current-buffer buf
        (setq tabulated-list-entries
              (mapcar #'llama-swap-activity--entry-to-row
                      llama-swap-state--metrics))
        (tabulated-list-print t)))))

;;; --- Capture viewer ---

(defun llama-swap-activity-open-capture ()
  "Open capture viewer for the selected activity entry."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No activity entry selected"))
    (llama-swap-http-get
     (format "/api/captures/%d" id) nil
     (lambda (status data err)
       (cond
        (err    (message "llama-swap: capture fetch error: %s" err))
        ((/= status 200) (message "llama-swap: capture not found (status %d)" status))
        (t      (llama-swap-activity--show-capture data)))))))

(defun llama-swap-activity--decode-base64 (b64-string)
  "Decode B64-STRING from base64, returning UTF-8 text or raw bytes."
  (condition-case _
      (decode-coding-string (base64-decode-string b64-string) 'utf-8)
    (error b64-string)))

(defun llama-swap-activity--show-capture (capture)
  "Display CAPTURE alist in a dedicated buffer."
  (let* ((id       (alist-get 'id capture))
         (buf-name (format "*llama-swap-capture<%d>*" id))
         (buf      (get-buffer-create buf-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (let ((req-headers  (alist-get 'req_headers capture))
            (req-body-raw (alist-get 'req_body capture))
            (resp-headers (alist-get 'resp_headers capture))
            (resp-body-raw (alist-get 'resp_body capture))
            (req-path     (alist-get 'req_path capture)))
        (insert (format "=== Capture #%d: %s ===\n\n" id (or req-path "")))
        ;; Request headers
        (insert "--- Request Headers ---\n")
        (llama-swap-activity--insert-headers req-headers)
        (insert "\n--- Request Body ---\n")
        (if req-body-raw
            (llama-swap-activity--insert-decoded-body req-body-raw)
          (insert "(none)\n"))
        (insert "\n--- Response Headers ---\n")
        (llama-swap-activity--insert-headers resp-headers)
        (insert "\n--- Response Body ---\n")
        (if resp-body-raw
            (llama-swap-activity--insert-decoded-body resp-body-raw)
          (insert "(none)\n")))
      (goto-char (point-min))
      (read-only-mode 1)
      (local-set-key (kbd "q") #'quit-window)
      (local-set-key (kbd "C-c C-w")
                     (lambda ()
                       (interactive)
                       (kill-new (buffer-string))
                       (message "Capture copied to kill ring"))))
    (pop-to-buffer buf)))

(defun llama-swap-activity--insert-headers (headers-alist)
  "Insert HEADERS-ALIST into current buffer as key: value lines."
  (if headers-alist
      (dolist (pair headers-alist)
        (insert (format "  %s: %s\n" (car pair) (cdr pair))))
    (insert "  (none)\n")))

(defun llama-swap-activity--insert-decoded-body (b64-string)
  "Decode B64-STRING and insert as pretty JSON if possible, else raw text."
  (let ((text (llama-swap-activity--decode-base64 b64-string)))
    (condition-case _
        (let* ((parsed (json-parse-string text :object-type 'alist :array-type 'list))
               (pretty (with-temp-buffer
                         (insert (json-encode parsed))
                         (json-pretty-print-buffer)
                         (buffer-string))))
          (insert pretty)
          (insert "\n"))
      (error
       (insert text)
       (insert "\n")))))

(defun llama-swap-activity-copy-id ()
  "Copy the ID of the selected activity entry to the kill ring."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if id
        (progn (kill-new (number-to-string id))
               (message "Copied capture ID %d" id))
      (user-error "No entry selected"))))

;;; --- Entry point ---

(defun llama-swap-activity-open ()
  "Open (or switch to) the activity table buffer."
  (interactive)
  (let ((buf (get-buffer-create llama-swap-activity--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'llama-swap-activity-mode)
        (llama-swap-activity-mode))
      (setq tabulated-list-entries
            (mapcar #'llama-swap-activity--entry-to-row
                    llama-swap-state--metrics))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(provide 'llama-swap-activity)
;;; llama-swap-activity.el ends here
