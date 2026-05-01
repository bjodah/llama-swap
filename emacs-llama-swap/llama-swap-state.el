;;; llama-swap-state.el --- Shared state for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Global state owned by the SSE event stream.  All mutations go through
;; functions which run the appropriate change hooks so open buffers refresh.

;;; Code:

(require 'cl-lib)

;;; --- Connection state ---

(defvar llama-swap-state--connection 'disconnected
  "SSE connection state: `disconnected', `connecting', or `connected'.")

(defvar llama-swap-state--version nil
  "Alist from /api/version, or nil.")

;;; --- Model state ---

(defvar llama-swap-state--models nil
  "List of model alists from the most recent modelStatus SSE event.")

;;; --- Log state ---

(defvar llama-swap-state--proxy-log ""
  "Bounded proxy log string fed by logData SSE events.")

(defvar llama-swap-state--upstream-log ""
  "Bounded upstream log string fed by logData SSE events.")

;;; --- Metrics/activity state ---

(defvar llama-swap-state--metrics nil
  "List of activity alists, newest first, from metrics SSE events.")

(defvar llama-swap-state--in-flight 0
  "Current in-flight request count from inflight SSE events.")

;;; --- Reconnect generation ---

(defvar llama-swap-state--event-generation 0
  "Incremented on each SSE reconnect so stale process filters are discarded.")

;;; --- Change hooks ---

(defvar llama-swap-state-connection-changed-hook nil
  "Hook run when connection state changes.  Called with no args.")

(defvar llama-swap-state-models-changed-hook nil
  "Hook run when model list changes.  Called with no args.")

(defvar llama-swap-state-proxy-log-changed-hook nil
  "Hook run when proxy log is updated.  Called with no args.")

(defvar llama-swap-state-upstream-log-changed-hook nil
  "Hook run when upstream log is updated.  Called with no args.")

(defvar llama-swap-state-metrics-changed-hook nil
  "Hook run when activity metrics are updated.  Called with no args.")

(defvar llama-swap-state-inflight-changed-hook nil
  "Hook run when in-flight count changes.  Called with no args.")

;;; --- Mutation functions ---

(defun llama-swap-state-set-connection (state)
  "Set connection STATE (`disconnected', `connecting', or `connected')."
  (setq llama-swap-state--connection state)
  (run-hooks 'llama-swap-state-connection-changed-hook))

(defun llama-swap-state-set-version (version-alist)
  "Set VERSION-ALIST from /api/version response."
  (setq llama-swap-state--version version-alist))

(defun llama-swap-state-set-models (models-list)
  "Replace model list with MODELS-LIST from modelStatus event."
  (setq llama-swap-state--models models-list)
  (run-hooks 'llama-swap-state-models-changed-hook))

(defun llama-swap-state--append-bounded-log (current new-text max-bytes)
  "Append NEW-TEXT to CURRENT string, trimming head to stay under MAX-BYTES.
Returns the new bounded string."
  (let ((combined (concat current new-text)))
    (if (> (string-bytes combined) max-bytes)
        (let* ((excess (- (string-bytes combined) max-bytes))
               ;; Find a newline boundary after excess bytes to avoid
               ;; cutting mid-line.
               (cut (or (string-search "\n" combined excess) excess)))
          (substring combined cut))
      combined)))

(defun llama-swap-state-append-log (source text)
  "Append TEXT to the log buffer for SOURCE (\"proxy\" or \"upstream\")."
  (let ((max (symbol-value 'llama-swap-log-max-size)))
    (cond
     ((string= source "proxy")
      (setq llama-swap-state--proxy-log
            (llama-swap-state--append-bounded-log
             llama-swap-state--proxy-log text max))
      (run-hooks 'llama-swap-state-proxy-log-changed-hook))
     ((string= source "upstream")
      (setq llama-swap-state--upstream-log
            (llama-swap-state--append-bounded-log
             llama-swap-state--upstream-log text max))
      (run-hooks 'llama-swap-state-upstream-log-changed-hook)))))

(defun llama-swap-state-prepend-metrics (entries)
  "Prepend ENTRIES (list of activity alists) to the metrics list."
  (let ((max (symbol-value 'llama-swap-max-metrics)))
    (setq llama-swap-state--metrics
          (seq-take (append entries llama-swap-state--metrics) max))
    (run-hooks 'llama-swap-state-metrics-changed-hook)))

(defun llama-swap-state-set-metrics (entries)
  "Replace metrics list with ENTRIES."
  (let ((max (symbol-value 'llama-swap-max-metrics)))
    (setq llama-swap-state--metrics (seq-take entries max))
    (run-hooks 'llama-swap-state-metrics-changed-hook)))

(defun llama-swap-state-set-in-flight (n)
  "Set in-flight count to integer N."
  (setq llama-swap-state--in-flight n)
  (run-hooks 'llama-swap-state-inflight-changed-hook))

(defun llama-swap-state-next-generation ()
  "Increment and return the event generation counter."
  (setq llama-swap-state--event-generation
        (1+ llama-swap-state--event-generation)))

(defun llama-swap-state-reset ()
  "Reset all state to initial values (used on disconnect)."
  (setq llama-swap-state--connection  'disconnected
        llama-swap-state--version     nil
        llama-swap-state--models      nil
        llama-swap-state--proxy-log   ""
        llama-swap-state--upstream-log ""
        llama-swap-state--metrics     nil
        llama-swap-state--in-flight   0))

;;; --- Read helpers ---

(defun llama-swap-state-model-by-id (id)
  "Return the model alist with :id equal to ID, or nil."
  (cl-find id llama-swap-state--models
           :key (lambda (m) (alist-get 'id m))
           :test #'equal))

(defun llama-swap-state-connection-string ()
  "Return a human-readable connection status string."
  (pcase llama-swap-state--connection
    ('connected    "Connected")
    ('connecting   "Connecting…")
    ('disconnected "Disconnected")))

(defun llama-swap-state-version-string ()
  "Return version string from /api/version, or empty string."
  (if llama-swap-state--version
      (format "v%s" (or (alist-get 'version llama-swap-state--version) "?"))
    ""))

(provide 'llama-swap-state)
;;; llama-swap-state.el ends here
