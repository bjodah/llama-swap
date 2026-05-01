;;; llama-swap-http.el --- Async HTTP via curl for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Async HTTP layer built on curl.  One-shot requests (GET, POST-JSON).
;; Long-lived SSE connections are handled in llama-swap-sse.el instead.

;;; Code:

(require 'json)
(require 'url-util)
(require 'llama-swap-auth)

(defvar-local llama-swap-http--active-processes nil
  "List of active curl process objects owned by the current buffer.")

(defconst llama-swap-http--status-marker "__LLAMA_SWAP_HTTP_STATUS__:"
  "Sentinel written by --write-out to delimit the response body from the status code.")

(defun llama-swap-http--curl-error-message (exit-code)
  "Return a friendly message for curl EXIT-CODE."
  (pcase exit-code
    (6  "Could not resolve host")
    (7  "Connection refused — is llama-swap running?")
    (22 "HTTP error (check server logs)")
    (28 "Request timed out")
    (35 "SSL/TLS handshake failed")
    (52 "Empty response from server")
    (56 "Network data receive error")
    (_ (format "curl failed with exit code %d" exit-code))))

(defun llama-swap-http--build-url (path &optional params)
  "Build a full URL from PATH and optional query PARAMS alist."
  (let ((base (symbol-value 'llama-swap-base-url)))
    (concat base path
            (when params
              (concat "?"
                      (mapconcat
                       (lambda (pair)
                         (format "%s=%s"
                                 (url-hexify-string (format "%s" (car pair)))
                                 (url-hexify-string (format "%s" (cdr pair)))))
                       params
                       "&"))))))

(defun llama-swap-http--base-curl-args (url method)
  "Return base curl argument list for URL with METHOD."
  (let ((timeout (number-to-string (symbol-value 'llama-swap-request-timeout)))
        (auth-header (llama-swap-auth-header))
        (args (list "--silent" "--show-error"
                    "--max-time" nil
                    "-X" method
                    "-H" "Accept: application/json"
                    "--write-out" (concat "\n" llama-swap-http--status-marker "%{http_code}")
                    url)))
    (setf (nth 3 args) timeout)
    (if auth-header
        (append (list "--silent" "--show-error"
                      "--max-time" timeout
                      "-X" method
                      "-H" auth-header
                      "-H" "Accept: application/json"
                      "--write-out" (concat "\n" llama-swap-http--status-marker "%{http_code}")
                      url))
      (list "--silent" "--show-error"
            "--max-time" timeout
            "-X" method
            "-H" "Accept: application/json"
            "--write-out" (concat "\n" llama-swap-http--status-marker "%{http_code}")
            url))))

(defun llama-swap-http--parse-response (output)
  "Parse curl OUTPUT into (STATUS-CODE . BODY-STRING).
Expects output ending with `llama-swap-http--status-marker' followed by NNN."
  (let ((marker llama-swap-http--status-marker))
    (if (string-match (concat (regexp-quote marker) "\\([0-9]+\\)") output)
        (let ((status (string-to-number (match-string 1 output)))
              (body (substring output 0 (match-beginning 0))))
          (when (string-suffix-p "\n" body)
            (setq body (substring body 0 -1)))
          (cons status body))
      (cons 0 output))))

(defun llama-swap-http--parse-json (body)
  "Parse BODY string as JSON, returning an alist or nil on error."
  (if (or (null body) (string-empty-p body))
      nil
    (condition-case _
        (json-parse-string body :object-type 'alist :array-type 'list)
      (error nil))))

(defun llama-swap-http--make-sentinel (process owner-buf callback)
  "Return a process sentinel for a one-shot curl PROCESS.
OWNER-BUF is cleaned up when the process finishes.
CALLBACK is called with (STATUS-CODE JSON-DATA ERR-STRING)."
  (lambda (proc _event)
    (when (buffer-live-p owner-buf)
      (with-current-buffer owner-buf
        (setq llama-swap-http--active-processes
              (delq proc llama-swap-http--active-processes))))
    (let ((exit-code (process-exit-status proc)))
      (unwind-protect
          (if (not (zerop exit-code))
              (funcall callback 0 nil
                       (llama-swap-http--curl-error-message exit-code))
            (let* ((raw (with-current-buffer (process-buffer proc)
                          (buffer-string)))
                   (parsed (llama-swap-http--parse-response raw))
                   (status (car parsed))
                   (body   (cdr parsed))
                   (json   (llama-swap-http--parse-json body)))
              (funcall callback status json nil)))
        (when (buffer-live-p (process-buffer proc))
          (kill-buffer (process-buffer proc)))))))

(defun llama-swap-http-request (method path params body-string callback)
  "Make an async one-shot HTTP request.
METHOD is \"GET\", \"POST\", etc.
PATH is the API path (e.g., \"/api/models\").
PARAMS is an alist of query parameters, or nil.
BODY-STRING is a pre-encoded string body, or nil.
CALLBACK is called with (STATUS-CODE JSON-DATA ERR-STRING).

Returns the curl process object."
  (let* ((url        (llama-swap-http--build-url path params))
         (timeout    (number-to-string (symbol-value 'llama-swap-request-timeout)))
         (auth-hdr   (llama-swap-auth-header))
         (curl-prog  (symbol-value 'llama-swap-curl-program))
         (owner-buf  (current-buffer))
         (proc-buf   (generate-new-buffer " *llama-swap-curl*"))
         (base-args  (append
                      (list "--silent" "--show-error"
                            "--max-time" timeout
                            "-X" method)
                      (when auth-hdr (list "-H" auth-hdr))
                      (list "-H" "Accept: application/json")
                      (when body-string
                        (list "-H" "Content-Type: application/json"
                              "-d" body-string))
                      (list "--write-out"
                            (concat "\n" llama-swap-http--status-marker "%{http_code}")
                            url)))
         (proc       (apply #'start-process "llama-swap-curl" proc-buf curl-prog base-args)))
    (when (buffer-live-p owner-buf)
      (with-current-buffer owner-buf
        (push proc llama-swap-http--active-processes)))
    (set-process-sentinel proc
                          (llama-swap-http--make-sentinel proc owner-buf callback))
    proc))

(defun llama-swap-http-get (path params callback)
  "Async GET request.  PATH, PARAMS, CALLBACK as in `llama-swap-http-request'."
  (llama-swap-http-request "GET" path params nil callback))

(defun llama-swap-http-post-json (path body-alist callback)
  "Async POST with JSON body.
BODY-ALIST is encoded as a JSON object.
CALLBACK as in `llama-swap-http-request'."
  (let ((body (encode-coding-string
               (json-encode body-alist)
               'utf-8)))
    (llama-swap-http-request "POST" path nil body callback)))

(defun llama-swap-http-cancel-all ()
  "Cancel all active HTTP requests owned by the current buffer."
  (dolist (proc llama-swap-http--active-processes)
    (when (process-live-p proc)
      (let ((buf (process-buffer proc)))
        (delete-process proc)
        (when (buffer-live-p buf)
          (kill-buffer buf)))))
  (setq llama-swap-http--active-processes nil))

(provide 'llama-swap-http)
;;; llama-swap-http.el ends here
