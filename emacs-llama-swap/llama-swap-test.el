;;; llama-swap-test.el --- ERT tests for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT unit tests for the llama-swap Emacs package.
;; Covers auth, HTTP helpers, SSE parsing, state mutations,
;; activity formatting, and chat stream parsing.
;; No live server required.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add package directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'llama-swap)

;;; ============================================================
;;; Auth tests
;;; ============================================================

(ert-deftest llama-swap-test-auth-explicit-key ()
  "Explicit API key is returned directly."
  (let ((llama-swap-api-key "my-secret-key")
        (llama-swap-auth--cached-key nil))
    (llama-swap-auth-clear-cache)
    (should (equal (llama-swap-auth--try-explicit) "my-secret-key"))))

(ert-deftest llama-swap-test-auth-explicit-nil ()
  "Nil explicit key returns nil."
  (let ((llama-swap-api-key nil))
    (should (null (llama-swap-auth--try-explicit)))))

(ert-deftest llama-swap-test-auth-empty-string-disables-auth ()
  "Empty explicit key disables auth-source lookup and sends no auth header."
  (let ((llama-swap-api-key "")
        (llama-swap-auth--cached-key nil)
        (auth-source-called nil))
    (cl-letf (((symbol-function 'llama-swap-auth--try-auth-source)
               (lambda ()
                 (setq auth-source-called t)
                 "auth-source-key")))
      (should (null (llama-swap-auth-get-key)))
      (should (eq llama-swap-auth--cached-key 'none))
      (should-not auth-source-called)
      (should (null (llama-swap-auth-header))))))

(ert-deftest llama-swap-test-auth-no-key ()
  "get-key returns nil when no key is configured."
  (let ((llama-swap-api-key nil)
        (llama-swap-auth--cached-key nil))
    (cl-letf (((symbol-function 'llama-swap-auth--try-auth-source) (lambda () nil)))
      (should (null (llama-swap-auth-get-key))))))

(ert-deftest llama-swap-test-auth-cache ()
  "Second call returns cached value without re-resolving."
  (let ((llama-swap-auth--cached-key "cached-key"))
    (should (equal (llama-swap-auth-get-key) "cached-key"))))

(ert-deftest llama-swap-test-auth-clear-cache ()
  "Clear cache resets to nil."
  (let ((llama-swap-auth--cached-key "old"))
    (llama-swap-auth-clear-cache)
    (should (null llama-swap-auth--cached-key))))

(ert-deftest llama-swap-test-auth-header-with-key ()
  "auth-header returns Bearer line when key present."
  (let ((llama-swap-auth--cached-key "testkey"))
    (should (equal (llama-swap-auth-header)
                   "Authorization: Bearer testkey"))))

(ert-deftest llama-swap-test-auth-header-no-key ()
  "auth-header returns nil when no key configured."
  (let ((llama-swap-api-key nil)
        (llama-swap-auth--cached-key nil))
    (cl-letf (((symbol-function 'llama-swap-auth--try-auth-source) (lambda () nil)))
      (should (null (llama-swap-auth-header))))))

(ert-deftest llama-swap-test-auth-url-host ()
  "URL host is extracted correctly."
  (let ((llama-swap-base-url "http://myserver:8080"))
    (should (equal (llama-swap-auth--url-host) "myserver"))))

(ert-deftest llama-swap-test-auth-none-sentinel ()
  "Cache uses `none' sentinel to distinguish no-key from uncached."
  (let ((llama-swap-api-key nil)
        (llama-swap-auth--cached-key nil))
    (cl-letf (((symbol-function 'llama-swap-auth--try-auth-source) (lambda () nil)))
      (llama-swap-auth-get-key)
      ;; After get-key with no key found, cache should hold `none'
      (should (eq llama-swap-auth--cached-key 'none)))))

;;; ============================================================
;;; HTTP helper tests
;;; ============================================================

(ert-deftest llama-swap-test-http-build-url-no-params ()
  "URL build with no params."
  (let ((llama-swap-base-url "http://localhost:8080"))
    (should (equal (llama-swap-http--build-url "/api/models")
                   "http://localhost:8080/api/models"))))

(ert-deftest llama-swap-test-http-build-url-with-params ()
  "URL build with query params."
  (let ((llama-swap-base-url "http://localhost:8080"))
    (let ((url (llama-swap-http--build-url "/api/metrics"
                                           '(("limit" . "10")))))
      (should (string-match-p "limit=10" url))
      (should (string-prefix-p "http://localhost:8080/api/metrics?" url)))))

(ert-deftest llama-swap-test-http-build-url-trailing-slash ()
  "Base URL without trailing slash works."
  (let ((llama-swap-base-url "http://host:9999"))
    (should (string-prefix-p "http://host:9999/v1" (llama-swap-http--build-url "/v1")))))

(ert-deftest llama-swap-test-http-parse-response-ok ()
  "Parse curl output with status marker."
  (let* ((out    (concat "{\"ok\":true}\n"
                         "__LLAMA_SWAP_HTTP_STATUS__:200"))
         (parsed (llama-swap-http--parse-response out)))
    (should (= (car parsed) 200))
    (should (equal (cdr parsed) "{\"ok\":true}"))))

(ert-deftest llama-swap-test-http-parse-response-no-marker ()
  "Parse curl output without status marker returns 0 and raw text."
  (let* ((parsed (llama-swap-http--parse-response "raw error text")))
    (should (= (car parsed) 0))
    (should (equal (cdr parsed) "raw error text"))))

(ert-deftest llama-swap-test-http-parse-response-404 ()
  "Parse 404 response."
  (let* ((out    (concat "not found\n"
                         "__LLAMA_SWAP_HTTP_STATUS__:404"))
         (parsed (llama-swap-http--parse-response out)))
    (should (= (car parsed) 404))
    (should (equal (cdr parsed) "not found"))))

(ert-deftest llama-swap-test-http-parse-response-empty-body ()
  "Parse response with empty body."
  (let* ((out    "__LLAMA_SWAP_HTTP_STATUS__:204")
         (parsed (llama-swap-http--parse-response out)))
    (should (= (car parsed) 204))
    (should (string-empty-p (string-trim (cdr parsed))))))

(ert-deftest llama-swap-test-http-parse-json-object ()
  "Parse JSON object."
  (let ((result (llama-swap-http--parse-json "{\"version\":\"42\",\"commit\":\"abc\"}")))
    (should (equal (alist-get 'version result) "42"))
    (should (equal (alist-get 'commit result) "abc"))))

(ert-deftest llama-swap-test-http-parse-json-array ()
  "Parse JSON array."
  (let ((result (llama-swap-http--parse-json "[{\"id\":\"m1\"},{\"id\":\"m2\"}]")))
    (should (listp result))
    (should (= (length result) 2))
    (should (equal (alist-get 'id (car result)) "m1"))))

(ert-deftest llama-swap-test-http-parse-json-empty ()
  "Empty body returns nil."
  (should (null (llama-swap-http--parse-json "")))
  (should (null (llama-swap-http--parse-json nil))))

(ert-deftest llama-swap-test-http-parse-json-invalid ()
  "Invalid JSON returns nil without error."
  (should (null (llama-swap-http--parse-json "not json {broken"))))

(ert-deftest llama-swap-test-http-curl-error-messages ()
  "curl exit code messages are descriptive."
  (should (string-match-p "Connection refused" (llama-swap-http--curl-error-message 7)))
  (should (string-match-p "timed out" (llama-swap-http--curl-error-message 28)))
  (should (string-match-p "exit code 99" (llama-swap-http--curl-error-message 99))))

(ert-deftest llama-swap-test-http-curl-error-known-codes ()
  "All documented curl codes are handled."
  (should (not (string-match-p "exit code 6"  (llama-swap-http--curl-error-message 6))))
  (should (not (string-match-p "exit code 22" (llama-swap-http--curl-error-message 22))))
  (should (not (string-match-p "exit code 35" (llama-swap-http--curl-error-message 35)))))

(ert-deftest llama-swap-test-http-request-returns-process ()
  "http-request returns a live process."
  (let ((llama-swap-base-url "http://localhost:19999")
        (llama-swap-curl-program "curl")
        (llama-swap-request-timeout 5)
        (llama-swap-api-key nil)
        (llama-swap-auth--cached-key 'none))
    (let ((proc (llama-swap-http-request "GET" "/test" nil nil #'ignore)))
      (unwind-protect
          (should (processp proc))
        (when (process-live-p proc) (delete-process proc))))))

(ert-deftest llama-swap-test-http-cancel-all-buffer-local ()
  "cancel-all only kills processes registered in the current buffer."
  (let (p1 p2)
    (unwind-protect
        (with-temp-buffer
          ;; Start a process in outer buffer
          (setq p1 (start-process "ls-test-p1" nil "sleep" "30"))
          (push p1 llama-swap-http--active-processes)
          (with-temp-buffer
            ;; Start a process in inner buffer
            (setq p2 (start-process "ls-test-p2" nil "sleep" "30"))
            (push p2 llama-swap-http--active-processes)
            ;; Cancel only this buffer's processes
            (llama-swap-http-cancel-all)
            (should-not (process-live-p p2)))
          ;; Outer buffer process should be unaffected
          (should (process-live-p p1)))
      (dolist (p (list p1 p2))
        (when (and p (process-live-p p)) (delete-process p))))))

;;; ============================================================
;;; State tests
;;; ============================================================

(ert-deftest llama-swap-test-state-set-connection ()
  "set-connection updates state and runs hook."
  (let ((llama-swap-state--connection 'disconnected)
        (hook-ran nil)
        (llama-swap-state-connection-changed-hook nil))
    (add-hook 'llama-swap-state-connection-changed-hook
              (lambda () (setq hook-ran t)))
    (llama-swap-state-set-connection 'connected)
    (should (eq llama-swap-state--connection 'connected))
    (should hook-ran)))

(ert-deftest llama-swap-test-state-set-models ()
  "set-models updates list and runs hook."
  (let ((llama-swap-state--models nil)
        (hook-ran nil)
        (llama-swap-state-models-changed-hook nil))
    (add-hook 'llama-swap-state-models-changed-hook
              (lambda () (setq hook-ran t)))
    (llama-swap-state-set-models '(((id . "m1") (state . "ready"))))
    (should (= (length llama-swap-state--models) 1))
    (should hook-ran)))

(ert-deftest llama-swap-test-state-append-log-proxy ()
  "append-log appends to proxy log and runs hook."
  (let ((llama-swap-state--proxy-log "old")
        (hook-ran nil)
        (llama-swap-state-proxy-log-changed-hook nil)
        (llama-swap-log-max-size (* 100 1024)))
    (add-hook 'llama-swap-state-proxy-log-changed-hook
              (lambda () (setq hook-ran t)))
    (llama-swap-state-append-log "proxy" " new")
    (should (equal llama-swap-state--proxy-log "old new"))
    (should hook-ran)))

(ert-deftest llama-swap-test-state-append-log-upstream ()
  "append-log appends to upstream log."
  (let ((llama-swap-state--upstream-log "")
        (llama-swap-state-upstream-log-changed-hook nil)
        (llama-swap-log-max-size (* 100 1024)))
    (llama-swap-state-append-log "upstream" "line1\n")
    (should (equal llama-swap-state--upstream-log "line1\n"))))

(ert-deftest llama-swap-test-state-log-bounding ()
  "Log is trimmed when it exceeds max-bytes."
  (let ((llama-swap-state--proxy-log "")
        (llama-swap-log-max-size 20)
        (llama-swap-state-proxy-log-changed-hook nil))
    (llama-swap-state-append-log "proxy" "line-one\nline-two\nline-three\n")
    (let ((result llama-swap-state--proxy-log))
      ;; Result should be <= max-bytes
      (should (<= (string-bytes result) 20)))))

(ert-deftest llama-swap-test-state-bounded-log-helper ()
  "Bounded log helper trims at newline boundary."
  (let ((result (llama-swap-state--append-bounded-log
                 "" "aaaa\nbbbb\ncccc\n" 10)))
    (should (<= (string-bytes result) 10))))

(ert-deftest llama-swap-test-state-bounded-log-no-trim-under-limit ()
  "Bounded log does not trim when under limit."
  (let ((result (llama-swap-state--append-bounded-log
                 "" "short\n" 1000)))
    (should (equal result "short\n"))))

(ert-deftest llama-swap-test-state-prepend-metrics ()
  "prepend-metrics adds entries at front of list."
  (let ((llama-swap-state--metrics '(((id . 0))))
        (llama-swap-max-metrics 100)
        (llama-swap-state-metrics-changed-hook nil))
    (llama-swap-state-prepend-metrics '(((id . 1))))
    (should (= (alist-get 'id (car llama-swap-state--metrics)) 1))))

(ert-deftest llama-swap-test-state-max-metrics ()
  "Metrics list is capped at llama-swap-max-metrics."
  (let ((llama-swap-state--metrics nil)
        (llama-swap-max-metrics 3)
        (llama-swap-state-metrics-changed-hook nil))
    (llama-swap-state-set-metrics
     '(((id . 0)) ((id . 1)) ((id . 2)) ((id . 3))))
    (should (= (length llama-swap-state--metrics) 3))))

(ert-deftest llama-swap-test-state-set-in-flight ()
  "set-in-flight updates counter and runs hook."
  (let ((llama-swap-state--in-flight 0)
        (hook-ran nil)
        (llama-swap-state-inflight-changed-hook nil))
    (add-hook 'llama-swap-state-inflight-changed-hook
              (lambda () (setq hook-ran t)))
    (llama-swap-state-set-in-flight 5)
    (should (= llama-swap-state--in-flight 5))
    (should hook-ran)))

(ert-deftest llama-swap-test-state-next-generation ()
  "next-generation increments and returns the counter."
  (let ((llama-swap-state--event-generation 0))
    (should (= (llama-swap-state-next-generation) 1))
    (should (= (llama-swap-state-next-generation) 2))
    (should (= llama-swap-state--event-generation 2))))

(ert-deftest llama-swap-test-state-model-by-id ()
  "model-by-id finds the right model."
  (let ((llama-swap-state--models
         '(((id . "m1") (state . "ready"))
           ((id . "m2") (state . "stopped")))))
    (let ((m (llama-swap-state-model-by-id "m2")))
      (should m)
      (should (equal (alist-get 'state m) "stopped")))
    (should (null (llama-swap-state-model-by-id "missing")))))

(ert-deftest llama-swap-test-state-connection-string ()
  "connection-string returns human-readable values."
  (let ((llama-swap-state--connection 'connected))
    (should (equal (llama-swap-state-connection-string) "Connected")))
  (let ((llama-swap-state--connection 'disconnected))
    (should (equal (llama-swap-state-connection-string) "Disconnected")))
  (let ((llama-swap-state--connection 'connecting))
    (should (string-match-p "Connecting" (llama-swap-state-connection-string)))))

(ert-deftest llama-swap-test-state-version-string ()
  "version-string returns formatted version."
  (let ((llama-swap-state--version '((version . "196") (commit . "abc"))))
    (should (string-match-p "196" (llama-swap-state-version-string))))
  (let ((llama-swap-state--version nil))
    (should (string-empty-p (llama-swap-state-version-string)))))

(ert-deftest llama-swap-test-state-reset ()
  "reset clears all state."
  (let ((llama-swap-state--connection  'connected)
        (llama-swap-state--models      '(((id . "m1"))))
        (llama-swap-state--in-flight   7)
        (llama-swap-state--proxy-log   "some log")
        (llama-swap-state-connection-changed-hook nil))
    (llama-swap-state-reset)
    (should (eq llama-swap-state--connection  'disconnected))
    (should (null llama-swap-state--models))
    (should (= llama-swap-state--in-flight 0))
    (should (string-empty-p llama-swap-state--proxy-log))))

;;; ============================================================
;;; SSE parser tests
;;; ============================================================

;; Use `connected' as initial state to avoid fetch-version triggering
;; on every first dispatch call, which would spawn unwanted curl processes.
(defmacro llama-swap-test--with-sse-state (&rest body)
  "Run BODY with clean SSE-testable state.
Starts with connection already `connected' to skip the version-fetch block."
  `(let ((llama-swap-state--connection   'connected)
         (llama-swap-state--models       nil)
         (llama-swap-state--proxy-log    "")
         (llama-swap-state--upstream-log "")
         (llama-swap-state--metrics      nil)
         (llama-swap-state--in-flight    0)
         (llama-swap-state--event-generation 0)
         (llama-swap-state-models-changed-hook      nil)
         (llama-swap-state-proxy-log-changed-hook   nil)
         (llama-swap-state-upstream-log-changed-hook nil)
         (llama-swap-state-metrics-changed-hook     nil)
         (llama-swap-state-inflight-changed-hook    nil)
         (llama-swap-state-connection-changed-hook  nil)
         (llama-swap-log-max-size  (* 100 1024))
         (llama-swap-max-metrics 100)
         (llama-swap-sse--remainder ""))
     ,@body))

(defun llama-swap-test--feed-sse (text)
  "Feed TEXT to the SSE remainder processor as if from a process filter."
  (let ((gen llama-swap-state--event-generation))
    (setq llama-swap-sse--remainder (concat llama-swap-sse--remainder text))
    (llama-swap-sse--process-remainder gen)))

(ert-deftest llama-swap-test-sse-parse-model-status ()
  "SSE modelStatus event updates model list."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"modelStatus\","
            "\"data\":\"[{\\\"id\\\":\\\"m1\\\","
            "\\\"state\\\":\\\"ready\\\"}]\"}\n\n"))
   (should (= (length llama-swap-state--models) 1))
   (should (equal (alist-get 'id (car llama-swap-state--models)) "m1"))
   (should (equal (alist-get 'state (car llama-swap-state--models)) "ready"))))

(ert-deftest llama-swap-test-sse-parse-model-status-multiple ()
  "SSE modelStatus with multiple models updates full list."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"modelStatus\","
            "\"data\":\"[{\\\"id\\\":\\\"m1\\\"},{\\\"id\\\":\\\"m2\\\"}]\"}\n\n"))
   (should (= (length llama-swap-state--models) 2))))

(ert-deftest llama-swap-test-sse-parse-log-proxy ()
  "SSE logData event appends to proxy log."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"logData\","
            "\"data\":\"{\\\"source\\\":\\\"proxy\\\","
            "\\\"data\\\":\\\"hello\\\\n\\\"}\"}\n\n"))
   (should (string-match-p "hello" llama-swap-state--proxy-log))))

(ert-deftest llama-swap-test-sse-parse-log-upstream ()
  "SSE logData event appends to upstream log."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"logData\","
            "\"data\":\"{\\\"source\\\":\\\"upstream\\\","
            "\\\"data\\\":\\\"upstream-line\\\\n\\\"}\"}\n\n"))
   (should (string-match-p "upstream-line" llama-swap-state--upstream-log))))

(ert-deftest llama-swap-test-sse-parse-inflight ()
  "SSE inflight event updates in-flight count."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"inflight\","
            "\"data\":\"{\\\"total\\\":3}\"}\n\n"))
   (should (= llama-swap-state--in-flight 3))))

(ert-deftest llama-swap-test-sse-parse-metrics ()
  "SSE metrics event prepends activity entries."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"metrics\","
            "\"data\":\"[{\\\"id\\\":42,"
            "\\\"model\\\":\\\"test-model\\\"}]\"}\n\n"))
   (should (= (length llama-swap-state--metrics) 1))
   (should (= (alist-get 'id (car llama-swap-state--metrics)) 42))))

(ert-deftest llama-swap-test-sse-split-frame ()
  "SSE data arriving in two partial writes is assembled correctly."
  (llama-swap-test--with-sse-state
   ;; First partial write — incomplete frame
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"inflight\","))
   ;; State not yet updated
   (should (= llama-swap-state--in-flight 0))
   ;; Second partial write completes the frame
   (llama-swap-test--feed-sse
    "\"data\":\"{\\\"total\\\":7}\"}\n\n")
   (should (= llama-swap-state--in-flight 7))))

(ert-deftest llama-swap-test-sse-multiple-frames ()
  "Multiple SSE frames in one delivery are all processed."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\n"
            "data:{\"type\":\"inflight\",\"data\":\"{\\\"total\\\":2}\"}\n\n"
            "event:message\n"
            "data:{\"type\":\"inflight\",\"data\":\"{\\\"total\\\":4}\"}\n\n"))
   (should (= llama-swap-state--in-flight 4))))

(ert-deftest llama-swap-test-sse-crlf-normalization ()
  "CRLF line endings in SSE frames are normalised to LF."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse
    (concat "event:message\r\n"
            "data:{\"type\":\"inflight\",\"data\":\"{\\\"total\\\":9}\"}\r\n\r\n"))
   (should (= llama-swap-state--in-flight 9))))

(ert-deftest llama-swap-test-sse-malformed-json-ignored ()
  "Malformed inner JSON does not crash the parser."
  (llama-swap-test--with-sse-state
   ;; Should not signal an error
   (should
    (condition-case _
        (progn
          (llama-swap-test--feed-sse
           "event:message\ndata:{\"type\":\"inflight\",\"data\":\"not-json\"}\n\n")
          t)
      (error nil)))
   ;; In-flight unchanged
   (should (= llama-swap-state--in-flight 0))))

(ert-deftest llama-swap-test-sse-keepalive-ignored ()
  "SSE keepalive comment lines don't update models or metrics."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse ": keepalive\n\n")
   ;; No meaningful state updated
   (should (null llama-swap-state--models))
   (should (null llama-swap-state--metrics))
   (should (= llama-swap-state--in-flight 0))))

(ert-deftest llama-swap-test-sse-stale-generation-ignored ()
  "SSE frames from old generations are discarded."
  (llama-swap-test--with-sse-state
   ;; Advance generation to 1 so generation 0 is now stale
   (llama-swap-state-next-generation)
   ;; Feed a complete frame using old generation 0
   (let ((old-gen 0))
     (setq llama-swap-sse--remainder
           (concat "event:message\n"
                   "data:{\"type\":\"inflight\",\"data\":\"{\\\"total\\\":99}\"}\n\n"))
     (llama-swap-sse--process-remainder old-gen))
   ;; In-flight should be unchanged (frame from old gen was discarded)
   (should (= llama-swap-state--in-flight 0))))

(ert-deftest llama-swap-test-sse-remainder-preserves-partial ()
  "Partial frame is preserved in remainder for next write."
  (llama-swap-test--with-sse-state
   (llama-swap-test--feed-sse "event:message\ndata:partial-line")
   ;; Remainder should still contain the incomplete frame
   (should (string-match-p "partial-line" llama-swap-sse--remainder))))

(ert-deftest llama-swap-test-sse-filter-stale-generation-no-error ()
  "Filter for a stale generation deletes the proc and returns cleanly."
  (let ((llama-swap-state--event-generation 1)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (start-process "llama-swap-test-sse-stale" nil
                                    "sleep" "10"))
          (let ((filter (llama-swap-sse--make-filter 0)))
            (funcall filter proc "junk\n\n"))
          (should-not (process-live-p proc)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))))

(ert-deftest llama-swap-test-sse-sentinel-stale-generation-no-error ()
  "Sentinel for a stale generation returns cleanly without signalling."
  (let ((llama-swap-state--event-generation 1)
        (llama-swap-sse--process 'placeholder)
        (proc nil))
    (unwind-protect
        (progn
          (setq proc (start-process "llama-swap-test-sse-stale-sent"
                                    nil "true"))
          (while (process-live-p proc)
            (accept-process-output proc 0.1))
          (let ((sentinel (llama-swap-sse--make-sentinel 0)))
            (funcall sentinel proc "finished\n"))
          ;; Stale-generation sentinel must not touch global SSE state.
          (should (eq llama-swap-sse--process 'placeholder)))
      (when (and proc (process-live-p proc))
        (delete-process proc)))))

(ert-deftest llama-swap-test-sse-start-while-running-no-error ()
  "Calling start while a process is alive is a no-op, not a Lisp error."
  (let ((proc nil)
        (llama-swap-sse--process nil)
        (llama-swap-sse--intentional-stop nil))
    (unwind-protect
        (progn
          (setq proc (start-process "llama-swap-test-sse-running"
                                    nil "sleep" "10"))
          (setq llama-swap-sse--process proc)
          ;; Must not signal; must not call --connect.
          (cl-letf (((symbol-function 'llama-swap-sse--connect)
                     (lambda () (error "should not reconnect"))))
            (llama-swap-sse-start)))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (setq llama-swap-sse--process nil))))

;;; ============================================================
;;; Models dashboard tests
;;; ============================================================

(ert-deftest llama-swap-test-models-entry-to-row-basic ()
  "entry-to-row converts model alist to table row."
  (with-temp-buffer
    (llama-swap-models-mode)
    (let ((row (llama-swap-models--entry-to-row
                '((id . "mymodel")
                  (name . "My Model")
                  (state . "ready")
                  (peerID . "")
                  (aliases . ("alias1" "alias2"))
                  (description . "A test model")))))
      (should (equal (car row) "mymodel"))
      (let ((cols (cadr row)))
        (should (string-match-p "ready" (aref cols 0)))
        (should (equal (aref cols 1) "My Model"))
        (should (equal (aref cols 2) ""))
        (should (string-match-p "alias1" (aref cols 3)))
        (should (equal (aref cols 4) "A test model"))))))

(ert-deftest llama-swap-test-models-entry-uses-id-when-no-name ()
  "entry-to-row uses model ID when name is empty."
  (with-temp-buffer
    (llama-swap-models-mode)
    (let ((row (llama-swap-models--entry-to-row
                '((id . "myid") (name . "") (state . "stopped")))))
      (should (equal (aref (cadr row) 1) "myid")))))

(ert-deftest llama-swap-test-models-entry-show-id-flag ()
  "show-id flag forces ID column instead of display name."
  (with-temp-buffer
    (llama-swap-models-mode)
    (setq llama-swap-models--show-id t)
    (let ((row (llama-swap-models--entry-to-row
                '((id . "myid") (name . "Pretty Name") (state . "ready")))))
      (should (equal (aref (cadr row) 1) "myid")))))

(ert-deftest llama-swap-test-models-state-face ()
  "State faces map correctly."
  (should (eq (llama-swap-models--state-face "ready")    'success))
  (should (eq (llama-swap-models--state-face "stopped")  'shadow))
  (should (eq (llama-swap-models--state-face "starting") 'warning))
  (should (eq (llama-swap-models--state-face "stopping") 'warning))
  (should (eq (llama-swap-models--state-face "shutdown") 'error))
  (should (eq (llama-swap-models--state-face "unknown")  'default)))

(ert-deftest llama-swap-test-models-visible-models-filter-unlisted ()
  "Unlisted models are hidden by default."
  (with-temp-buffer
    (llama-swap-models-mode)
    (let ((llama-swap-state--models
           '(((id . "listed") (unlisted . :false))
             ((id . "hidden") (unlisted . t)))))
      (setq llama-swap-models--show-unlisted nil)
      (let ((visible (llama-swap-models--visible-models)))
        (should (= (length visible) 1))
        (should (equal (alist-get 'id (car visible)) "listed"))))))

(ert-deftest llama-swap-test-models-visible-models-show-unlisted ()
  "show-unlisted flag makes unlisted models visible."
  (with-temp-buffer
    (llama-swap-models-mode)
    (let ((llama-swap-state--models
           '(((id . "listed") (unlisted . :false))
             ((id . "hidden") (unlisted . t)))))
      (setq llama-swap-models--show-unlisted t)
      (should (= (length (llama-swap-models--visible-models)) 2)))))

(ert-deftest llama-swap-test-models-visible-filter-peers ()
  "Peer models are hidden when show-peers is nil."
  (with-temp-buffer
    (llama-swap-models-mode)
    (let ((llama-swap-state--models
           '(((id . "local") (peerID . ""))
             ((id . "peer")  (peerID . "remote-peer")))))
      (setq llama-swap-models--show-peers nil)
      (let ((visible (llama-swap-models--visible-models)))
        (should (= (length visible) 1))
        (should (equal (alist-get 'id (car visible)) "local"))))))

(ert-deftest llama-swap-test-models-visible-show-peers ()
  "show-peers flag makes peer models visible."
  (with-temp-buffer
    (llama-swap-models-mode)
    (let ((llama-swap-state--models
           '(((id . "local") (peerID . ""))
             ((id . "peer")  (peerID . "remote-peer")))))
      (setq llama-swap-models--show-peers t)
      (should (= (length (llama-swap-models--visible-models)) 2)))))

(ert-deftest llama-swap-test-models-global-hook-refreshes-from-other-buffer ()
  "SSE-triggered model hook refreshes the dashboard outside its buffer."
  (let ((llama-swap-state-models-changed-hook nil)
        (llama-swap-state-connection-changed-hook nil)
        (llama-swap-state-inflight-changed-hook nil)
        (llama-swap-state--models '(((id . "hook-model")
                                      (name . "Hook Model")
                                      (state . "ready"))))
        (buf (get-buffer-create llama-swap-models--buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (_buffer &optional _frame) t)))
          (with-current-buffer buf
            (llama-swap-models-mode)
            (setq tabulated-list-entries nil))
          (with-temp-buffer
            (run-hooks 'llama-swap-state-models-changed-hook))
          (with-current-buffer buf
            (should (= (length tabulated-list-entries) 1))
            (should (equal (caar tabulated-list-entries) "hook-model"))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; ============================================================
;;; Log tests
;;; ============================================================

(ert-deftest llama-swap-test-logs-global-hook-refreshes-from-other-buffer ()
  "SSE-triggered log hook refreshes visible log buffers outside their buffer."
  (let ((llama-swap-state-proxy-log-changed-hook nil)
        (llama-swap-state--proxy-log "")
        (buf nil))
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (_buffer &optional _frame) t))
                  ((symbol-function 'switch-to-buffer-other-window)
                   (lambda (buffer-or-name &optional _norecord)
                     (setq buf (get-buffer buffer-or-name)))))
          (llama-swap-logs-open-proxy)
          (with-temp-buffer
            (llama-swap-state-append-log "proxy" "hello from hook\n"))
          (with-current-buffer buf
            (should (string-match-p "hello from hook" (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;; ============================================================
;;; Activity tests
;;; ============================================================

(ert-deftest llama-swap-test-activity-entry-to-row ()
  "activity entry-to-row converts metric alist to table row."
  (let ((metric '((id . 5)
                  (timestamp . "2024-01-01T12:34:56Z")
                  (model . "test-model")
                  (req_path . "/v1/chat/completions")
                  (resp_status_code . 200)
                  (tokens . ((input_tokens . 100)
                              (output_tokens . 50)
                              (tokens_per_second . 120.5)))
                  (duration_ms . 450)
                  (has_capture . t))))
    (let ((row (llama-swap-activity--entry-to-row metric)))
      (should (= (car row) 5))
      (let ((cols (cadr row)))
        (should (equal (aref cols 0) "5"))
        (should (equal (aref cols 2) "test-model"))
        (should (equal (aref cols 3) "/v1/chat/completions"))
        (should (equal (aref cols 4) "200"))
        (should (equal (aref cols 5) "100"))
        (should (equal (aref cols 6) "50"))
        (should (string-match-p "120" (aref cols 7)))
        (should (equal (aref cols 8) "450"))
        (should (equal (aref cols 9) "Y"))))))

(ert-deftest llama-swap-test-activity-entry-no-capture ()
  "Activity row shows empty capture indicator when has_capture is false."
  (let ((metric '((id . 1)
                  (has_capture . :false)
                  (tokens . ()))))
    (let ((row (llama-swap-activity--entry-to-row metric)))
      (should (equal (aref (cadr row) 9) "")))))

(ert-deftest llama-swap-test-activity-entry-missing-fields ()
  "Activity row handles missing optional fields gracefully."
  (let ((metric '((id . 2))))
    ;; Should not error
    (should (condition-case _
                (progn (llama-swap-activity--entry-to-row metric) t)
              (error nil)))))

(ert-deftest llama-swap-test-activity-format-speed-normal ()
  "Speed formatting for normal values."
  (should (string-match-p "120" (llama-swap-activity--format-speed 120.0))))

(ert-deftest llama-swap-test-activity-format-speed-negative ()
  "Speed formatting returns ? for negative values."
  (should (equal (llama-swap-activity--format-speed -1.0) "?")))

(ert-deftest llama-swap-test-activity-format-speed-large ()
  "Speed formatting uses k suffix for values > 1000."
  (should (string-match-p "k" (llama-swap-activity--format-speed 1500.0))))

(ert-deftest llama-swap-test-activity-format-speed-non-number ()
  "Speed formatting returns ? for non-number."
  (should (equal (llama-swap-activity--format-speed nil) "?")))

(ert-deftest llama-swap-test-activity-base64-decode ()
  "base64 decode produces correct output string."
  (let ((b64 (base64-encode-string "hello world" t)))
    (should (equal (llama-swap-activity--decode-base64 b64) "hello world"))))

;;; ============================================================
;;; Chat stream parser tests
;;; ============================================================

(ert-deftest llama-swap-test-chat-parse-content-delta ()
  "Chat stream content delta is extracted and appended."
  (let ((content '()))
    (cl-letf (((symbol-function 'llama-swap-chat--append-text)
               (lambda (t) (push t content))))
      (with-temp-buffer
        (llama-swap-chat-mode)
        (llama-swap-chat--process-frame
         (concat "data:{\"choices\":[{\"delta\":"
                 "{\"content\":\"hello \"},\"finish_reason\":null}]}\n\n"))))
    (should (equal (car content) "hello "))))

(ert-deftest llama-swap-test-chat-parse-done-ignored ()
  "[DONE] frame does not error."
  (should
   (condition-case _
       (progn
         (with-temp-buffer
           (llama-swap-chat-mode)
           (llama-swap-chat--process-frame "data: [DONE]\n\n"))
         t)
     (error nil))))

(ert-deftest llama-swap-test-chat-parse-empty-content-ignored ()
  "Chunk with empty content string is not appended."
  (let ((content '()))
    (cl-letf (((symbol-function 'llama-swap-chat--append-text)
               (lambda (t) (push t content))))
      (with-temp-buffer
        (llama-swap-chat-mode)
        (llama-swap-chat--process-frame
         (concat "data:{\"choices\":[{\"delta\":"
                 "{\"content\":\"\"},\"finish_reason\":null}]}\n\n"))))
    (should (null content))))

(ert-deftest llama-swap-test-chat-parse-null-content-ignored ()
  "Chunk with null content is not appended."
  (let ((content '()))
    (cl-letf (((symbol-function 'llama-swap-chat--append-text)
               (lambda (t) (push t content))))
      (with-temp-buffer
        (llama-swap-chat-mode)
        (llama-swap-chat--process-frame
         (concat "data:{\"choices\":[{\"delta\":"
                 "{\"content\":null},\"finish_reason\":\"stop\"}]}\n\n"))))
    (should (null content))))

(ert-deftest llama-swap-test-chat-parse-malformed-chunk-no-error ()
  "Malformed JSON chunk does not error."
  (should
   (condition-case _
       (progn
         (with-temp-buffer
           (llama-swap-chat-mode)
           (llama-swap-chat--process-frame
            "data:{\"choices\":[{broken\n\n"))
         t)
     (error nil))))

(ert-deftest llama-swap-test-chat-parse-split-across-writes ()
  "Content accumulated across two partial SSE writes."
  (let ((content '()))
    (cl-letf (((symbol-function 'llama-swap-chat--append-text)
               (lambda (t) (push t content))))
      (with-temp-buffer
        (llama-swap-chat-mode)
        (setq llama-swap-chat--sse-remainder "")
        (setq llama-swap-chat--response-marker nil)
        (cl-flet
            ((process-text ()
               (let ((text llama-swap-chat--sse-remainder))
                 (setq text (replace-regexp-in-string "\r\n" "\n" text))
                 (while (string-match "\n\n" text)
                   (let* ((end (match-end 0))
                          (frame (substring text 0 end)))
                     (setq text (substring text end))
                     (llama-swap-chat--process-frame frame)))
                 (setq llama-swap-chat--sse-remainder text))))
          ;; First partial write
          (setq llama-swap-chat--sse-remainder
                "data:{\"choices\":[{\"delta\":{\"content\":\"hel")
          (process-text)
          (should (null content))
          ;; Second partial write completes the frame
          (setq llama-swap-chat--sse-remainder
                (concat llama-swap-chat--sse-remainder
                        "lo\"},\"finish_reason\":null}]}\n\n"))
          (process-text)
          (should (equal (car content) "hello")))))  ))

(ert-deftest llama-swap-test-chat-parse-reasoning-content ()
  "reasoning_content delta is wrapped in <think> tags."
  (let ((content '()))
    (cl-letf (((symbol-function 'llama-swap-chat--append-text)
               (lambda (t) (push t content))))
      (with-temp-buffer
        (llama-swap-chat-mode)
        (llama-swap-chat--process-frame
         (concat "data:{\"choices\":[{\"delta\":"
                 "{\"reasoning_content\":\"thinking...\","
                 "\"content\":\"\"},\"finish_reason\":null}]}\n\n"))))
    (should content)
    (should (string-match-p "thinking" (car content)))
    (should (string-match-p "<think>" (car content)))))

(ert-deftest llama-swap-test-chat-build-request-body ()
  "Chat request body includes model, messages, and stream flag."
  (let ((llama-swap-chat-temperature 0.7)
        (llama-swap-chat-max-tokens nil)
        (llama-swap-chat-system-prompt nil))
    (with-temp-buffer
      (llama-swap-chat-mode)
      (setq llama-swap-chat--model "test-model"
            llama-swap-chat--messages '(("user" . "hello")))
      ;; Body uses string keys since json-encode will serialize them
      (let ((body (llama-swap-chat--build-request-body)))
        (should (equal (cdr (assoc "model" body)) "test-model"))
        (should (eq (cdr (assoc "stream" body)) t))
        (should (= (cdr (assoc "temperature" body)) 0.7))
        (should (listp (cdr (assoc "messages" body))))))))

(ert-deftest llama-swap-test-chat-build-request-body-with-system ()
  "System prompt is prepended when set."
  (let ((llama-swap-chat-system-prompt "You are helpful.")
        (llama-swap-chat-max-tokens nil))
    (with-temp-buffer
      (llama-swap-chat-mode)
      (setq llama-swap-chat--model "m"
            llama-swap-chat--messages '(("user" . "hi")))
      (let* ((body (llama-swap-chat--build-request-body))
             (msgs (cdr (assoc "messages" body)))
             (first (car msgs)))
        (should (equal (cdr (assoc "role" first)) "system"))
        (should (equal (cdr (assoc "content" first)) "You are helpful."))))))

(ert-deftest llama-swap-test-chat-build-request-body-max-tokens ()
  "max_tokens field is included when set."
  (let ((llama-swap-chat-system-prompt nil)
        (llama-swap-chat-max-tokens 512))
    (with-temp-buffer
      (llama-swap-chat-mode)
      (setq llama-swap-chat--model "m"
            llama-swap-chat--messages '(("user" . "hi")))
      (let ((body (llama-swap-chat--build-request-body)))
        (should (= (cdr (assoc "max_tokens" body)) 512))))))

(ert-deftest llama-swap-test-chat-user-input ()
  "User input is extracted after the last prompt separator."
  (with-temp-buffer
    (llama-swap-chat-mode)
    (insert "some prior content")
    (insert llama-swap-chat--prompt-separator)
    (insert "  my input  ")
    (should (equal (llama-swap-chat--get-user-input) "my input"))))

(ert-deftest llama-swap-test-chat-user-input-empty ()
  "Empty input area returns empty string."
  (with-temp-buffer
    (llama-swap-chat-mode)
    (insert llama-swap-chat--prompt-separator)
    (should (string-empty-p (llama-swap-chat--get-user-input)))))

(ert-deftest llama-swap-test-chat-delete-user-input ()
  "delete-user-input removes text after separator but keeps separator."
  (with-temp-buffer
    (llama-swap-chat-mode)
    (insert "history")
    (insert llama-swap-chat--prompt-separator)
    (insert "some text to delete")
    (llama-swap-chat--delete-user-input)
    (should (string-suffix-p llama-swap-chat--prompt-separator (buffer-string)))))

(ert-deftest llama-swap-test-chat-render-message ()
  "render-message inserts label and content into buffer."
  (with-temp-buffer
    (llama-swap-chat-mode)
    (llama-swap-chat--render-message "user" "hello world")
    (should (string-match-p "You" (buffer-string)))
    (should (string-match-p "hello world" (buffer-string)))))

(ert-deftest llama-swap-test-chat-render-assistant-message ()
  "render-message for assistant uses 'Assistant' label."
  (with-temp-buffer
    (llama-swap-chat-mode)
    (llama-swap-chat--render-message "assistant" "my response")
    (should (string-match-p "Assistant" (buffer-string)))))

(ert-deftest llama-swap-test-chat-sentinel-appends-curl-error ()
  "Non-zero curl exit status is inserted into the assistant response."
  (let ((proc nil)
        (proc-buf nil))
    (unwind-protect
        (with-temp-buffer
          (llama-swap-chat-mode)
          (insert "\n--- Assistant ---\n")
          (setq llama-swap-chat--response-marker (point-marker))
          (setq proc-buf (generate-new-buffer " *llama-swap-test-chat*"))
          (setq proc (start-process "llama-swap-test-chat-error"
                                    proc-buf "sh" "-c" "exit 22"))
          (while (process-live-p proc)
            (accept-process-output proc 0.1))
          (funcall (llama-swap-chat--make-sentinel (current-buffer))
                   proc "finished\n")
          (should (string-match-p "Generation error: curl exit 22"
                                  (buffer-string))))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (buffer-live-p proc-buf)
        (kill-buffer proc-buf)))))

(provide 'llama-swap-test)
;;; llama-swap-test.el ends here
