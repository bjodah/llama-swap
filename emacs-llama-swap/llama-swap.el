;;; llama-swap.el --- Emacs client for llama-swap proxy  -*- lexical-binding: t; -*-

;; Author: llama-swap contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai, llm
;; URL: https://github.com/mostlygeek/llama-swap

;;; Commentary:

;; An Emacs client for the llama-swap transparent proxy server.
;; Provides a model dashboard with load/unload controls, proxy and
;; upstream log buffers, an activity table with capture inspection,
;; and a streaming chat playground — all backed by the /api/events
;; SSE stream.

;;; Code:

(defgroup llama-swap nil
  "Emacs client for llama-swap proxy."
  :group 'tools
  :prefix "llama-swap-")

(defcustom llama-swap-base-url "http://localhost:8080"
  "Base URL of the llama-swap management API."
  :type 'string
  :group 'llama-swap)

(defcustom llama-swap-api-key nil
  "API key for llama-swap Bearer authentication.
When nil, auth-source is tried first.  Set to an empty string to
disable authentication entirely."
  :type '(choice (const nil) string)
  :group 'llama-swap)

(defcustom llama-swap-auth-source-host nil
  "Host to use for auth-source lookup.
When nil, the host from `llama-swap-base-url' is used."
  :type '(choice (const nil) string)
  :group 'llama-swap)

(defcustom llama-swap-curl-program "curl"
  "Path to the curl executable."
  :type 'string
  :group 'llama-swap)

(defcustom llama-swap-request-timeout 30
  "Default HTTP request timeout in seconds."
  :type 'integer
  :group 'llama-swap)

(defcustom llama-swap-log-max-size (* 100 1024)
  "Maximum byte size of each log buffer before oldest text is trimmed."
  :type 'integer
  :group 'llama-swap)

(defcustom llama-swap-max-metrics 100
  "Maximum number of activity entries to keep in memory."
  :type 'integer
  :group 'llama-swap)

(defcustom llama-swap-chat-temperature 1.0
  "Default temperature for chat completions."
  :type 'number
  :group 'llama-swap)

(defcustom llama-swap-chat-max-tokens nil
  "Default max_tokens for chat completions.  nil means omit the field."
  :type '(choice (const nil) integer)
  :group 'llama-swap)

(defcustom llama-swap-chat-system-prompt nil
  "Default system prompt text for chat sessions.  nil means no system message."
  :type '(choice (const nil) string)
  :group 'llama-swap)

(require 'llama-swap-auth)
(require 'llama-swap-http)
(require 'llama-swap-state)
(require 'llama-swap-sse)
(require 'llama-swap-models)
(require 'llama-swap-logs)
(require 'llama-swap-activity)
(require 'llama-swap-chat)

;;;###autoload
(defun llama-swap ()
  "Open the llama-swap model dashboard and start the event stream."
  (interactive)
  (unless (executable-find llama-swap-curl-program)
    (user-error "llama-swap: curl not found.  Install curl or set `llama-swap-curl-program'"))
  (llama-swap-models-open)
  (unless (llama-swap-sse-connected-p)
    (llama-swap-connect)))

;;;###autoload
(defun llama-swap-connect ()
  "Connect to the llama-swap event stream."
  (interactive)
  (llama-swap-sse-start))

;;;###autoload
(defun llama-swap-disconnect ()
  "Disconnect from the llama-swap event stream."
  (interactive)
  (llama-swap-sse-stop))

;;;###autoload
(defun llama-swap-reconnect ()
  "Reconnect to the llama-swap event stream."
  (interactive)
  (llama-swap-sse-stop)
  (llama-swap-sse-start))

(provide 'llama-swap)
;;; llama-swap.el ends here
