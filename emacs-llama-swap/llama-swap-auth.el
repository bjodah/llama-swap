;;; llama-swap-auth.el --- Authentication for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Resolves Bearer token API key via auth-source, explicit defcustom,
;; or nil (no auth).  llama-swap accepts Bearer <key>, Basic (password=key),
;; or x-api-key.  We always build a Bearer header.

;;; Code:

(require 'auth-source)
(require 'url-parse)

(defvar llama-swap-auth--cached-key nil
  "Cached API key string, or nil, or the symbol `none' when no key is available.")

(defun llama-swap-auth--url-host ()
  "Extract host from `llama-swap-base-url'."
  (let ((parsed (url-generic-parse-url (symbol-value 'llama-swap-base-url))))
    (url-host parsed)))

(defun llama-swap-auth--try-auth-source ()
  "Look up API key in auth-source.
Returns key string or nil."
  (let* ((host (or (symbol-value 'llama-swap-auth-source-host)
                   (llama-swap-auth--url-host)))
         (found (car (auth-source-search :host host
                                         :port "llama-swap"
                                         :max 1))))
    (when found
      (let ((secret (plist-get found :secret)))
        (when secret
          (if (functionp secret) (funcall secret) secret))))))

(defun llama-swap-auth--try-explicit ()
  "Return `llama-swap-api-key' if non-nil."
  (symbol-value 'llama-swap-api-key))

(defun llama-swap-auth-get-key ()
  "Return the API key string, or nil if none is configured.
Tries: 1) cache, 2) explicit `llama-swap-api-key', 3) auth-source."
  (when (null llama-swap-auth--cached-key)
    (setq llama-swap-auth--cached-key
          (or (llama-swap-auth--try-explicit)
              (llama-swap-auth--try-auth-source)
              'none)))
  (if (eq llama-swap-auth--cached-key 'none)
      nil
    llama-swap-auth--cached-key))

(defun llama-swap-auth-clear-cache ()
  "Clear cached API key."
  (interactive)
  (setq llama-swap-auth--cached-key nil)
  (message "llama-swap: auth cache cleared"))

(defun llama-swap-auth-header ()
  "Return a Bearer Authorization header string, or nil if no key configured."
  (let ((key (llama-swap-auth-get-key)))
    (when key
      (format "Authorization: Bearer %s" key))))

(provide 'llama-swap-auth)
;;; llama-swap-auth.el ends here
