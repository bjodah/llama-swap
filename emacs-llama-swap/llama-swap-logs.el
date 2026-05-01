;;; llama-swap-logs.el --- Log buffers for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Displays proxy and upstream logs from the SSE-fed state.
;; Buffers stay in sync via state change hooks.

;;; Code:

(require 'llama-swap-state)

(defvar llama-swap-logs--proxy-buffer-name    "*llama-swap-proxy-log*")
(defvar llama-swap-logs--upstream-buffer-name "*llama-swap-upstream-log*")
(defvar llama-swap-logs--combined-buffer-name "*llama-swap-log*")

(defvar llama-swap-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")       #'llama-swap-logs-refresh)
    (define-key map (kbd "e")       #'llama-swap-logs-erase)
    (define-key map (kbd "C-c C-k") #'llama-swap-disconnect)
    (define-key map (kbd "q")       #'quit-window)
    map)
  "Keymap for `llama-swap-logs-mode'.")

(define-derived-mode llama-swap-logs-mode special-mode "llama-swap-log"
  "Major mode for llama-swap log buffers."
  (setq buffer-read-only t)
  (setq-local revert-buffer-function
              (lambda (_ignore-auto _noconfirm) (llama-swap-logs-refresh))))

;;; --- Proxy log buffer ---

(defun llama-swap-logs-open-proxy ()
  "Open the proxy log buffer."
  (interactive)
  (llama-swap-logs--open
   llama-swap-logs--proxy-buffer-name
   (lambda () llama-swap-state--proxy-log)
   'llama-swap-state-proxy-log-changed-hook))

;;; --- Upstream log buffer ---

(defun llama-swap-logs-open-upstream ()
  "Open the upstream log buffer."
  (interactive)
  (llama-swap-logs--open
   llama-swap-logs--upstream-buffer-name
   (lambda () llama-swap-state--upstream-log)
   'llama-swap-state-upstream-log-changed-hook))

;;; --- Combined log buffer ---

(defun llama-swap-logs-open-combined ()
  "Open a combined log buffer with both proxy and upstream."
  (interactive)
  (llama-swap-logs--open
   llama-swap-logs--combined-buffer-name
   (lambda ()
     (concat "=== PROXY ===\n"
             llama-swap-state--proxy-log
             "\n=== UPSTREAM ===\n"
             llama-swap-state--upstream-log))
   'llama-swap-state-proxy-log-changed-hook
   'llama-swap-state-upstream-log-changed-hook))

;;; --- Selector ---

(defun llama-swap-logs-open-selector ()
  "Prompt for which log to open."
  (interactive)
  (let ((choice (completing-read "Log: " '("proxy" "upstream" "combined") nil t)))
    (pcase choice
      ("proxy"    (llama-swap-logs-open-proxy))
      ("upstream" (llama-swap-logs-open-upstream))
      ("combined" (llama-swap-logs-open-combined)))))

;;; --- Generic log buffer opener ---

(defun llama-swap-logs--open (buf-name content-fn &rest hooks)
  "Open BUF-NAME and show content from CONTENT-FN.
HOOKS are hook variable symbols to add a refresh listener to."
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'llama-swap-logs-mode)
        (llama-swap-logs-mode))
      ;; Store content function as buffer-local for refresh
      (setq-local llama-swap-logs--content-fn content-fn)
      ;; Add hooks
      (dolist (hook hooks)
        (add-hook hook #'llama-swap-logs--refresh-visible-buffers))
      (llama-swap-logs--render buf content-fn))
    (switch-to-buffer-other-window buf)))

(defvar-local llama-swap-logs--content-fn nil
  "Buffer-local function that returns the current log string.")

(defun llama-swap-logs--render (buf content-fn)
  "Render log content in BUF using CONTENT-FN."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (insert (funcall content-fn))
        ;; Scroll to end if point was at end before
        (goto-char (max (point-min) (min pos (point-max))))))))

(defun llama-swap-logs--refresh-if-visible ()
  "Refresh log buffer if it is currently visible."
  (let ((buf (current-buffer)))
    (when (and (buffer-live-p buf)
               (get-buffer-window buf)
               llama-swap-logs--content-fn)
      (llama-swap-logs--render buf llama-swap-logs--content-fn))))

(defun llama-swap-logs--refresh-visible-buffers ()
  "Refresh all visible llama-swap log buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (derived-mode-p 'llama-swap-logs-mode)
                   (get-buffer-window buf)
                   llama-swap-logs--content-fn)
          (llama-swap-logs--render buf llama-swap-logs--content-fn))))))

(defun llama-swap-logs-refresh ()
  "Refresh the current log buffer."
  (interactive)
  (when llama-swap-logs--content-fn
    (llama-swap-logs--render (current-buffer) llama-swap-logs--content-fn)))

(defun llama-swap-logs-erase ()
  "Clear the local log display (does not affect server logs)."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(provide 'llama-swap-logs)
;;; llama-swap-logs.el ends here
