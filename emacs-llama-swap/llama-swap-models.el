;;; llama-swap-models.el --- Model dashboard for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; tabulated-list-mode dashboard showing model state from the SSE stream.
;; Provides load, unload, and unload-all commands.

;;; Code:

(require 'tabulated-list)
(require 'llama-swap-http)
(require 'llama-swap-state)

(defvar llama-swap-models--buffer-name "*llama-swap-models*"
  "Name of the models dashboard buffer.")

(defvar-local llama-swap-models--show-unlisted nil
  "When non-nil, display unlisted models in the dashboard.")

(defvar-local llama-swap-models--show-peers t
  "When non-nil, include peer models in the dashboard.")

(defvar-local llama-swap-models--show-id nil
  "When non-nil, display model ID instead of name in the Name column.")

;;; --- Mode definition ---

(defvar llama-swap-models-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'llama-swap-models-refresh)
    (define-key map (kbd "l")   #'llama-swap-models-load)
    (define-key map (kbd "u")   #'llama-swap-models-unload)
    (define-key map (kbd "U")   #'llama-swap-models-unload-all)
    (define-key map (kbd "n")   #'llama-swap-models-toggle-name)
    (define-key map (kbd "h")   #'llama-swap-models-toggle-unlisted)
    (define-key map (kbd "p")   #'llama-swap-models-toggle-peers)
    (define-key map (kbd "a")   #'llama-swap-activity-open)
    (define-key map (kbd "L")   #'llama-swap-logs-open-selector)
    (define-key map (kbd "c")   #'llama-swap-models-chat)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "RET") #'llama-swap-models-load)
    map)
  "Keymap for `llama-swap-models-mode'.")

(define-derived-mode llama-swap-models-mode tabulated-list-mode "llama-swap"
  "Major mode for the llama-swap model dashboard."
  (setq tabulated-list-format
        [("State" 10 t)
         ("Model" 35 t)
         ("Peer"   8 t)
         ("Aliases" 20 nil)
         ("Description" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("State" . nil))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function #'llama-swap-models--revert)
  (add-hook 'llama-swap-state-models-changed-hook
            #'llama-swap-models--refresh-if-visible)
  (add-hook 'llama-swap-state-connection-changed-hook
            #'llama-swap-models--update-header)
  (add-hook 'llama-swap-state-inflight-changed-hook
            #'llama-swap-models--update-header)
  (llama-swap-models--update-header))

;;; --- Header line ---

(defun llama-swap-models--update-header ()
  "Refresh the header-line to reflect current connection and in-flight count."
  (when (buffer-live-p (get-buffer llama-swap-models--buffer-name))
    (with-current-buffer llama-swap-models--buffer-name
      (setq header-line-format
            (format "  llama-swap %s  |  %s  |  In-flight: %d"
                    (llama-swap-state-version-string)
                    (llama-swap-state-connection-string)
                    llama-swap-state--in-flight))
      (force-mode-line-update))))

;;; --- Entry conversion ---

(defun llama-swap-models--state-face (state)
  "Return a face symbol for model STATE string."
  (pcase state
    ("ready"    'success)
    ("starting" 'warning)
    ("stopping" 'warning)
    ("stopped"  'shadow)
    ("shutdown" 'error)
    (_          'default)))

(defun llama-swap-models--entry-to-row (model)
  "Convert MODEL alist to a tabulated-list row (ID . COLS)."
  (let* ((id    (alist-get 'id model ""))
         (name  (or (and (not llama-swap-models--show-id)
                         (let ((n (alist-get 'name model)))
                           (if (and n (not (string-empty-p n))) n nil)))
                    id))
         (state (or (alist-get 'state model) "unknown"))
         (peer  (or (alist-get 'peerID model) ""))
         (aliases (let ((a (alist-get 'aliases model)))
                    (if (listp a) (string-join a ", ") "")))
         (desc  (or (alist-get 'description model) ""))
         (face  (llama-swap-models--state-face state)))
    (list id
          (vector
           (propertize state 'face face)
           name
           peer
           aliases
           desc))))

(defun llama-swap-models--visible-models ()
  "Return filtered model list for display."
  (seq-filter
   (lambda (m)
     (let ((is-peer     (not (string-empty-p (or (alist-get 'peerID m) ""))))
           (is-unlisted (eq (alist-get 'unlisted m) t)))
       (and (or llama-swap-models--show-peers (not is-peer))
            (or llama-swap-models--show-unlisted (not is-unlisted)))))
   llama-swap-state--models))

;;; --- Refresh ---

(defun llama-swap-models--revert (&optional _ignore-auto _noconfirm)
  "Revert handler: refresh models from server."
  (llama-swap-models-refresh))

(defun llama-swap-models--redraw ()
  "Redraw the models list from the current state."
  (when (buffer-live-p (get-buffer llama-swap-models--buffer-name))
    (with-current-buffer llama-swap-models--buffer-name
      (setq tabulated-list-entries
            (mapcar #'llama-swap-models--entry-to-row
                    (llama-swap-models--visible-models)))
      (tabulated-list-print t)
      (llama-swap-models--update-header))))

(defun llama-swap-models-refresh ()
  "Refresh model list.
If the SSE stream is connected, this simply redraws the buffer from memory
since state is kept live.  If disconnected, it reconnects the SSE stream."
  (interactive)
  (require 'llama-swap-sse)
  (if (llama-swap-sse-connected-p)
      (progn
        (llama-swap-models--redraw)
        (message "llama-swap: models are live-updating via SSE"))
    (llama-swap-reconnect)))

(defun llama-swap-models--refresh-if-visible ()
  "Refresh the dashboard buffer if it is currently visible."
  (let ((buf (get-buffer llama-swap-models--buffer-name)))
    (when (and buf (get-buffer-window buf))
      (llama-swap-models--redraw))))

;;; --- Commands ---

(defun llama-swap-models--selected-id ()
  "Return the model ID of the currently selected row, or error."
  (or (tabulated-list-get-id)
      (user-error "No model selected")))

(defun llama-swap-models-load ()
  "Load (start) the selected model via GET /upstream/<model>/."
  (interactive)
  (let ((id (llama-swap-models--selected-id)))
    (message "llama-swap: loading %s…" id)
    (llama-swap-http-get
     (format "/upstream/%s/" (url-hexify-string id)) nil
     (lambda (status _data err)
       (if err
           (message "llama-swap: load failed: %s" err)
         (message "llama-swap: %s responded with status %d" id status))))))

(defun llama-swap-models-unload ()
  "Unload (stop) the selected model via POST /api/models/unload/<model>."
  (interactive)
  (let ((id (llama-swap-models--selected-id)))
    (message "llama-swap: unloading %s…" id)
    (llama-swap-http-request
     "POST" (format "/api/models/unload/%s" (url-hexify-string id))
     nil nil
     (lambda (status _data err)
       (if err
           (message "llama-swap: unload failed: %s" err)
         (message "llama-swap: unload %s status %d" id status))))))

(defun llama-swap-models-unload-all ()
  "Unload all models via POST /api/models/unload."
  (interactive)
  (when (yes-or-no-p "Unload all models? ")
    (llama-swap-http-request
     "POST" "/api/models/unload" nil nil
     (lambda (status _data err)
       (if err
           (message "llama-swap: unload-all failed: %s" err)
         (message "llama-swap: unload-all status %d" status))))))

(defun llama-swap-models-toggle-name ()
  "Toggle between displaying model name and model ID."
  (interactive)
  (setq llama-swap-models--show-id (not llama-swap-models--show-id))
  (llama-swap-models--refresh-if-visible))

(defun llama-swap-models-toggle-unlisted ()
  "Toggle display of unlisted models."
  (interactive)
  (setq llama-swap-models--show-unlisted (not llama-swap-models--show-unlisted))
  (llama-swap-models--refresh-if-visible))

(defun llama-swap-models-toggle-peers ()
  "Toggle display of peer models."
  (interactive)
  (setq llama-swap-models--show-peers (not llama-swap-models--show-peers))
  (llama-swap-models--refresh-if-visible))

(defun llama-swap-models-chat ()
  "Open a chat buffer for the selected model."
  (interactive)
  (let ((id (llama-swap-models--selected-id)))
    (require 'llama-swap-chat)
    (llama-swap-chat id)))

;;; --- Entry point ---

(defun llama-swap-models-open ()
  "Open (or switch to) the model dashboard buffer."
  (let ((buf (get-buffer-create llama-swap-models--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'llama-swap-models-mode)
        (llama-swap-models-mode))
      (setq tabulated-list-entries
            (mapcar #'llama-swap-models--entry-to-row
                    (llama-swap-models--visible-models)))
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(provide 'llama-swap-models)
;;; llama-swap-models.el ends here
