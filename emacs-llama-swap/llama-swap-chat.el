;;; llama-swap-chat.el --- Streaming chat playground for llama-swap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Purpose-built streaming chat mode.  Sends messages to
;; POST /v1/chat/completions with stream:true and renders
;; OpenAI-compatible SSE deltas in the chat buffer.

;;; Code:

(require 'json)
(require 'llama-swap-auth)
(require 'llama-swap-state)

;;; --- Buffer-local state ---

(defvar-local llama-swap-chat--model nil
  "Model ID for this chat session.")

(defvar-local llama-swap-chat--messages nil
  "Message history as a list of (role . content) pairs, oldest first.")

(defvar-local llama-swap-chat--process nil
  "Active generation process, or nil.")

(defvar-local llama-swap-chat--sse-remainder ""
  "Accumulated partial SSE output from the current generation.")

(defvar-local llama-swap-chat--response-marker nil
  "Buffer marker pointing to the start of the current assistant response.")

(defvar-local llama-swap-chat--in-reasoning nil
  "Non-nil while inside a <think> reasoning block.")

;;; --- Mode definition ---

(defvar llama-swap-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'llama-swap-chat-send)
    (define-key map (kbd "C-c C-k") #'llama-swap-chat-cancel)
    (define-key map (kbd "C-c C-r") #'llama-swap-chat-reset)
    (define-key map (kbd "C-c C-n") #'llama-swap-chat-new)
    map)
  "Keymap for `llama-swap-chat-mode'.")

(define-derived-mode llama-swap-chat-mode text-mode "llama-swap-chat"
  "Major mode for llama-swap streaming chat."
  (setq-local word-wrap t))

;;; --- Prompt area ---

(defconst llama-swap-chat--prompt-separator
  "\n--- You ---\n"
  "Separator before user input area.")

(defun llama-swap-chat--insert-prompt ()
  "Insert the user input prompt at end of buffer."
  (save-excursion
    (goto-char (point-max))
    (insert llama-swap-chat--prompt-separator)))

(defun llama-swap-chat--get-user-input ()
  "Return the text after the last prompt separator, trimmed."
  (save-excursion
    (let ((sep llama-swap-chat--prompt-separator))
      (goto-char (point-max))
      (if (search-backward sep nil t)
          (string-trim (buffer-substring (+ (point) (length sep)) (point-max)))
        ""))))

(defun llama-swap-chat--delete-user-input ()
  "Delete the user input area (after last prompt separator)."
  (let ((sep llama-swap-chat--prompt-separator))
    (save-excursion
      (goto-char (point-max))
      (when (search-backward sep nil t)
        (delete-region (+ (point) (length sep)) (point-max))))))

;;; --- Send ---

(defun llama-swap-chat-send ()
  "Send the current input to the model."
  (interactive)
  (when llama-swap-chat--process
    (user-error "Generation already in progress; use C-c C-k to cancel"))
  (let ((text (llama-swap-chat--get-user-input)))
    (when (string-empty-p text)
      (user-error "No input to send"))
    (llama-swap-chat--delete-user-input)
    (push (cons "user" text) llama-swap-chat--messages)
    (llama-swap-chat--render-message "user" text)
    (llama-swap-chat--start-generation)))

(defun llama-swap-chat--render-message (role content)
  "Append ROLE/CONTENT message block to the chat buffer."
  (save-excursion
    (goto-char (point-max))
    (let* ((label (if (string= role "user") "You" "Assistant"))
           (face  (if (string= role "user") 'font-lock-keyword-face 'font-lock-string-face)))
      (insert (propertize (format "\n--- %s ---\n" label) 'face face))
      (insert content)
      (insert "\n"))))

;;; --- Stream generation ---

(defun llama-swap-chat--build-request-body ()
  "Build the JSON request body alist for chat completions."
  (let* ((history (reverse llama-swap-chat--messages))
         (system  (symbol-value 'llama-swap-chat-system-prompt))
         (msgs    (append
                   (when system
                     (list (list (cons "role" "system")
                                 (cons "content" system))))
                   (mapcar (lambda (m)
                             (list (cons "role" (car m))
                                   (cons "content" (cdr m))))
                           history)))
         (body    (list (cons "model" llama-swap-chat--model)
                        (cons "messages" msgs)
                        (cons "stream" t)
                        (cons "temperature"
                              (or (symbol-value 'llama-swap-chat-temperature) 1.0)))))
    (let ((max-tok (symbol-value 'llama-swap-chat-max-tokens)))
      (when max-tok
        (push (cons "max_tokens" max-tok) body)))
    body))

(defun llama-swap-chat--start-generation ()
  "Start the streaming chat completion."
  (let* ((url      (concat (symbol-value 'llama-swap-base-url)
                           "/v1/chat/completions"))
         (auth-hdr (llama-swap-auth-header))
         (body     (json-encode (llama-swap-chat--build-request-body)))
         (curl-prog (symbol-value 'llama-swap-curl-program))
         (buf      (current-buffer))
         (proc-buf (generate-new-buffer " *llama-swap-chat-curl*"))
         (args     (append
                    (list "--silent" "--show-error" "--no-buffer" "-N")
                    (when auth-hdr (list "-H" auth-hdr))
                    (list "-H" "Content-Type: application/json"
                          "-H" "Accept: text/event-stream"
                          "-X" "POST"
                          "-d" body
                          url)))
         (proc (apply #'start-process "llama-swap-chat" proc-buf curl-prog args)))
    (setq llama-swap-chat--process proc
          llama-swap-chat--sse-remainder ""
          llama-swap-chat--in-reasoning nil)
    ;; Insert assistant header and set marker
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (insert (propertize "\n--- Assistant ---\n" 'face 'font-lock-string-face))
        (setq llama-swap-chat--response-marker (point-marker))))
    (set-process-filter proc (llama-swap-chat--make-filter buf))
    (set-process-sentinel proc (llama-swap-chat--make-sentinel buf))))

;;; --- SSE stream parser ---

(defun llama-swap-chat--make-filter (chat-buf)
  "Return a process filter that streams output into CHAT-BUF."
  (lambda (_proc output)
    (when (buffer-live-p chat-buf)
      (with-current-buffer chat-buf
        (setq llama-swap-chat--sse-remainder
              (concat llama-swap-chat--sse-remainder output))
        (setq llama-swap-chat--sse-remainder
              (replace-regexp-in-string "\r\n" "\n"
                                        llama-swap-chat--sse-remainder))
        (let ((text llama-swap-chat--sse-remainder))
          (while (string-match "\n\n" text)
            (let* ((end   (match-end 0))
                   (frame (substring text 0 end)))
              (setq text (substring text end))
              (llama-swap-chat--process-frame frame)))
          (setq llama-swap-chat--sse-remainder text))))))

(defun llama-swap-chat--process-frame (frame)
  "Process one SSE FRAME from the chat response."
  (dolist (line (split-string frame "\n"))
    (when (string-prefix-p "data:" line)
      (let ((data (string-trim (substring line 5))))
        (cond
         ((string= data "[DONE]")
          ;; Stream finished; sentinel will clean up
          )
         ((not (string-empty-p data))
          (condition-case _
              (let* ((chunk   (json-parse-string data
                                                 :object-type 'alist
                                                 :array-type 'list))
                     (choices (alist-get 'choices chunk))
                     (delta   (and choices
                                   (alist-get 'delta (car choices))))
                     (content (and delta (alist-get 'content delta)))
                     (reasoning (and delta
                                     (or (alist-get 'reasoning_content delta)
                                         (alist-get 'reasoning delta)))))
                (when (and reasoning (not (eq reasoning :null))
                           (not (string-empty-p reasoning)))
                  (llama-swap-chat--append-text
                   (format "<think>%s</think>" reasoning)))
                (when (and content (not (eq content :null))
                           (not (string-empty-p content)))
                  (llama-swap-chat--append-text content)))
            (error nil))))))))

(defun llama-swap-chat--append-text (text)
  "Append TEXT at `llama-swap-chat--response-marker' in the chat buffer."
  (when (and (markerp llama-swap-chat--response-marker)
             (marker-buffer llama-swap-chat--response-marker))
    (with-current-buffer (marker-buffer llama-swap-chat--response-marker)
      (save-excursion
        (goto-char llama-swap-chat--response-marker)
        (insert text)
        (set-marker llama-swap-chat--response-marker (point))))))

;;; --- Sentinel ---

(defun llama-swap-chat--make-sentinel (chat-buf)
  "Return a process sentinel for the chat stream in CHAT-BUF."
  (lambda (proc _event)
    (unless (process-live-p proc)
      (when (buffer-live-p (process-buffer proc))
        (kill-buffer (process-buffer proc)))
      (when (buffer-live-p chat-buf)
        (with-current-buffer chat-buf
          (setq llama-swap-chat--process nil
                llama-swap-chat--sse-remainder ""
                llama-swap-chat--response-marker nil)
          (let ((exit-code (process-exit-status proc)))
            (unless (zerop exit-code)
              (llama-swap-chat--append-text
               (format "\n[Generation error: curl exit %d]\n" exit-code))))
          ;; Save last assistant response to history
          (llama-swap-chat--save-assistant-response)
          ;; Insert new user prompt
          (llama-swap-chat--insert-prompt)
          (goto-char (point-max)))))))

(defun llama-swap-chat--save-assistant-response ()
  "Extract the last assistant response and save it to message history."
  ;; Find the last "--- Assistant ---\n" block and capture its content
  (save-excursion
    (goto-char (point-max))
    (when (search-backward "\n--- Assistant ---\n" nil t)
      (let* ((start (+ (point) (length "\n--- Assistant ---\n")))
             (end   (point-max))
             (text  (string-trim (buffer-substring start end))))
        (unless (string-empty-p text)
          (push (cons "assistant" text) llama-swap-chat--messages))))))

;;; --- Cancel / reset ---

(defun llama-swap-chat-cancel ()
  "Cancel the current generation."
  (interactive)
  (when llama-swap-chat--process
    (when (process-live-p llama-swap-chat--process)
      (delete-process llama-swap-chat--process))
    (setq llama-swap-chat--process nil
          llama-swap-chat--sse-remainder ""
          llama-swap-chat--response-marker nil)
    (llama-swap-chat--insert-prompt)
    (goto-char (point-max))
    (message "llama-swap: generation cancelled")))

(defun llama-swap-chat-reset ()
  "Clear conversation history and buffer."
  (interactive)
  (when (yes-or-no-p "Clear conversation? ")
    (llama-swap-chat-cancel)
    (setq llama-swap-chat--messages nil)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (llama-swap-chat--insert-welcome)
    (llama-swap-chat--insert-prompt)
    (goto-char (point-max))))

(defun llama-swap-chat-new ()
  "Open a new chat buffer for a different model."
  (interactive)
  (let ((model (llama-swap-chat--prompt-model)))
    (llama-swap-chat model)))

;;; --- Welcome message ---

(defun llama-swap-chat--insert-welcome ()
  "Insert welcome message with keybinding hints."
  (insert
   (format "llama-swap chat  |  model: %s\n"
           (or llama-swap-chat--model "?")))
  (insert "C-c C-c  send    C-c C-k  cancel    C-c C-r  reset\n")
  (insert (make-string 50 ?─) "\n"))

;;; --- Entry point ---

(defun llama-swap-chat--prompt-model ()
  "Prompt the user for a model ID, defaulting to known models."
  (let ((models (mapcar (lambda (m) (alist-get 'id m))
                        llama-swap-state--models)))
    (completing-read "Model: " models nil nil nil nil
                     (car models))))

;;;###autoload
(defun llama-swap-chat (model)
  "Open a chat buffer for MODEL."
  (interactive (list (llama-swap-chat--prompt-model)))
  (let* ((buf-name (format "*llama-swap-chat<%s>*" model))
         (buf      (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'llama-swap-chat-mode)
        (llama-swap-chat-mode))
      (setq llama-swap-chat--model model
            llama-swap-chat--messages nil)
      (erase-buffer)
      (llama-swap-chat--insert-welcome)
      (llama-swap-chat--insert-prompt)
      (goto-char (point-max)))
    (switch-to-buffer buf)))

(provide 'llama-swap-chat)
;;; llama-swap-chat.el ends here
