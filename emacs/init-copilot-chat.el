;; Load copilot chat
;;
;; do (copilot-chat-display) to display the chat

(setq load-path (append (list
                         (concat emacs-git "copilot-chat.el/")
                         (concat emacs-git "chatgpt-shell/")
                         )
                        load-path))

(require 'ansi-color)
(require 'markdown-mode)
(require 'copilot-chat)
(require 'shell-maker)
;(require 'copilot-chat-shell-maker)

(setq copilot-chat-frontend 'shell-maker)

;(push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list)
;(copilot-chat-shell-maker-init)

;; Apply the filter to shell-mode
(add-hook 'copilot-chat-shell-mode-hook 'variable-pitch-mode)
(add-hook 'copilot-chat-shell-shell-mode-hook 'variable-pitch-mode)

(defun copy-buffer-to-temp-markdown ()
  "Copy the current buffer to a temporary read-only buffer and display it in markdown mode."
  (interactive)
  (let* ((ctx (buffer-substring-no-properties (point-min) (point-max)))
         (temp-buffer (generate-new-buffer "*Temp Markdown*"))
         (original-buffer (current-buffer)))
    (with-current-buffer temp-buffer
      (insert ctx)
      (markdown-mode)
      (read-only-mode 1)
      (local-set-key (kbd "q")
                     (lambda ()
                       (interactive)
                       (kill-buffer (current-buffer))))
    (switch-to-buffer temp-buffer)
    (variable-pitch-mode t))))

(global-set-key (kbd "<f6>") 'copy-buffer-to-temp-markdown)

(provide 'init-copilot-chat)

