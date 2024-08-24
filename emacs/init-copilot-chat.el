;; Load copilot chat
;;
;; do (copilot-chat-display) to display the chat

(setq load-path (append (list
                         (concat emacs-git "copilot-chat.el/"))
                        load-path))

(require 'copilot-chat)

(provide 'init-copilot-chat)

