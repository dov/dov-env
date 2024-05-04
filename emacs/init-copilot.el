(setq load-path (append
                 (list
                  (concat emacs-git "copilot")
                  (concat emacs-git "packages/editorconfig-20240318.2049/")
                  )
                 load-path))
		  
(require 'editorconfig)
(require 'copilot)

(defun copilot-accept-completion-by-line-and-newline ()
  """Accept the completion and insert a newline"
  (interactive)

  (copilot-accept-completion-by-line)
  (call-interactively (key-binding (kbd "RET"))))

(defun my-copilot-mode-hook ()
  ;;; Restore the left and right keys
  (define-key copilot-completion-map (kbd "M-RET") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
;  (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
  (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line-and-newline)

  ; write a binding to right bracket
  (define-key copilot-mode-map (kbd "A-C-[") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "A-C-]") #'copilot-previous-completion)
;  (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
  (define-key copilot-mode-map (kbd "A-C-n") #'copilot-accept-completion-by-line-and-newline)
  (define-key copilot-mode-map (kbd "A-C-RET") #'copilot-accept-completion-by-line-and-newline)

  (define-key copilot-mode-map (kbd "A-C-/") 'copilot-complete))

(add-hook 'copilot-mode-hook 'my-copilot-mode-hook)

; Meanwhile let entering copilot not be activated automatically

(provide 'init-copilot)
