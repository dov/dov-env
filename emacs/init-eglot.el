;; Configure eglot
(use-package eglot
  :ensure t
  :config
  ;; Optional: Add any additional configuration for eglot here

  (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-<return>") 'minibuffer-choose-completion)

  ;; Dov - I prefer to turn on eglot manually at the moment

;  (add-hook 'python-mode-hook 'eglot-ensure)
  ;; Add more hooks for other languages as needed
)

;; Don't let eglot reformat my code!
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (remove-hook 'post-self-insert-hook #'eglot--post-self-insert-hook t)))
