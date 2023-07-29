(setq load-path (append
                 (list
                  (concat emacs-git "/packages/git-commit-20230213.2015")
                  (concat emacs-git "/packages/magit-section-20230213.2018")
                  (concat emacs-git "/packages/magit-20230217.2343"))
                 load-path))
(use-package
  magit)

(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "i") 'magit-gitignore-in-topdir)
  (define-key magit-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile))

(add-hook 'magit-mode-hook 'my-magit-mode-hook)

(setq magit-push-always-verify nil)

(provide 'init-magit)
