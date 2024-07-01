(setq load-path (append
                 (list
                  (concat emacs-git "packages/git-commit-20240623.1335/")
                  (concat emacs-git "packages/magit-section-20240628.1638/")
                  (concat emacs-git "packages/magit-20240630.1208/"))
                 load-path))
(use-package
  magit)

(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "i") 'magit-gitignore-in-topdir)
  (define-key magit-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile))

(add-hook 'magit-mode-hook 'my-magit-mode-hook)

(setq magit-push-always-verify nil)

(provide 'init-magit)
