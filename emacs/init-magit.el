(setq load-path (append
                 (list
                  (concat emacs-git "packages/transient-20240729.1524/")
                  (concat emacs-git "packages/git-commit-20240730.1355/")
                  (concat emacs-git "packages/magit-section-20240730.1741/")
                  (concat emacs-git "packages/magit-20240730.1741/"))
                 load-path))
(use-package transient
  :ensure t)
(use-package magit
  :ensure t)

(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "i") 'magit-gitignore-in-topdir)
  (define-key magit-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile))

(add-hook 'magit-mode-hook 'my-magit-mode-hook)

(setq magit-push-always-verify nil)

(global-set-key "\C-ci" nil)
(global-set-key "\C-cii" 'magit-status)
(global-set-key "\C-cif" 'magit-file-popup)
(global-set-key "\C-cib" 'magit-diff-buffer-file-popup)
(global-set-key "\C-ciB" 'magit-blame-popup)
(global-set-key "\C-cid" 'magit-diff-popup)
(global-set-key "\C-c\C-b" 'magit-blame-mode)

(provide 'init-magit)
