(setq load-path (append
                 (list
                  (concat emacs-git "/packages/git-commit-20230213.2015")
                  (concat emacs-git "/packages/magit-section-20230213.2018")
                  (concat emacs-git "/packages/magit-20230217.2343"))
                 load-path))
(use-package
  magit)

(add-hook 'magit-mode-hook 
  (lambda()
    (define-key magit-mode-map (kbd "i") 'magit-gitignore-in-topdir)))

(setq magit-push-always-verify nil)

(provide 'init-magit)
