(setq load-path (append
                 (list
                  (concat emacs-git "packages/literate-calc-mode-20240513.1200/"))
                 load-path))

(use-package literate-calc-mode)

(provide 'init-literate-calc)

