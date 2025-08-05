(setq load-path (append
                 (list
                  (concat emacs-git "packages/literate-calc-mode-20240823.335"))
                 load-path))

(use-package literate-calc-mode)

(provide 'init-literate-calc)

