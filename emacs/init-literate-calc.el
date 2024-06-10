(setq load-path (append
                 (list
                  (concat emacs-git "packages/literate-calc-mode-20220215.1814//"))
                 load-path))

(use-package literate-calc-mode)

(provide 'init-literate-calc)

