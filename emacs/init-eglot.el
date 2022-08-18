(setq load-path (append
                 (list
                  (concat emacs-git "/packages/eglot-20220726.1405/")
                  )
                 load-path))

(require 'init-company-mode)

; Additional stuff needed for lsp mode
(load "eglot")

(provide 'init-eglot)
