(setq load-path (append
                 (list
                  (concat emacs-git "/packages/use-package-20220809.42/")
                  (concat emacs-git "/packages/bind-key-20220815.1925/")
                  )
                 load-path))

; Load the emacs for statistics package
(load "bind-key")
(load "use-package")

(provide 'init-use-package)
