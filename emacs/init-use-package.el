(setq load-path (append
                 (list
                  (concat emacs-git "packages/use-package-2.4.5/")
;                  (concat emacs-git "packages/bind-key-20180513.430/")
                  )
                 load-path))

;; Load the emacs for statistics package
;(load "bind-key")
(load "use-package.el")

(provide 'init-use-package)
