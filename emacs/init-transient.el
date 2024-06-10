(setq load-path (append
                 (list
                  (concat emacs-git "packages/transient-20211105.100/"))
                 load-path))
(load "transient.el")

(provide 'init-transient)
