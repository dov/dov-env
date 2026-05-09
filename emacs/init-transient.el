(setq load-path (append
                 (list
                  (concat emacs-git "packages/transient-20260507.1521/"))
                 load-path))
(load "transient.el")

(provide 'init-transient)
