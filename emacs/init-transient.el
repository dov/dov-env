(setq load-path (append
                 (list
                  (concat emacs-git "packages/cond-let-20260601.1457")
                  (concat emacs-git "packages/transient-20260617.1137/"))
                 load-path))
(load "transient.el")

(provide 'init-transient)
