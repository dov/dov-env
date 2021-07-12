(setq load-path (append
                 (list
                  (concat emacs-git "/packages/transient-20210530.2252/"))
                 load-path))
(load "transient.el")

(provide 'init-transient)
