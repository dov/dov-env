(setq load-path (append
                 (list
                  (concat emacs-git "magit-imerge/"))
                 load-path))
(load "magit-imerge.el")

(provide 'init-magit-imerge)
