(setq load-path (append
                 (list
                  (concat emacs-git "/packages/bind-key-20230203.2004/"))
                 load-path))

(load "bind-key-autoloads.el")

(provide 'init-bind-key)

