(setq load-path (append
                 (list
                  (concat emacs-git "/packages/default-text-scale-20191226.2234/"))
                 load-path))
(load "default-text-scale-autoloads.el")

; enable the minor mode to bind the keybindings
(default-text-scale-mode)

(provide 'init-default-text-scale)
