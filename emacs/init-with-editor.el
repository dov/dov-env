(setq load-path (append
                 (list
                  (concat emacs-git "packages/with-editor-20240623.1757/"))
                 load-path))
(load "with-editor.el")

(provide 'init-with-editor)
