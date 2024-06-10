(setq load-path (append
                 (list
                  (concat emacs-git "packages/with-editor-20210524.1654/"))
                 load-path))
(load "with-editor.el")

(provide 'init-with-editor)
