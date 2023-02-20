(setq load-path (append
                 (list
                  (concat emacs-git "/packages/csv-mode-1.22/")
                  )
                 load-path))

(load "csv-mode.el")

(provide 'init-csv-mode)
