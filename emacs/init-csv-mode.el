(setq load-path (append
                 (list
                  (concat emacs-git "packages/csv-mode-1.22/")
                  )
                 load-path))

(autoload 'csv-mode "csv-mode.el" "CSV mode" t nil)

(provide 'init-csv-mode)
