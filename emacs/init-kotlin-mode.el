(setq load-path (append
                 (list
                  (concat emacs-git "packages/kotlin-mode-20230123.1859/")
                  )
                 load-path))

(autoload 'kotlin-mode "kotlin-mode.el" "CSV mode" t nil)

(provide 'init-kotlin-mode)
