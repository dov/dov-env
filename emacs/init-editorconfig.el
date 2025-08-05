(setq load-path (append
                 (list
                  (concat emacs-git "packages/editorconfig-20250219.1528/")
                  )
                 load-path))
		  
(require 'editorconfig)

(provide 'init-editorconfig)
