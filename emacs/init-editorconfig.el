(setq load-path (append
                 (list
                  (concat emacs-git "packages/editorconfig-20240604.602/")
                  )
                 load-path))
		  
(require 'editorconfig)

(provide 'init-editorconfig)
