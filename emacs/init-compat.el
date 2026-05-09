(setq load-path (append
                 (list
                 (concat emacs-git "packages/compat-31.0.0.1")
                  )
                 load-path))
		  
(require 'compat)

(provide 'init-compat)
