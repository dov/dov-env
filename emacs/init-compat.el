(setq load-path (append
                 (list
                 (concat emacs-git "packages/compat-30.0.0.0/")
                  )
                 load-path))
		  
(require 'compat)

(provide 'init-compat)
