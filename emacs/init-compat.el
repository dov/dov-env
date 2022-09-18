(setq load-path (append
                 (list
                  (concat emacs-git "packages/compat-28.1.2.2/")
                  )
                 load-path))
		  
(require 'compat)

(provide 'init-compat)
