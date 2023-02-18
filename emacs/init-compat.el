(setq load-path (append
                 (list
                 (concat emacs-git "/packages/compat-29.1.3.4/")
                  )
                 load-path))
		  
(require 'compat)

(provide 'init-compat)
