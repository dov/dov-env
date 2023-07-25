(setq load-path (append
                 (list
                  (concat emacs-git "/packages/deadgrep-20230429.429/")
                  emacs-git
                  )
                 load-path))
		  
(require 'deadgrep-autoloads)
(provide 'init-deadgrep)
