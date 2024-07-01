(setq load-path (append
                 (list
                  (concat emacs-git "packages/anaphora-20240120.1744/")
                  )
                 load-path))
		  
(require 'anaphora)

(provide 'init-anaphora)
