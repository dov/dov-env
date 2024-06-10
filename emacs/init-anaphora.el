(setq load-path (append
                 (list
                  (concat emacs-git "packages/anaphora-20180618.2200/")
                  )
                 load-path))
		  
(require 'anaphora)

(provide 'init-anaphora)
