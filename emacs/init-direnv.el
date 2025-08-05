(setq load-path (append
                 (list
                  (concat emacs-git "packages/direnv-20240314.715")
                  )
                 load-path))
		  
(require 'direnv)

(provide 'init-direnv)
