(setq load-path (append
                 (list
                  (concat emacs-git "packages/ssh-agency-20191009.156/")
                  emacs-git
                  )
                 load-path))
		  
(require 'ssh-agency)

(provide 'init-ssh-agency)
