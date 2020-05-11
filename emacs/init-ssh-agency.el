(setq load-path (append
                 (list
                  (concat emacs-git "packages/ssh-agency-20200329.1558/")
                  emacs-git
                  )
                 load-path))
		  
(require 'ssh-agency)

(provide 'init-ssh-agency)
