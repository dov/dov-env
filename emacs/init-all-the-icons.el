(setq load-path (append
                 (list
                  (concat emacs-git "/packages/all-the-icons-20211108.7/")
                  emacs-git
                  )
                 load-path))
		  
(require 'all-the-icons)

;; override standard commands with their corresponding helm commands

(provide 'init-all-the-icons)
