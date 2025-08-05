(setq load-path (append
                 (list
                  (concat emacs-git "packages/cmake-font-lock-20211224.2006")
                  emacs-git
                  )
                 load-path))
		  
(require 'cmake-mode)

;; override standard commands with their corresponding helm commands

(provide 'init-cmake)
