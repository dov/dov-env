(setq load-path (append
                 (list
                  (concat emacs-git "packages/cmake-font-lock-20211224.2006/")
                  (concat emacs-git "packages/cmake-mode-20230215.1434/")
                  emacs-git
                  )
                 load-path))
		  
(require 'cmake-mode)
(require 'cmake-font-lock)

;; override standard commands with their corresponding helm commands

(provide 'init-cmake)
