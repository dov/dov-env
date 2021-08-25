(setq load-path (append
                 (list
                  (concat emacs-git "ein/lisp")
                  )
                 load-path))
		  
(require 'ein)
(require 'ein-notebook)
(require 'ein-jupyter)
(require 'ein-subpackages)

(provide 'init-ein)
