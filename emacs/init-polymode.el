(setq load-path (append
                 (list
                  (concat emacs-git "/polymode")
                  )
                 load-path))
		  
(require 'polymode)
(require 'poly-markdown)

(provide 'init-polymode)
