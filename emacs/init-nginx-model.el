(setq load-path (append
                 (list
                  (concat emacs-git "packages/nginx-mode-20240412.402/")
                  )
                 load-path))
		  
(require 'nginx-mode)

(provide 'init-nginx-mode)
