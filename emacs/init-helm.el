(setq load-path (append
                 (list
                  (concat emacs-git "/packages/helm-20180707.2049/")
                  (concat emacs-git "/packages/helm-core-20180706.946/")
                  (concat emacs-git "/packages/async-20180527.1030/")
                  emacs-git
                  )
                 load-path))
		  
(require 'helm-config)

;; override standard commands with their corresponding helm commands

; execute-extended-command → helm-M-x
;(global-set-key "\M-x" 'execute-extended-command)
(global-set-key "\M-x" 'helm-M-x)

; switch-to-buffer → helm-buffers-list
(global-set-key "\C-xb" 'helm-buffers-list)


(setq helm-buffer-max-length 30)

(provide 'init-helm)
