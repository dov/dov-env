(require 'helm-config)

;; override standard commands with their corresponding helm commands

; execute-extended-command → helm-M-x
(global-set-key "\M-x" 'helm-M-x)

; switch-to-buffer → helm-buffers-list
(global-set-key "\C-xb" 'helm-buffers-list)


(setq helm-buffer-max-length 30)

(provide 'init-helm)
