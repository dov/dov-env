(setq load-path (append
                 (list
                  (concat emacs-git "/packages/helm-20230117.1512/")
                  (concat emacs-git "/packages/helm-core-20230116.1031/")
                  (concat emacs-git "/packages/async-20221228.1315/")
                  emacs-git
                  )
                 load-path))
		  
;; override standard commands with their corresponding helm commands

; execute-extended-command → helm-M-x
;(global-set-key "\M-x" 'execute-extended-command)
;(global-set-key "\M-x" 'helm-M-x)

(defun old-ff (&optional no-op) (interactive)
       (call-interactively 'find-file))
(require 'helm)

;(helm-add-action-to-source "Fallback find-file"
;                       'old-ff
;                       helm-source-find-files)



(load "helm-autoloads" nil t)
(helm-mode 1)

(eval-after-load 'helm-mode
  '(add-to-list 'helm-completing-read-handlers-alist '(old-file)))

;(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq helm-buffer-max-length 30)
(setq helm-buffer-skip-remote-checking 't)

(provide 'init-helm)
