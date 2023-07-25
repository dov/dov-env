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
;(global-set-key "\C-xb" 'helm-buffer-switch-buffers)

(defun old-shell-command (&optional no-op) (interactive)
       (call-interactively 'shell-command))
(use-package helm
   :demand t
   :bind* (:map helm-map 
                ([tab] . helm-execute-if-single-persistent-action)
                ("C-i" . helm-execute-persistent-action)
                ))

(load "helm-autoloads" nil t)
;(helm-mode nil)

; from
; https://emacs.stackexchange.com/questions/63461/how-to-turn-on-helm-mode-for-a-specific-function
(defun execute-with-helm (command)
  (if helm-mode
      (call-interactively command))
  (progn
    (helm-mode 1)
    ;; We call `unwind-protect' to ensure that `helm-mode' is
    ;; disabled even though `command' doesn't complete normally.
    ;;
    ;; Without `unwind-protect', if the user presses =C-g= while
    ;; `command' is being executed, then the entire function would
    ;; be exited and therefore, `helm-mode' wouldn't be disabled'
    (unwind-protect (call-interactively command)
      (helm-mode -1))))

(defun old-ff (&optional no-op) (interactive)
       (call-interactively 'find-file))

(eval-after-load 'helm-mode
  '(add-to-list 'helm-completing-read-handlers-alist '(old-ff)))

(setq helm-mode-no-completion-in-region-in-modes
      '(shell-mode))
        
(defun my-shell-command (&optional no-op)
  (interactive)
  (let ((completion-in-region-function 'completion--in-region))
    (call-interactively 'shell-command)))
(global-set-key "\M-!" 'my-shell-command)

(defun my-insert-file (&optional no-op)
  (interactive)
  (let ((completion-in-region-function 'completion--in-region))
    (call-interactively 'insert-file)))
(global-set-key "\C-xi" 'my-insert-file)

(defun my-write-file (&optional no-op)
  (interactive)
  (let* ((save-helm-mode (helm-mode))
         (completion-in-region-function nil))
    (helm-mode 0)
    (call-interactively 'write-file)
    (helm-mode save-helm-mode)))
(global-set-key "\C-x\C-w" 'my-write-file)

; helm-dabbrev has the annoying feature to leave a selection of the trailing
; word at the end of the completion. This function turns that off.
(defun my-dabbrev-and-deactivate ()
  (interactive)
  (call-interactively 'helm-dabbrev)
  (deactivate-mark))

(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(define-key global-map "\C-xb" 'helm-mini)
(define-key global-map [(meta ?/)] 'my-dabbrev-and-deactivate)
(define-key global-map [(control ?/)] 'my-dabbrev-and-deactivate)

(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq helm-buffer-max-length 30)
(setq helm-buffer-skip-remote-checking 't)

(provide 'init-helm)
