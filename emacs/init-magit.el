;; -*- lexical-binding: t; -*-

;; Global circuit-breaker to force lexical scope on Magit's position macros
(eval-and-compile
  (setq-default lexical-binding t))

;; Prevent Magit from trying to evaluate the macro that binds "$" dynamically
(with-eval-after-load 'magit-status
  (setq magit-status-goto-file-position nil))

;; 1. Force the active evaluation environment to be lexical
;(setq lexical-binding t)

(setq load-path (append
                 (list
                  (concat emacs-git "packages/seq-2.24/")
                  (concat emacs-git "packages/llama-20260601.1455/")
                  (concat emacs-git "packages/git-commit-20240730.1355/")
                  (concat emacs-git "packages/magit-section-20260503.2051/")
                  (concat emacs-git "packages/magit-20260619.2310/"))
                 load-path))
;; 3. FORCE Llama to load natively with strict lexical rules before Magit initializes
;(eval-and-compile
;  (require 'llama nil t))

(use-package transient
  :ensure t)
(use-package magit
  :ensure t
  :init
  (setq lexical-binding t)
  :config
  ;; Fallback implementation if macro expansions continue to leak $
  (defun magit-status--get-file-position () nil))

;; prevent crash because of lacking tags, needed for using
;(with-eval-after-load 'magit
;  (defun magit-get-current-tag (&optional rev exact-match)
;    "A safety-wrapped version of the original function to prevent TRAMP crashes."
;    (let ((tag (magit-git-str "describe" "--tags"
;                              (and exact-match "--exact-match")
;                              rev)))
;      (if (and tag (>= (length tag) 7))
;          tag
;        nil))))

(defun my-magit-mode-hook ()
  (define-key magit-mode-map (kbd "i") 'magit-gitignore-in-topdir)
  (define-key magit-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile))

(add-hook 'magit-mode-hook 'my-magit-mode-hook)

(setq magit-push-always-verify nil)

(global-set-key "\C-ci" nil)
(global-set-key "\C-cii" 'magit-status)
(global-set-key "\C-cif" 'magit-file-popup)
(global-set-key "\C-cib" 'magit-diff-buffer-file-popup)
(global-set-key "\C-ciB" 'magit-blame-popup)
(global-set-key "\C-cid" 'magit-diff-popup)
(global-set-key "\C-c\C-b" 'magit-blame-mode)

(provide 'init-magit)
