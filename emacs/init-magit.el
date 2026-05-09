(setq load-path (append
                 (list
                  (concat emacs-git "packages/llama-20260508.949/")
                  (concat emacs-git "packages/git-commit-20240730.1355/")
                  (concat emacs-git "packages/magit-section-20260503.2051/")
                  (concat emacs-git "packages/magit-20260506.643/"))
                 load-path))
(use-package transient
  :ensure t)
(use-package magit
  :ensure t)

; prevent crash because of lacking tags, needed for using
(with-eval-after-load 'magit
  (defun magit-get-current-tag (&optional rev exact-match)
    "A safety-wrapped version of the original function to prevent TRAMP crashes."
    (let ((tag (magit-git-str "describe" "--tags"
                              (and exact-match "--exact-match")
                              rev)))
      (if (and tag (>= (length tag) 7))
          tag
        nil))))

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
