(setq load-path (append
                 (list
                  (concat emacs-git "/packages/pythonic-20230821.1733/")
                  (concat emacs-git "/packages/conda-20231109.219/")
                  emacs-git
                  )
                 load-path))

(require 'conda)

(setq conda-env-home-directory (expand-file-name "/terra/mambaforge/"))

;; ;; if you want interactive shell support, include:
;; (conda-env-initialize-interactive-shells)
;; ;; if you want eshell support, include:
;; (conda-env-initialize-eshell)
;; ;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode t)
;; ;; if you want to automatically activate a conda environment on the opening of a file:
;; (add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
;;                                           (conda-env-activate-for-buffer))))
;;
(provide 'init-conda)
