(setq load-path (append
                 (list
                  (concat emacs-git "/packages/company-20220814.1137/"))
                 load-path))

; Load the emacs for statistics package
(load "company")
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

(provide 'init-company-mode)
