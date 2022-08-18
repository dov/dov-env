(setq load-path (append
                 (list
                  (concat emacs-git "/packages/company-20220814.1137/"))
                 load-path))

; Load the emacs for statistics package
(load "company")

(provide 'init-company-mode)
