(setq load-path (append
                 (list
                  (concat emacs-git "packages/doom-themes-20240701.308/"))
                 load-path))

; If this fails, then search repo for "coding: utf-8-unix" and change to
; "coding: utf-8"
(load "doom-themes-autoloads.el")

(provide 'init-doom-themes)

