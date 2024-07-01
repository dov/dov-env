(setq load-path (append
                 (list
                  (concat emacs-git "packages/emojify-20200309.553/")
                  (concat emacs-git "packages/ht-20210119.741/"))
                 load-path))
(load "emojify-autoloads.el")
(load "ht-autoloads.el")

;; should it be on by default?

(setq emojify-emoji-set "emojione-v2.2.6")
;(global-emojify-mode) ; :-) 

(global-set-key "\C-cea" 'emojify-apropos-emoji)
(global-set-key "\C-cep" 'emojify-describe-emoji-at-point)

(provide 'init-emojify)
