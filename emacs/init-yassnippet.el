(setq load-path (append
                 (list
                  (concat emacs-git "/packages/yasnippet-20200604.246/"))
                 load-path))

(load "yasnippet")

(setq yas-snippet-dirs (list (concat emacs-git "yasnippet/snippets")
                             (concat emacs-git "snippets")))


;(add-to-list 'load-path
;              (concat emacs-git "yasnippet"))
;(load "yasnippet/yasnippet")
;(yas-global-mode 1)
;(setq yas-snippet-dirs (list (concat emacs-git "yasnippet/snippets")
;                             (concat emacs-git "snippets")))
;
;(yas-reload-all)

(provide 'init-yassnippet)
