(setq load-path (append
                 (list
                  (concat emacs-git "/packages/eglot-20220726.1405/")
                  (concat emacs-git "/packages/project-0.8.1")
                  (concat emacs-git "/packages/eldoc-1.13.0")
                  )
                 load-path))

(require 'init-company-mode)

; Additional stuff needed for eglot mode
(load "project")
(load "eldoc")
(load "eglot")

; TBD - put this in a generic place
(add-to-list 'eglot-server-programs
  `(lua-mode . ("/space3/pub-repos/lua-language-server/bin/lua-language-server")))

(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-to-list 'eglot-server-programs '(vala-mode "vala-language-server"))

; Do I want this?
;(add-hook 'eglot-managed-mode-hook
;          (lambda ()
;            (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)))
;(add-hook 'c-mode-hook 'eglot-ensure)
;(add-hook 'c++-mode-hook 'eglot-ensure)

(provide 'init-eglot)
