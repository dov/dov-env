(setq load-path (append
                 (list
                  (concat emacs-git "packages/skeletor-20210129.239/"))
                 load-path))
(require 'f-shortdoc)
(require 'f)

; Add my c-project

(setq skeletor--directory (concat emacs-git "skeletor-templates"))
(setq skeletor--licenses-directory (concat emacs-git "skeletor-licenses"))
(setq skeletor-project-directory (concat (getenv "HOME") "git"))

(load "skeletor.el")

(skeletor-define-template "dov-cpp-project"
  :default-license (rx bol 
  ))
(skeletor-define-template "xjet-log-project"
  :default-license "xjet.txt"
  )
(provide 'init-skeletor)
