(setq load-path (append
                 (list
                  (concat emacs-git "/packages/markdown-mode-20211022.55/"))
                 load-path))
(load "markdown-mode.el")
;(setq markdown-command (concat "pandoc --self-contained -t html --css " emacs-git "../lib/dov-org.css | /home/dov/scripts/premailer --stdout"))
(setq markdown-command (concat emacs-git "../scripts/pandoc-premailer.sh"))

(provide 'init-markdown)
