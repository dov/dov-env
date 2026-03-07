(setq load-path 
  (append
    (list (concat emacs-git "packages/markdown-mode-20211022.55/"))
    load-path))

(defun MyMarkdownHook ()
  (variable-pitch-mode t))

(use-package markdown-mode
  :ensure nil
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command (concat emacs-git "../scripts/pandoc-premailer.sh"))
  (setq markdown-fontify-code-blocks-natively t)
  (add-hook 'markdown-mode-hook 'MyMarkdownHook))

(provide 'init-markdown)
