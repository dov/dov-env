;; ignore on Windows

(if (not (string-match "x86_64-w64-mingw32" system-configuration))
    (add-to-list 'load-path (concat emacs-git "/telega.el")))

(add-hook 'telega-chat-mode-hook
          (lambda () 
            (setq bidi-paragraph-start-re "^")
            (setq bidi-paragraph-separate-re "^"))
          t)
(setq telega-chat-use-markdown-formatting t)
(require 'telega)

(provide 'init-telega)
