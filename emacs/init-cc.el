(autoload 'cc-mode "cc-mode" nil t)

(defun my-cmode-stuff (map) ""
  (update-indent-mode)

  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (define-key map [return] 'newline-and-indent)
  (define-key map [(control c) (control e)] 'goto-compilation-directory-and-compile)
  (define-key map  [(control ?') (control e)] 'goto-compilation-directory-and-compile)
  (define-key map (kbd "C-?") 'c-comment-selection-or-word)
  (define-key map [(alt ? )] 'gud-break)
  (define-key map [(control x) (control ? )] 'gud-break)
  (define-key map [(control c) (control s)] 'dov-git-grep-here)
  ;; versions that deactivates the marker at the end of expantion
  (define-key map [(meta ?/)] 'my-dabbrev-and-deactivate)
  (define-key map [(control ?/)] 'my-dabbrev-and-deactivate)
  (define-key map (kbd "C-<tab>") 'completion-at-point)

  (outline-minor-mode)
  ; outline key bindings
  (outline-keys map)
  (modify-syntax-entry ?_ "w")
  (local-set-key [(alt ?b)] 'left-word)
  (local-set-key [(alt ?f)] 'right-word)
  (c-set-offset 'arglist-intro 2)  

  ;  (subword-mode)
;  (auto-complete-mode 0)   ; I don't believe in autocomplete mode for C/C++
  )



;(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c-mode-common-hook 'font-lock-mode)
(add-hook 'c-mode-hook   (lambda() (my-cmode-stuff c-mode-map)))
(add-hook 'c++-mode-hook (lambda() (my-cmode-stuff c++-mode-map)))

(provide 'init-cc)
