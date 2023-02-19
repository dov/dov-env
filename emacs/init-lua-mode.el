(autoload 'lua-mode "lua-mode.el" "Lua mode" t nil)

(defun my-lua-mode-hook ()
  (interactive)
  (define-key lua-mode-map [(control c) (control c)] 'shell-lua-on-buffer))

(add-hook 'lua-mode-hook 'my-lua-mode-hook)

(provide 'init-lua-mode)

