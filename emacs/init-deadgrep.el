(setq load-path (append
                 (list
                  (concat emacs-git "packages/deadgrep-20240627.1535/")
                  (concat emacs-git "packages/wgrep-deadgrep-20231215.1145/")
                  (concat emacs-git "packages/spinner-1.7.4")
                  emacs-git
                  )
                 load-path))
		  
(global-set-key [(control c) ?s] 'deadgrep)
(defun deadgrep-visit-result-other-window-and-stay ()
  """In dead grep open the current line in another window but keep the position inthe window"""
  (interactive)
  (let ((w (selected-window)))
    (deadgrep--visit-result #'find-file-other-window)
    (recenter)
    (select-window w)))

(defun my-deadgrep-edit-mode-hook ()
  (interactive)
  (define-key deadgrep-mode-map "v" 'deadgrep-visit-result-other-window-and-stay)
  (define-key deadgrep-mode-map "j" 'next-line)
  (define-key deadgrep-mode-map "k" 'previous-line))
(add-hook 'deadgrep-mode-hook 'my-deadgrep-edit-mode-hook)


(require 'deadgrep-autoloads)
(require 'wgrep-deadgrep-autoloads)

(provide 'init-deadgrep)
