(setq load-path (append
                 (list
                  (concat emacs-git "/packages/deadgrep-20230429.429/")
                  (concat emacs-git "/packages/wgrep-deadgrep-20230405.936/")
                  (concat emacs-git "/packages/spinner-1.7.4")
                  emacs-git
                  )
                 load-path))
		  
(global-set-key [(control c) ?s] 'deadgrep)
(defun deadgrep-visit-result-other-window-and-stay ()
  """In dead grep open the current line in another window but keep the position inthe window"""
  (interactive)
  (let ((w (selected-window)))
    (deadgrep--visit-result #'find-file-other-window)
    (select-window w)
    (deadgrep-forward-match)))

(defun my-deadgrep-edit-mode-hook ()
  (define-key deadgrep-mode-map "v" 'deadgrep-visit-result-other-window-and-stay)
  (define-key deadgrep-mode-map "j" 'next-line)
  (define-key deadgrep-mode-map "k" 'previous-line))
(add-hook 'deadgrep-edit-mode-hook 'my-deadgrep-edit-mode-hook)


(require 'deadgrep-autoloads)
(require 'wgrep-deadgrep-autoloads)

(provide 'init-deadgrep)
