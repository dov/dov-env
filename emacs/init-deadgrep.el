(setq load-path (append
                 (list
                  (concat emacs-git "/packages/deadgrep-20230429.429/")
                  (concat emacs-git "/packages/wgrep-deadgrep-20230405.936/")
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

(define-key deadgrep-mode-map "v" 'deadgrep-visit-result-other-window-and-stay)
(define-key deadgrep-mode-map "j" 'next-line)
(define-key deadgrep-mode-map "k" 'previous-line)

(require 'deadgrep-autoloads)
(require 'wgrep-deadgrep-autoloads)

(provide 'init-deadgrep)
