; Load the emacs for statistics package
(add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")
(load "ess-autoloads")

(defun find-most-recent-R-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\*R\\*"))

(global-set-key [(alt meta r)] 'find-most-recent-R-buffer)

(ess-toggle-underscore nil)

(require 'ess-r-mode)
(provide 'init-ess)
