;; From: https://gist.github.com/rmuslimov/646aa0e761628dbf0c0fab510a4e4ed6

(defun toggle-window-sticky ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun sticky-window-keep-window-visible ()
  "Insure the buffer associated with the current window stays visible.
This is handy for ERC buffers where you would like to see the
conversation while you work in other windows within the frame.
This is intended to be used with `sticky-window-delete-window'.
A prefix arg reverses this operation."
  (interactive)
  (set-window-dedicated-p (selected-window) (not current-prefix-arg)))

(defun sticky-window-delete-window ()
  "This is intended to be a replacement for `delete-window', but
that avoids deleting windows that have been marked as dedicated
with `sticky-window-keep-window-visible'."
  (interactive)
  (let ((window (selected-window)))
	(if (and (not current-prefix-arg) (window-dedicated-p window))
		(error "This is a dedicated window. Use C-u prefix on this keybinding to really delete it.")
	  (set-window-dedicated-p (selected-window) nil)
	  (delete-window window))))

(defun sticky-window-delete-other-windows ()
  "Delete all other windows that are not marked to be visible with `sticky-window-keep-window-visible'."
  (interactive)
  (mapcar (lambda (window)
			(if (not (window-dedicated-p window))
				(delete-window window)))
		  (cdr (window-list))))

(global-set-key (kbd "C-c t") 'toggle-window-sticky)
(global-set-key (kbd "C-x 1") 'sticky-window-delete-other-windows)

(provide 'sticky-w)
