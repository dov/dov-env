;;; Windows utilities

(defun buffer-to-visual-studio ()
  """Open the file showed by the current buffer in visual studio"""
  (interactive)
  (let ((cmd (format "OpenFileInVs.py %s %d %d"
                     (buffer-file-name)
                     (line-number-at-pos)
                     (current-column)
                     )))
    (message cmd)
    (shell-command cmd)))
(global-set-key "\C-c\M-s" 'buffer-to-visual-studio)
