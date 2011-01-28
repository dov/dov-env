;;

(defvar gdb-libtool-command-name "libtool"
  "Pathname for executing gdb.")

(defun gdb-libtool (path &optional corefile)
  "Run gdb on a libtool program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  (interactive "FRun gdb-libtool on file: ")
  (load "gud")
  (setq path (file-truename (expand-file-name path)))
  (let ((file (file-name-nondirectory path)))
    (switch-to-buffer (concat "*gud-" file "*"))
    (setq default-directory (file-name-directory path))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (apply 'make-comint
	   (concat "gud-" file)
	   (substitute-in-file-name gdb-libtool-command-name)
	   nil
	   "--mode=execute"
	   (substitute-in-file-name gdb-command-name)
	   "-fullname"
	   "-cd" default-directory
	   file
	   (and corefile (list corefile)))
;    (set-process-filter (get-buffer-process (current-buffer)) 'gud-filter)
;    (set-process-sentinel (get-buffer-process (current-buffer)) 'gud-sentinel)
    ;; XEmacs change: turn on gdb mode after setting up the proc filters
    ;; for the benefit of shell-font.el
    (gud-mode)
    (gud-set-buffer)))
