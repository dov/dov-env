(defun blender-remote-cli-set-global-keybindings ()
  (interactive)
  ;; These binding should really only be set in micropython files
  (global-set-key (kbd "C-c b") nil)
  (global-set-key (kbd "C-c b r") 'blender-eval-buffer)
  )

(defun blender-eval-buffer ()
  "Run buffer in blender-remote-cli, outputting to *Blender Output* buffer."
  (interactive)
  (let* ((file-name (or (buffer-file-name) (buffer-name)))
         (default-dir (if (buffer-file-name) (file-name-directory (buffer-file-name)) default-directory))
         (code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (temp-file (make-temp-file "blender-eval-" nil ".py"))
         (output-buffer-name "*Blender Output*")
         (bootstrap-script (format "import traceback, sys
try:
    with open('%s', 'r') as f:
        code_obj = compile(f.read(), '%s', 'exec')
        exec(code_obj)
except Exception:
    exc_type, exc_value, exc_traceback = sys.exc_info()
    traceback.print_exception(exc_type, exc_value, exc_traceback.tb_next, file=sys.stdout)" 
                                   temp-file (file-name-nondirectory file-name))))

    (with-temp-file temp-file
      (insert code))

    (with-current-buffer (get-buffer-create output-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory default-dir)
      (insert (format "cd %s\n" default-dir)))
    
    (message "Executing in Blender...")
    
    (call-process "blender-remote-cli" nil output-buffer-name nil "execute" "-c" bootstrap-script)
    
    (with-current-buffer output-buffer-name
      (setq buffer-read-only nil)
      (compilation-mode))
    
    (display-buffer output-buffer-name)
    (delete-file temp-file)))

(blender-remote-cli-set-global-keybindings)

(provide 'init-python-remote-cli)
