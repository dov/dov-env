;; An emacs interface to
;;
;;  https://github.com/igamenovoer/blender-remote
;;
;; Usage:
;;
;;   Run blender as:
;;
;;     blender-remote-cli start
;;
;;  Edit a python script and send it by "C-c b r" to blender
;;
;; Dov Grobgeld <dov.grobgeld@gmail.com>
;;  2026-03-08 Sun

(defun blender-remote-cli-set-global-keybindings ()
  (interactive)
  ;; These binding should really only be set in micropython files
  (global-set-key (kbd "C-c b") nil)
  (global-set-key (kbd "C-c b r") 'blender-eval-buffer)
  )

(defun blender-eval-buffer ()
  "Run buffer in blender-remote-cli, outputting to *Blender Output* buffer."
  (interactive)
  (let* ((FileName (or (buffer-file-name) (buffer-name)))
         (DefaultDir (if (buffer-file-name) (file-name-directory (buffer-file-name)) default-directory))
         (Code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-substring-no-properties (point-min) (point-max))))
         (TempFile (make-temp-file "blender-eval-" nil ".py"))
         (OutputBufferName "*Blender Output*")
         ;; Updated BootstrapScript with sys.path and __main__ support
         (BootstrapScript (format "import traceback, sys, os
PathToRun = r'%s'
ScriptDir = os.path.dirname(PathToRun)
CurrentDir = os.getcwd()

if ScriptDir not in sys.path:
  sys.path.insert(0, ScriptDir)
if CurrentDir not in sys.path:
  sys.path.insert(0, CurrentDir)

try:
  with open('%s', 'r') as f:
    CodeObj = compile(f.read(), '%s', 'exec')
    GlobalDict = {
      '__file__': PathToRun,
      '__name__': '__main__',
      '__package__': None,
    }
    import builtins
    GlobalDict['__builtins__'] = builtins
    exec(CodeObj, GlobalDict)
except Exception:
  ExcType, ExcValue, ExcTraceback = sys.exc_info()
  traceback.print_exception(ExcType, ExcValue, ExcTraceback.tb_next, file=sys.stdout)"
                                   FileName
                                   TempFile
                                   (file-name-nondirectory FileName))))

    (with-temp-file TempFile
      (insert Code))

    (with-current-buffer (get-buffer-create OutputBufferName)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq default-directory DefaultDir)
      (insert (format "cd %s\n" DefaultDir)))
    
    (message "Executing in Blender...")
    
    (call-process "blender-remote-cli" nil OutputBufferName nil "execute" "-c" BootstrapScript)
    
    (with-current-buffer OutputBufferName
      (setq buffer-read-only nil)
      (compilation-mode))
    
    (display-buffer OutputBufferName)
    (delete-file TempFile)))

(blender-remote-cli-set-global-keybindings)

(provide 'init-python-remote-cli)
