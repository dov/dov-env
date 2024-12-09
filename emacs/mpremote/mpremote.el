;; A mode for exploring micropython from emacs
;;
;; 2023-08-26 Sat
;;
;; TBD:
;;    - Configure comint so that I can write interactively in the
;;      buffer. 

(setq mp-port "a0")

(defun mpremote-set-global-keybindings ()
  (interactive)
  ;; These binding should really only be set in micropython files
  (global-set-key (kbd "C-c p") nil)
  (global-set-key (kbd "C-c p e") 'mpremote-eval-buffer-raw)
  (global-set-key (kbd "C-c p s") 'mpremote-save)
  (global-set-key (kbd "C-c p r") 'mpremote-save-and-run)
  (global-set-key (kbd "C-c p o") 'mpremote-eval-org-block)
  )

(defun mpremote-eval-org-block ()
  """Mark the current org mode block and send it to mpremote"""
  (interactive)
  (save-excursion
    (org-babel-mark-block)
    (mpremote-eval-raw (buffer-substring (region-beginning) (region-end)))
    (deactivate-mark)))
    
(defun mpremote-send-input ()
  """Send comint input to mpremote with a trailing carriage return"""
  (interactive)
  (comint-send-input t)
  (mpremote-send-string "\r"))

(defun mpremote-send-string (s)
  """Send a string to the mpremote process"""
  (comint-send-string (get-process "mpremote") s))

(defun run-mpremote (&optional port)
  """Start mpremote"""
  (interactive)
  (if (null port)
      (progn
        (setq port (completing-read "Port: " '("u0" "u1" "u2" "a0" "a1" "a2")))))
  (setq mp-port port)
  (message (format "port is %s" mp-port))
  (make-comint-in-buffer "mpremote" "*mpremote*" "mpremote" nil
                         "resume" mp-port)
  (sleep-for 0.1)
  (switch-to-buffer "*mpremote*")
  (use-local-map (copy-keymap comint-mode-map))
  (local-set-key [return] 'mpremote-send-input)
  (sleep-for 0.1)
  (mpremote-send-string "\r")
  (mpremote-set-global-keybindings))

(defun mpremote-reset ()
  """Start mpremote"""
  (interactive)
  (mpremote-send-string "\r\x04")
  (sleep-for 0.5)
  (mpremote-send-string "\x03"))

(defun mpremote-interrupt ()
  """send interrupt to mpremote"""
  (interactive)
  (mpremote-send-string "\x03"))

(defun mpremote-return ()
  """Start mpremote"""
  (interactive)
  (mpremote-send-string "\r"))

(defun mpremote-eval (s)
  """Send a raw string mpremote process"""
  (interactive)
  (comint-send-string (get-process "mpremote") s))

(defun mpremote-eval-raw (s)
  """Send a raw string mpremote process"""
  (comint-send-string (get-process "mpremote")
                      (concat "\x05"  ;; control-e
                              s
                              "\x04"  ;; control-d
                              )))
  
(defun mpremote-test (s)
  """Send a raw string mpremote process"""
  (interactive)
  (comint-send-string (get-process "mpremote") "\x01") ;; control-a
  
)
  
;; This is work in progress
(defun read-proc-until (proc until)
  "Read from comint process PROC until the string UNTIL appears at the end of the process buffer."
  (let ((buffer (process-buffer proc))
        (output ""))
    (with-current-buffer buffer
      (let ((start (point)))
        (while (not (string-suffix-p until output))
          (accept-process-output proc)
          (setq output (buffer-substring-no-properties start (point-max))))))
    output))

(defun mpremote-read-until (until)
  "Read from comint process mpremote until the string UNTIL appears at the end of the process buffer."
  (let ((proc (get-process "mpremote")))
    (read-proc-until proc until)))

; test
;(let ((proc (get-process "mpremote")))
;  (mpremote-send-string "print('hello')\r")
;  (mpremote-read-until ">>> "))

(defun mpremote-raw-mode-run (code)
  "Execute the sequence of commands to interact with mpremote."
  (let ((proc (get-process "mpremote")))
    
    ;; Step 1: enter raw REPL
    (mpremote-send-string "\x01")
    (mpremote-read-until ">") 

    ;; Step 2-5: enter raw-paste mode
    (mpremote-send-string "\x05A\x01")
;    (mpremote-read-until proc "\x01\x80\x00\x01") 

    ;; Step 6-8: write out code
    (mpremote-send-string (concat code "\x04\x02"))
    (let ((ret (mpremote-read-until ">>> "))) ;; Read response
      (substring ret 28 -3))))

(setq mp-port "a0")

(defun mpremote-eval-buffer-raw ()
  """Send the current buffer contents to the mpremote buffer"""
  (interactive)
  (let* ((text 
          (if (region-active-p)
              (buffer-substring (region-beginning) (region-end))
            (buffer-string))))
    (if (string-match "\n " text)
        (mpremote-eval-raw text)
        (mpremote-eval (concat (s-replace "\n" "\r" text) "\r")))))

(defun mpremote-save-old ()
  """There is most likely a better way of doing this"""
  (interactive)
  (mpremote-eval-raw
   (concat
    "with open('" (buffer-name) "','w') as fh:\n"
    "  fh.write('''"
    (s-replace "'''" "\\'\\'\\'" 
        (s-replace "\\" "\\\\" buffer-string))
    "''')\n"
    )))

(defun mpremote-save ()
  """Save the current file to the mpremote device"""
  (interactive)
  (let* ((buffer-name (buffer-name))
         (buffer-string (buffer-string))
         (proc (get-process "mpremote"))
         (buffer (process-buffer proc)))
    (with-current-buffer buffer
      (let ((start (point)))
        (mpremote-raw-mode-run
         (concat
          "with open('" buffer-name "','w') as fh:\n"
          "  fh.write('''"
          (s-replace "'''" "\\'\\'\\'" 
                     (s-replace "\\" "\\\\" buffer-string))
          "''')\n"
          ))
        ;; Remove the overhead of the raw buffer communication
        (delete-region start (point-max))
        )
      (comint-goto-process-mark)
      (insert (concat "*** Uploaded \"" buffer-name "\" ***"))
      (comint-set-process-mark)
      ))
  (mpremote-send-string "\r"))
                      
(defun mpremote-save-and-run ()
  """Save the current file to the device and run it"""
  (interactive)
  (mpremote-interrupt)
  (mpremote-save)
  (mpremote-send-string
   (concat
    "execfile(\""
    (buffer-name)
    "\")\r")))

(provide 'mpremote)
