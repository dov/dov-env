;; A mode for exploring micropython from emacs
;;
;; 2023-08-26 Sat
;;
;; TBD:
;;    - Configure comint so that I can write interactively in the
;;      buffer. 

(setq mp-port "a0")

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
  (mpremote-send-string "\r"))

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

(defun mpremote-save ()
  """There is most likely a better way of doing this"""
  (interactive)
  (mpremote-eval-raw
   (concat
    "with open('" (buffer-name) "','w') as fh:\n"
    "  fh.write('''"
    (s-replace "'''" "\\'\\'\\'" (buffer-string))
    "''')\n"
    )))
                      
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

(define-key python-mode-map (kbd "C-c p") 'mpremote-eval-buffer-raw)

(provide 'mpremote)
