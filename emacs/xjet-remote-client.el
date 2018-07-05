;; An example of a xjet tcp ip remote daemon client in emacs lisp.
;;
;; Dov Grobgeld
;; 2018-07-05 Thu

(require 'bindat)

(setq xjet-remote-daemon-port 2000)
(setq xjet-remote-daemon-host "127.0.0.1")

(defun xjet-listen-start nil
  "starts an emacs tcp client listener"
  (interactive)
  (make-network-process :name "xjet-client" :buffer "*xjet-client*" :host xjet-remote-daemon-host
                        :service xjet-remote-daemon-port :filter 'xjet-listen-filter))

;; Return messages are returned here. 
(defun xjet-listen-filter (proc str)
  (setq str (string-to-unibyte str))
  (while (> (length str) 0)
    (setq status (bindat-get-field (bindat-unpack '((:len u32r)) (substring str 4 8)) :len))
    (setq msg-len (bindat-get-field (bindat-unpack '((:len u32r)) (substring str 8 12)) :len))
    (setq msg (substring str 12 (+ 12 msg-len)) )
    (message msg)
    (setq str (substring str (+ 12 msg-len)))
    )
  (delete-process proc)
  msg)

(defun xjet-remote-command (op value)
  "Send a python command according to the xjet remote protocol"
  (setq proc (xjet-listen-start))
  (setq pylen (length value))
  (process-send-string proc
    (concat
      "\104\063\042\021"   ; x44 x33 x22 x11 -- magic bytes
      (bindat-pack '((:len u32r)) (list (cons :len op)))  ; xc    -- op=12 python string
      (bindat-pack '((:len u32r)) (list (cons :len pylen))) ; string len
      value)))

(defun xjet-remote-python-string (string)
  (xjet-remote-command 12 string))

(defun xjet-remote-python-file (filename)
  (xjet-remote-command 13 filename))

(provide 'xjet-remote-client)
