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

;; Return message is returned here. TBD: parse and raise error
(defun xjet-listen-filter (proc string)
  (delete-process proc))

(defun xjet-remote-command (op value)
  "Send a python command according to the xjet remote protocol"
  (interactive)
  (setq proc (xjet-listen-start))
  (setq pystring value)
  (setq pylen (length pystring))
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

