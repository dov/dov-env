;; Keyboard macro editor for GNU Emacs
;; Copyright (C) 1990 Dave Gillespie

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Usage:
;;   (autoload 'edit-kbd-macro "macedit" "Edit a named keyboard macro" t)
;;   (autoload 'edit-last-kbd-macro "macedit" "Edit a keyboard macro" t)



;; To use, type `M-x edit-last-kbd-macro' to edit the most recently
;; defined keyboard macro.  If you have used `M-x name-last-kbd-macro'
;; to give a keyboard macro a name, type `M-x edit-kbd-macro' to edit
;; the macro by name.  When you are done editing, type `C-c C-c' to
;; record your changes back into the original keyboard macro.




;;; The user-level commands for editing macros.

(defun edit-last-kbd-macro (&optional prefix buffer hook)
  "Edit the most recently defined keyboard macro."
  (interactive "P")
  (MacEdit-edit-macro last-kbd-macro
		      (function (lambda (x arg) (setq last-kbd-macro x)))
		      prefix buffer hook)
)

(defun edit-kbd-macro (cmd &optional prefix buffer hook in-hook out-hook)
  "Edit a keyboard macro which has been assigned a name by name-last-kbd-macro.
\(See also edit-last-kbd-macro.)"
  (interactive "CCommand name: \nP")
  (and cmd
       (MacEdit-edit-macro (if in-hook
			       (funcall in-hook cmd)
			     (symbol-function cmd))
			   (or out-hook
			       (list 'lambda '(x arg)
				     (list 'fset
					   (list 'quote cmd)
					   'x)))
			   prefix buffer hook cmd))
)




;;; Formatting a keyboard macro as human-readable text.

(defun MacEdit-print-macro (macro-str local-map)
  (let ((save-map (current-local-map))
	(print-escape-newlines t)
	key-symbol key-str key-last prefix-arg this-prefix)
    (unwind-protect
	(progn
	  (use-local-map local-map)
	  (while (MacEdit-peek-char)
	    (MacEdit-read-key)
	    (setq this-prefix prefix-arg)
	    (or (memq key-symbol '(digit-argument
				   negative-argument
				   universal-argument))
		(null prefix-arg)
		(progn
		  (cond ((consp prefix-arg)
			 (insert (format "prefix-arg (%d)\n"
					 (car prefix-arg))))
			((eq prefix-arg '-)
			 (insert "prefix-arg -\n"))
			((numberp prefix-arg)
			 (insert (format "prefix-arg %d\n" prefix-arg))))
		  (setq prefix-arg nil)))
	    (cond ((null key-symbol)
		   (insert "type \"")
		   (MacEdit-insert-string macro-str)
		   (insert "\"\n")
		   (setq macro-str ""))
		  ((eq key-symbol 'digit-argument)
		   (MacEdit-prefix-arg key-last nil prefix-arg))
		  ((eq key-symbol 'negative-argument)
		   (MacEdit-prefix-arg ?- nil prefix-arg))
		  ((eq key-symbol 'universal-argument)
		   (let* ((c-u 4) (argstartchar last-command-char)
			  (char (MacEdit-read-char)))
		     (while (= char argstartchar)
		       (setq c-u (* 4 c-u)
			     char (MacEdit-read-char)))
		     (MacEdit-prefix-arg char c-u nil)))
		  ((eq key-symbol 'self-insert-command)
		   (insert "insert ")
		   (if (and (>= key-last 32) (<= key-last 126))
		       (let ((str ""))
			 (while (or (and (eq key-symbol
					     'self-insert-command)
					 (< (length str) 60)
					 (>= key-last 32)
					 (<= key-last 126))
				    (and (memq key-symbol
					       '(backward-delete-char
						 delete-backward-char
						 backward-delete-char-untabify))
					 (> (length str) 0)))
			   (if (eq key-symbol 'self-insert-command)
			       (setq str (concat str
						 (char-to-string key-last)))
			     (setq str (substring str 0 -1)))
			   (MacEdit-read-key))
			 (insert "\"" str "\"\n")
			 (MacEdit-unread-chars key-str))
		     (insert "\"")
		     (MacEdit-insert-string (char-to-string key-last))
		     (insert "\"\n")))
		  ((and (eq key-symbol 'quoted-insert)
			(MacEdit-peek-char))
		   (insert "quoted-insert\n")
		   (let ((ch (MacEdit-read-char))
			 ch2)
		     (if (and (>= ch ?0) (<= ch ?7))
			 (progn
			   (setq ch (- ch ?0)
				 ch2 (MacEdit-read-char))
			   (if ch2
			       (if (and (>= ch2 ?0) (<= ch2 ?7))
				   (progn
				     (setq ch (+ (* ch 8) (- ch2 ?0))
					   ch2 (MacEdit-read-char))
				     (if ch2
					 (if (and (>= ch2 ?0) (<= ch2 ?7))
					     (setq ch (+ (* ch 8) (- ch2 ?0)))
					   (MacEdit-unread-chars ch2))))
				 (MacEdit-unread-chars ch2)))))
		     (if (or (and (>= ch ?0) (<= ch ?7))
			     (< ch 32) (> ch 126))
			 (insert (format "type \"\\%03o\"\n" ch))
		       (insert "type \"" (char-to-string ch) "\"\n"))))
		  ((memq key-symbol '(isearch-forward
				      isearch-backward
				      isearch-forward-regexp
				      isearch-backward-regexp))
		   (insert (symbol-name key-symbol) "\n")
		   (MacEdit-isearch-argument))
		  ((eq key-symbol 'execute-extended-command)
		   (MacEdit-read-argument obarray 'commandp))
		  (t
		   (let ((cust (get key-symbol 'MacEdit-print)))
		     (if cust
			 (funcall cust)
		       (insert (symbol-name key-symbol))
		       (indent-to 30)
		       (insert " # ")
		       (MacEdit-insert-string key-str)
		       (insert "\n")
		       (let ((int (MacEdit-get-interactive key-symbol)))
			 (if (string-match "\\`\\*" int)
			     (setq int (substring int 1)))
			 (while (> (length int) 0)
			   (cond ((= (aref int 0) ?a)
				  (MacEdit-read-argument
				   obarray nil))
				 ((memq (aref int 0) '(?b ?B ?D ?f ?F ?n
							  ?s ?S ?x ?X))
				  (MacEdit-read-argument))
				 ((and (= (aref int 0) ?c)
				       (MacEdit-peek-char))
				  (insert "type \"")
				  (MacEdit-insert-string
				   (char-to-string
				    (MacEdit-read-char)))
				  (insert "\"\n"))
				 ((= (aref int 0) ?C)
				  (MacEdit-read-argument
				   obarray 'commandp))
				 ((= (aref int 0) ?k)
				  (MacEdit-read-key)
				  (if key-symbol
				      (progn
					(insert "type \"")
					(MacEdit-insert-string key-str)
					(insert "\"\n"))
				    (MacEdit-unread-chars key-str)))
				 ((= (aref int 0) ?N)
				  (or this-prefix
				      (MacEdit-read-argument)))
				 ((= (aref int 0) ?v)
				  (MacEdit-read-argument
				   obarray 'user-variable-p)))
			   (let ((nl (string-match "\n" int)))
			     (setq int (if nl
					   (substring int (1+ nl))
					 "")))))))))))
      (use-local-map save-map)))
)

(defun MacEdit-prefix-arg (char c-u value)
  (let ((sign 1))
    (if (and (numberp value) (< value 0))
	(setq sign -1 value (- value)))
    (if (eq value '-)
	(setq sign -1 value nil))
    (while (and char (= ?- char))
      (setq sign (- sign) c-u nil)
      (setq char (MacEdit-read-char)))
    (while (and char (>= char ?0) (<= char ?9))
      (setq value (+ (* (if (numberp value) value 0) 10) (- char ?0)) c-u nil)
      (setq char (MacEdit-read-char)))
    (setq prefix-arg
	  (cond (c-u (list c-u))
		((numberp value) (* value sign))
		((= sign -1) '-)))
    (MacEdit-unread-chars char))
)

(defun MacEdit-insert-string (str)
  (let ((i 0) j ch)
    (while (< i (length str))
      (if (and (> (setq ch (aref str i)) 127)
	       (< ch 160))
	  (progn
	    (setq ch (- ch 128))
	    (insert "\\M-")))
      (if (< ch 32)
	  (cond ((= ch 8)  (insret "\\b"))
		((= ch 9)  (insert "\\t"))
		((= ch 10) (insert "\\n"))
		((= ch 13) (insert "\\r"))
		((= ch 27) (insert "\\e"))
		(t (insert "\\C-" (char-to-string (downcase (+ ch 64))))))
	(if (< ch 127)
	    (if (or (= ch 34) (= ch 92))
		(insert "\\" (char-to-string ch))
	      (setq j i)
	      (while (and (< (setq i (1+ i)) (length str))
			  (>= (setq ch (aref str i)) 32)
			  (/= ch 34) (/= ch 92)
			  (< ch 127)))
	      (insert (substring str j i))
	      (setq i (1- i)))
	  (if (memq ch '(127 255))
	      (insert (format "\\%03o" ch))
	    (insert "\\M-" (char-to-string (- ch 128))))))
      (setq i (1+ i))))
)

(defun MacEdit-lookup-key (map)
  (setq key-str macro-str
	key-symbol (or (lookup-key map key-str)
		       (lookup-key (current-global-map) key-str)))
  (and (integerp key-symbol)
       (setq key-str (substring macro-str 0 key-symbol)
	     key-symbol (or (lookup-key map key-str)
			    (lookup-key (current-global-map) key-str))))
  (and (consp key-symbol)
       (setq key-symbol nil))
  (or key-symbol
      (setq key-str ""))
  (setq key-last (and (> (length key-str) 0)
		      (logand (aref key-str (1- (length key-str))) 127)))
  key-symbol
)

(defun MacEdit-read-argument (&optional obarray pred)   ;; currently ignored
  (let ((str "")
	(min-bsp 0)
	(exec (eq key-symbol 'execute-extended-command))
	str-base)
    (while (progn
	     (MacEdit-lookup-key (current-global-map))
	     (or (and (eq key-symbol 'self-insert-command)
		      (< (length str) 60))
		 (memq key-symbol
			    '(backward-delete-char
			      delete-backward-char
			      backward-delete-char-untabify))
		 (eq key-last 9)))
      (setq macro-str (substring macro-str (length key-str)))
      (or (and (eq key-last 9)
	       obarray
	       (let ((comp (try-completion str obarray pred)))
		 (and (stringp comp)
		      (> (length comp) (length str))
		      (setq str comp))))
	  (if (or (eq key-symbol 'self-insert-command)
		  (and (or (eq key-last 9)
			   (<= (length str) min-bsp))
		       (setq min-bsp (+ (length str) (length key-str)))))
	      (setq str (concat str key-str))
	    (setq str (substring str 0 -1)))))
    (setq str-base str
	  str (concat str key-str)
	  macro-str (substring macro-str (length key-str)))
    (if exec
	(let ((comp (try-completion str-base obarray pred)))
	  (if (if (stringp comp)
		  (and (commandp (intern comp))
		       (setq str-base comp))
		(commandp (intern str-base)))
	      (insert str-base "\n")
	    (insert "execute-extended-command\n")
	    (insert "type \"")
	    (MacEdit-insert-string str)
	    (insert "\"\n")))
      (if (> (length str) 0)
	  (progn
	    (insert "type \"")
	    (MacEdit-insert-string str)
	    (insert "\"\n")))))
)

(defun MacEdit-isearch-argument ()
  (let ((str "")
	(min-bsp 0)
	ch)
    (while (and (setq ch (MacEdit-read-char))
		(or (<= ch 127) (not search-exit-option))
		(not (eq ch search-exit-char))
		(or (eq ch search-repeat-char)
		    (eq ch search-reverse-char)
		    (eq ch search-delete-char)
		    (eq ch search-yank-word-char)
		    (eq ch search-yank-line-char)
		    (eq ch search-quote-char)
		    (eq ch ?\r)
		    (eq ch ?\t)
		    (not search-exit-option)
		    (and (/= ch 127) (>= ch 32))))
      (if (and (eq ch search-quote-char)
	       (MacEdit-peek-char))
	  (setq str (concat str (char-to-string ch)
			    (char-to-string (MacEdit-read-char)))
		min-bsp (length str))
	(if (or (and (< ch 127) (>= ch 32))
		(eq ch search-yank-word-char)
		(eq ch search-yank-line-char)
		(and (or (not (eq ch search-delete-char))
			 (<= (length str) min-bsp))
		     (setq min-bsp (1+ (length str)))))
	    (setq str (concat str (char-to-string ch)))
	  (setq str (substring str 0 -1)))))
    (if (eq ch search-exit-char)
	(if (= (length str) 0)  ;; non-incremental search
	    (progn
	      (setq str (concat str (char-to-string ch)))
	      (and (eq (MacEdit-peek-char) ?\C-w)
		   (progn
		     (setq str (concat str "\C-w"))
		     (MacEdit-read-char)))
	      (if (> (length str) 0)
		  (progn
		    (insert "type \"")
		    (MacEdit-insert-string str)
		    (insert "\"\n")))
	      (MacEdit-read-argument)
	      (setq str "")))
      (MacEdit-unread-chars ch))
    (if (> (length str) 0)
	(progn
	  (insert "type \"")
	  (MacEdit-insert-string str)
	  (insert "\"\n"))))
)

;;; Get the next keystroke-sequence from the input stream.
;;; Sets key-symbol, key-str, and key-last as a side effect.
(defun MacEdit-read-key ()
  (MacEdit-lookup-key (current-local-map))
  (and key-symbol
       (setq macro-str (substring macro-str (length key-str))))
)

(defun MacEdit-peek-char ()
  (and (> (length macro-str) 0)
       (aref macro-str 0))
)

(defun MacEdit-read-char ()
  (and (> (length macro-str) 0)
       (prog1
	   (aref macro-str 0)
	 (setq macro-str (substring macro-str 1))))
)

(defun MacEdit-unread-chars (chars)
  (and (integerp chars)
       (setq chars (char-to-string chars)))
  (and chars
       (setq macro-str (concat chars macro-str)))
)

(defun MacEdit-dump (mac)
  (set-mark-command nil)
  (insert "\n\n")
  (MacEdit-print-macro mac (current-local-map))
)



;;; Parse a keyboard macro description in MacEdit-print-macro's format.

(defun MacEdit-read-macro (&optional map)
  (or map (setq map (current-local-map)))
  (let ((macro-str ""))
    (while (not (progn
		  (skip-chars-forward " \t\n")
		  (eobp)))
      (cond ((looking-at "#"))   ;; comment
	    ((looking-at "prefix-arg[ \t]+-[ \t]+\n")
	     (MacEdit-append-chars "\C-u-"))
	    ((looking-at "prefix-arg[ \t]+\\(-?[0-9]+\\)[ \t]+\n")
	     (MacEdit-append-chars (concat "\C-u" (MacEdit-match-string 1))))
	    ((looking-at "prefix-arg[ \t]+(\\([0-9]+\\))[ \t]+\n")
	     (let ((val (string-to-int (MacEdit-match-string 1))))
	       (while (/= val 0)
		 (or (and (> val 0) (= (% val 4) 0))
		     (error "Bad prefix argument value"))
		 (MacEdit-append-chars "\C-u")
		 (setq val (/ val 4)))))
	    ((looking-at "prefix-arg")
	     (error "Bad prefix argument syntax"))
	    ((looking-at "insert ")
	     (forward-char 7)
	     (MacEdit-append-chars (read (current-buffer)))
	     (if (< (current-column) 7)
		 (forward-line -1)))
	    ((looking-at "type ")
	     (forward-char 5)
	     (MacEdit-append-chars (read (current-buffer)))
	     (if (< (current-column) 5)
		 (forward-line -1)))
	    ((looking-at "\\([-a-zA-z0-9_]+\\)[ \t]*\\(.*\\)\n")
	     (let* ((func (intern (MacEdit-match-string 1)))
		    (arg (MacEdit-match-string 2))
		    (cust (get func 'MacEdit-read)))
	       (if cust
		   (funcall cust arg)
		 (or (commandp func)
		     (error "Not an Emacs command"))
		 (or (equal arg "")
		     (string-match "\\`#" arg)
		     (error "Unexpected argument to command"))
		 (let ((keys
			(or (where-is-internal func map t)
			    (where-is-internal func (current-global-map) t))))
		   (if keys
		       (MacEdit-append-chars keys)
		     (MacEdit-append-chars (concat "\ex"
						   (symbol-name func)
						   "\n")))))))
	    (t (error "Syntax error")))
      (forward-line 1))
    macro-str)
)

(defun MacEdit-append-chars (chars)
  (setq macro-str (concat macro-str chars))
)

(defun MacEdit-match-string (n)
  (if (match-beginning n)
      (buffer-substring (match-beginning n) (match-end n))
    "")
)



(defun MacEdit-get-interactive (func)
  (if (symbolp func)
      (let ((cust (get func 'MacEdit-interactive)))
	(if cust
	    cust
	  (MacEdit-get-interactive (symbol-function func))))
    (or (and (eq (car-safe func) 'lambda)
	     (let ((int (if (consp (nth 2 func))
			    (nth 2 func)
			  (nth 3 func))))
	       (and (eq (car-safe int) 'interactive)
		    (stringp (nth 1 int))
		    (nth 1 int))))
	""))
)

(put 'search-forward           'MacEdit-interactive "s")
(put 'search-backward          'MacEdit-interactive "s")
(put 'word-search-forward      'MacEdit-interactive "s")
(put 'word-search-backward     'MacEdit-interactive "s")
(put 're-search-forward        'MacEdit-interactive "s")
(put 're-search-backward       'MacEdit-interactive "s")
(put 'switch-to-buffer         'MacEdit-interactive "B")
(put 'kill-buffer              'MacEdit-interactive "B")
(put 'rename-buffer            'MacEdit-interactive "B\nB")
(put 'goto-char                'MacEdit-interactive "N")
(put 'global-set-key           'MacEdit-interactive "k\nC")
(put 'global-unset-key         'MacEdit-interactive "k")
(put 'local-set-key            'MacEdit-interactive "k\nC")
(put 'local-unset-key          'MacEdit-interactive "k")

;;; Think about kbd-macro-query



;;; Edit a keyboard macro in another buffer.
;;; (Prefix argument is currently ignored.)

(defun MacEdit-edit-macro (mac repl &optional prefix buffer hook arg)
  (or (stringp mac)
      (error "Not a keyboard macro"))
  (let ((oldbuf (current-buffer))
	(local (current-local-map))
	(buf (get-buffer-create (or buffer "*Edit Macro*"))))
    (set-buffer buf)
    (kill-all-local-variables)
    (use-local-map MacEdit-mode-map)
    (setq buffer-read-only nil)
    (setq major-mode 'MacEdit-mode)
    (setq mode-name "Edit Macro")
    (make-local-variable 'MacEdit-original-buffer)
    (setq MacEdit-original-buffer oldbuf)
    (make-local-variable 'MacEdit-replace-function)
    (setq MacEdit-replace-function repl)
    (make-local-variable 'MacEdit-replace-argument)
    (setq MacEdit-replace-argument arg)
    (make-local-variable 'MacEdit-finish-hook)
    (setq MacEdit-finish-hook hook)
    (erase-buffer)
    (insert "# Keyboard Macro Editor.  Press C-c C-c to finish; press C-x k RET to cancel.\n")
    (insert "# Original keys: " (key-description mac) "\n\n")
    (message "Formatting keyboard macro...")
    (MacEdit-print-macro mac local)
    (switch-to-buffer buf)
    (goto-char (point-min))
    (forward-line 3)
    (recenter '(4))
    (set-buffer-modified-p nil)
    (message "Formatting keyboard macro...done")
    (run-hooks 'MacEdit-format-hook))
)

(defun MacEdit-finish-edit ()
  (interactive)
  (or (and (boundp 'MacEdit-original-buffer)
	   (boundp 'MacEdit-replace-function)
	   (boundp 'MacEdit-replace-argument)
	   (boundp 'MacEdit-finish-hook)
	   (eq major-mode 'MacEdit-mode))
      (error "This command is valid only in buffers created by edit-kbd-macro."))
  (let ((buf (current-buffer))
	(str (buffer-string))
	(func MacEdit-replace-function)
	(arg MacEdit-replace-argument)
	(hook MacEdit-finish-hook))
    (goto-char (point-min))
    (run-hooks 'MacEdit-compile-hook)
    (and (buffer-modified-p)
	 func
	 (progn
	   (message "Compiling keyboard macro...")
	   (let ((mac (MacEdit-read-macro
		       (and (buffer-name MacEdit-original-buffer)
			    (save-excursion
			      (set-buffer MacEdit-original-buffer)
			      (current-local-map))))))
	     (and (buffer-name MacEdit-original-buffer)
		  (switch-to-buffer MacEdit-original-buffer))
	     (funcall func mac arg))
	   (message "Compiling keyboard macro...done")))
    (kill-buffer buf)
    (if hook
	(funcall hook arg)))
)

(defun MacEdit-mode ()
  "Keyboard Macro Editing mode.  Press C-c C-c to save and exit.
To abort the edit, just kill this buffer with C-x k."
  (interactive)
  (error "This mode can be enabled only by edit-kbd-macro or edit-last-kbd-macro.")
)
(put 'MacEdit-mode 'mode-class 'special)

(defvar MacEdit-mode-map nil)
(if MacEdit-mode-map
    ()
  (setq MacEdit-mode-map (make-sparse-keymap))
  (define-key MacEdit-mode-map "\C-c\C-c" 'MacEdit-finish-edit)
)

