; -*- Encoding: utf-8 -*-
;;======================================================================
;;   emacs (not Xemacs) mode
;;----------------------------------------------------------------------

(if (string-match "mingw-nt" system-configuration)
    (progn
      (setq emacs-git "c:/users/dov/emacs")
      (setq emacs-persistance-dir "c:/Document and Settings/dovg")
;      (set-default-font "-*-Lucida Console-*-*-*-*-15-*-*-*-*-*-*")
      (set-default-font "-*-DejaVu Sans Mono-normal-r-normal-normal-14-*-*-*-*-*-iso10646-1")
      (setq browse-url-generic-program "c:/Program Files (x86)/Mozilla Firefox/firefox.exe")

      ;; don't use Hebrew locale!
      (setq system-time-locale "C")
      )
  (progn
    (setq browse-url-generic-program "firefox")
    (setq emacs-git "/home/dov/.config/emacs")
    (setq emacs-persistance-dir "/home/dov/.emacs.d")
    (condition-case err
     (set-default-font "Liberation Mono 8")
;    (set-default-font "Consolas 12") 
;     (set-default-font "lucidasanstypewriter-bold-14")
;     (set-default-font "lucidasanstypewriter-bold-12")
;       (set-default-font "Bitstream Vera Sans Mono-11")

     (error "No such font, but who cares"))
    (setq load-path (append (list
                             "/usr/local/share/emacs/site-lisp/vm"
                             ) load-path))
;    (load "vm")
    (if (and (getenv "HOSTNAME") (string-match "orbotech.com" (getenv "HOSTNAME")))
        (setq add-log-mailing-address "dov@orbotech.com")
      (setq add-log-mailing-address "dov.grobgeld@gmail.com"))))
                  
(setq load-path (append
                 (list
                  (concat emacs-git "/org-mode")
                  emacs-git
                  )
                 load-path))

(defconst inhibit-startup-message t)

(menu-bar-mode 't)
(tool-bar-mode 'nil)

;; Get newer private versions of standard libraries
;(load "cmake-mode")
(load "cc-mode")
(load "vc")
(load "gdb-libtool")
(load "gtk-look")
(load "icicles")
;(icy-mode)
;(load "icicles-xmas")
;(load "icicles-menu-xmas")
;(load "vala-mode")
(load "javascript.el")
(load "scott.emacs")
(load "sgml-mode")
(load "doc-mode")
(load "csharp-mode-0.4.0")
(load "octave-mod")
(load "vc-ediff")
(load "magit")
(load "epresent.el")

; centos doesn't support a lot of things
(if (eq (string-match "21.4.1" emacs-version) nil)
    (progn
      (load "ack")

      ;; Doxygen stuff
      (load "doxymacs")
      (defun my-doxymacs-font-lock-hook ()
	(if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
	    (doxymacs-font-lock)))
))
(load "python-mode")
(load "mediawiki")
(load "dired+")
(load "dired-details")
(load "dired-details+")
(load "csv-mode")

(autoload 'octave-help "octave-hlp" nil t)
(load-library "matlab-load")

(require 'color-moccur)
;; TeX
(load "tex-mode")
(load "octave-mod")

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

;; Text mode stuff
(add-hook 'text-mode-hook 'visual-line-mode)

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; DMacro
(load "dmacro")
(load "dired")  ;; Since I am using a dired function below
(dmacro-load (concat emacs-git "/dov.dmacro"))
(def-dmacro-function pwdleaf () (basename (substring (pwd) 20 -1)))
(global-set-key "\C-cm" 'insert-dmacro)
(global-set-key "\C-ci" 'magit-status)

(setq auto-dmacro-alist (append '(("\\.h$" . dot-h)
				  ("\\.H$" . dot-h)
				  ("SConstruct" . sconstruct)
				  ("SConscript" . sconscript))
				  auto-dmacro-alist))

(setq gdb-command-name "/usr/bin/gdb")

;; org-mode
(require 'org-install)
(defun my-org-hook ()
  (local-set-key [(control c) (control ?.)] 'org-time-stamp)
  )
(add-hook 'org-mode-hook 'my-org-hook)

;; Python use python-mode
(setq ipython-command "ipython")
(require 'ipython)
;(setq py-python-command-args '("-pylab" "-p" "pylab" "-colors" "LightBG"))
(setq py-python-command-args '("-pylab" "-colors" "LightBG"))

; ediff options
(setq ediff-patch-options "")

(setq mode-compile-expert-p t)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)

;;set up persistent.el
;;to remember histories across emacs sessions
(setq persistent-session-list `(read-expression-history
                                extended-command-history
                                find-tag-history
                                query-replace-history
                                grep-history
                                file-name-history
                                compile-history
                                kill-ring
                                replace-string-history
                                replace-regex-history
                                query-replace-regex-history
                                minibuffer-history
                                shell-command-history
                                buffer-name-history
                                find-file-history
                                ))

;; Other

(require 'persistent)
(persistent-session-load-from-alist)
(setq persistent-session-size 1000)

(require 'compile)
(setq compilation-error-regexp-alist
  (append (list
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))

(add-hook 'java-mode-hook 'font-lock-mode)
(add-hook 'sgml-mode-hook 'font-lock-mode)
(add-hook 'c-mode-common-hook 'font-lock-mode)
(add-hook 'cperl-mode-hook 'font-lock-mode)
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(add-hook 'vala-mode-hook
          (lambda ()
            (define-key c-mode-map [(return)] 'newline-and-indent)))
(add-hook 'cperl-mode-hook
          (lambda ()
            (define-key cperl-mode-map [(return)] 'newline-and-indent)))

(autoload 'vala-mode "vala-mode.el" "Valamode" t)
(autoload 'pov-mode "pov-mode.el" "PoVray scene file mode" t)
(autoload 'sather-mode "sather.el" "Sather mode" t nil)
(autoload 'cweb-mode "cweb.el" "CWeb mode" t nil)

;; Set some auto mode
(setq auto-mode-alist
      (append
       (list (cons "\\.sa$" 'sather-mode))
       (list (cons "\\.cs$" 'csharp-mode))
       (list (cons "\\.csv$" 'csv-mode))
       (list (cons "\\.js$" 'javascript-mode))
       (list (cons "\\.java$" 'java-mode))
       (list (cons "\\.w$" 'cweb-mode))
       (list (cons "\\.mp$" 'metapost-mode))
       (list (cons "\\.mf$" 'metafont-mode))
       (list (cons "\\.cmake$" 'cmake-mode))
       (list (cons "SConstruct" 'python-mode))
       (list (cons "SConscript" 'python-mode))
       (list (cons "\\.py$" 'python-mode))
       (list (cons "ChangeLog" 'change-log-mode))
       (list (cons "\\.gob$" 'c++-mode))
       (list (cons "\\.hh$" 'c++-mode))
       (list (cons "\\.H$" 'c++-mode))
       (list (cons "\\.cxx$" 'c++-mode))
       (list (cons "\\.cu$" 'c++-mode))
       (list (cons "\\.vala$" 'vala-mode))
       (list (cons "\\.pov$"  'pov-mode))
       (list (cons "\\.inc$"  'pov-mode))
       (list (cons "\\.tcl$"  'tcl-mode))
       (list (cons "\\.doc$"  'doc-mode))
       (list (cons "\\.adc$"  'doc-mode))
       (list (cons "\\.m$" 'matlab-mode)) 
       (list (cons "\\.xml$" 'xml-mode)) 
       (list (cons "notes.txt" 'mediawiki-mode)) 
       (list (cons "\\.txt$" 'org-mode)) 
       (list (cons "\\.org" 'org-mode)) 
       auto-mode-alist))

;; mapping between languages and their major mode  (in Emacs)
;(setq org-export-htmlized-org-css-url "/home/dov/tmp/org-mode/ORGWEBPAGE/org.css")
;(load "htmlize")
(load "org-s5")
(load "org-htmlslidy")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (perl . t)
   (emacs-lisp . t)
   (python . t)
   (ditaa . t)
   ))
(load "org-exp-blocks")

; Choose applications to open external files .

(if (string-match "mingw-nt" system-configuration)
    (progn
      (setq org-file-apps
            (append
             '(("png" . "c:/progra~2/IrfanView/i_view32.exe %s"))
             org-file-apps))
      )
  (progn 
    (setq org-file-apps
          (append
           '(("png" . "eog %s"))
           '(("pdf" . "evince %s"))
           org-file-apps))))

(setq org-src-lang-modes
      '(("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . fundamental)
        ("perl" . cperl)
        ("pyhon" . python)
        ))
;; )

;; Don't jump when curser reaches end of terminal.
(setq scroll-conservatively 10000)

;; octave make ret do indent
(defun RET-behaves-as-LFD ()
  (let ((x (key-binding "\C-j")))
    (local-set-key "\C-m" x)))
(add-hook 'octave-mode-hook 'RET-behaves-as-LFD)

;; D-language support
(autoload 'd-mode "d-mode" 
  "Major mode for editing D code." t)
(setq auto-mode-alist (cons '( "\\.d\\'" . d-mode ) auto-mode-alist ))
(autoload 'dlint-minor-mode "dlint" nil t)
(add-hook 'd-mode-hook (lambda () (dlint-minor-mode 1)))

(setq sendmail-program "/home/dov/scripts/gmail-sendmail")
(setq lpr-command "poe")
(setq mc-pgp-user-id "dov@menora")

;; other misc setup
(setq dabbrev-case-fold-search nil) ; make dabbrev case sensitive

;; Override the stupid timestamp of the psgml mode!
(defun html-helper-default-insert-timestamp nil)

(defun my-gud-cont-to-here ()
  "Put a breakpoint at the current line and cont. Then erase the break point. The approach here is very simplistic in that it doesn't know how to deal with embedded break point. It will run to the first break point it finds."
  (interactive)
  (gud-break 1)
  (let ((p (point)))
    (gud-cont 1)
    (goto-char p))

  (gud-remove (point)))


(setq sgml-markup-faces
      '((comment font-lock-comment-face)
	(start-tag font-lock-keyword-face)
	(end-tag font-lock-keyword-face)))

(setq sgml-public-map
      '("%S" "/data/alg/local/sgml/%o/%c/%d"))

(setq sgml-catalog-files
      '("CATALOG" "/data/alg/local/sgml/CATALOG"))

(setq-default cperl-indent-level 4)
(setq cperl-indent-parens-as-block t) 
(setq diff-switches "-w")
(setq vc-diff-switches "-w -c")
(setq tex-dvi-view-command "xdvi")
(setq tex-dvi-print-command "dvipslprint")
(setq next-line-add-newlines nil)

;; Load ange-ftp
;(require 'ange-ftp)
;(setq ange-ftp-gateway-host nil)
;(setq ange-ftp-local-host-regexp "^[^.]*$")
;(setq ange-ftp-smart-gateway nil)

;; Set options for VM
(setq vm-spool-files (list "/home/dov/Mail/mbox"))
(setq vm-included-text-prefix "> ")
(setq vm-included-text-attribution-format "In your message, %F, from %y-%02M-%02d, you wrote:\n")
(setq vm-display-using-mime t)
(setq vm-mime-max-message-size 100000000)
(setq mime-editor/split-message nil)
(setq vm-reply-ignored-addresses '("^dov@orbotechcom.com"
                                   "[ \<]dov@orbotech.com"))

;; Some functions defined by me
(defun perl-pe-region()
  "Run a perl -pe 'expr' on the region and put it into the buffer.
   It works, but it is slow..."
   (interactive)
   (shell-command-on-region
    (min (point) (mark)) (max (point) (mark))
    (concat "perl -pe '"
	    (read-shell-command "perl -pe command: ")
	    "'")
    t
    t
    )
)

(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function 'browse-url-generic)
(defun run-eperl-maybe-and-preview-html()
  "Runs eperl for phtml files and then runs the preview."
  (interactive)
  (if (string-match "\.phtml" buffer-file-name)
      (let ((htmlfilename (concat (file-name-sans-extension buffer-file-name) ".html")))
	(if (shell-command (concat "eperl " buffer-file-name "> " htmlfilename))
	    (browse-url-of-file htmlfilename)))
    (browse-url-of-file))
)
(defun run-asciidoc-maybe-and-preview-html()
  "Runs asciidoc for txt files and then runs the preview."
  (interactive)
  (if (string-match "\.txt" buffer-file-name)
      (let ((htmlfilename (concat (file-name-sans-extension buffer-file-name) ".html")))
	(if (shell-command (concat "asciidoc " buffer-file-name "> " htmlfilename))
	    (browse-url-of-file htmlfilename)))
    (browse-url-of-file))
)

(setq-default dired-no-confirm
	      '(byte-compile chgrp chmod chown compress copy delete
			     load move shell symlink uncompress
                             revert-subdirs))

(defun date-stamp-here ()
  "Insert a date and time stamp"
  (interactive)
  (insert (format-time-string "%A %Y-%m-%d %R ")))

(defun insert-my-name ()
  "Insert my name"
  (interactive)
  (insert "Dov Grobgeld <dov.grobgeld@gmail.com>"))


(defun html-date-line ()
  "Insert a html <hr> and a datestamp"
  (interactive)
  (newline)
  (insert "<p>")
  (newline)
  (insert "<!------------------------------>")
  (newline)
  (insert "<hr>")
  (newline)
  (insert (format-time-string "%a %b"))
  (save-excursion
    (insert (format-time-string "%e %Y, %R:%S ")))
  (just-one-space)
;;  (shell-command "ds" 1)
  (end-of-line)
  (newline)
  (insert "<p>")
  (newline)
  (insert "<h2>")
  (save-excursion
    (insert "</h2>"))
  )

(defun newline-and-indent-relative()
  "Do a newline and a relative indent."
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun copy-line-to-next()
  "Copy current line to next line."
  (interactive)
  (beginning-of-line)
  (kill-line 1)
  (yank)
  (yank)
  (previous-line 1)
  )

(defun last-element (l)
  (cond
   ((eq (cdr l) '()) (car l))
   ((last-element (cdr l)))))
 
(defun unbury-buffer ()
  "put last buffer in the buffer list to the current window"
  (interactive)
  (switch-to-buffer (last-element (buffer-list))))
 
(defun space-or-undo ()
  "if last command was UNDO, continues undoing, else insert SPACE"
  (interactive)
  (cond
   ((eq last-command 'undo)
    (let ((modified (buffer-modified-p)))
     (setq this-command 'undo)
     (undo-more 1)
     (and modified (not (buffer-modified-p))
	  (delete-auto-save-file-if-necessary))))
   (t (self-insert-command 1))))

(defun save-buffers-dont-ask ()
  "save all buffers with out asking for confirmation"
  (interactive)
  (save-some-buffers 1)
  (message "saved"))

;; Some utility functions for quickly getting to my todo list
(defun todo ()
  "Load my personal todo list"
  (interactive)
  (find-file "~/light/log-book.html")
  (font-lock-fontify-buffer)
)
(defun view-todo ()
  "View my personal todo list"
  (interactive)
  (w3-open-local "~/html/todo.html")
)

(defun move-to-first-window-line()
  (interactive)
  (move-to-window-line 0))

(defun move-to-last-window-line()
  (interactive)
;  (move-to-window-line (- (window-height) 2)))
  (move-to-window-line (- (window-height) 2)))

(defun move-to-middle-window-line()
  (interactive)
  (move-to-window-line (/ (- (window-height) 2)2)))


(defun scroll-dont-move-cursor (dist)
  ""
  (let ((p (point)))
    (scroll-up dist)
    (goto-char p)))
  
(defun scroll-up-line ()
  (interactive)
  (scroll-dont-move-cursor 1))

(defun scroll-down-line ()
  (interactive)
  (scroll-dont-move-cursor -1))

(defun goto-end-of-gud-buffer ()
  (interactive) 
  (switch-to-buffer gud-comint-buffer)
  ; prepare for user input
  (end-of-buffer))

(global-set-key "\M-[" 'find-matching-keyword)
(global-set-key "\M-]" 'c-beginning-of-defun)
(global-set-key [(control ?') ?'] 'find-matching-keyword)
(global-set-key [(control ?') (control a)] 'save-buffer)
(global-set-key [(control ?.)] 'dabbrev-expand)
(global-set-key [(control ?,)] 'undo)
(global-set-key " " 'space-or-undo)
(global-set-key "\C-x\C-m" 'save-buffers-dont-ask)
(global-set-key [(control h) (control j)] 'gtk-lookup-symbol)
(global-set-key [(control h) (control q)] 'qtdoc-lookup)
(global-set-key [(f2)] 'perl-pe-region)
(global-set-key [(control f27)] 'move-to-first-window-line)
(global-set-key [(super f27)] 'move-to-first-window-line)
(global-set-key [(control home)] 'move-to-first-window-line)
(global-set-key [(control kp-home)] 'move-to-first-window-line)
(global-set-key [(control kp-begin)] 'move-to-middle-window-line)
(global-set-key [(super home)] 'move-to-first-window-line)
(global-set-key [(control f33)] 'move-to-last-window-line)
(global-set-key [(super f33)] 'move-to-last-window-line)
(global-set-key [(control end)] 'move-to-last-window-line)
(global-set-key [(control kp-end)] 'move-to-last-window-line)
(global-set-key [(super end)] 'move-to-last-window-line)
(global-set-key [(super kp-insert)] 'move-to-middle-window-line)
(global-set-key [(begin)] 'move-to-middle-window-line)
(global-set-key [(super kp-add)] 'scroll-up-line)
(global-set-key [(button5)] '(lambda () (interactive) (scroll-up 5)))
(global-set-key [(button4)] '(lambda () (interactive) (scroll-down 5)))
(global-set-key [(shift button5)] '(lambda () (interactive) (scroll-up 1)))
(global-set-key [(shift button4)] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [(control button5)] '(lambda () (interactive) (scroll-up)))
(global-set-key [(control button4)] '(lambda () (interactive) (scroll-down)))
(global-set-key [(super kp-enter)] 'scroll-down-line)
;(global-set-key [(control f31)] 'move-to-middle-window-line)
;(global-set-key [f31] 'recenter)
(global-set-key "\C-x\C-k" 'kill-compilation)
(global-set-key [(alt d)] 'goto-end-of-gud-buffer)

;; Shortcuts to go to special buffers
(global-set-key [(alt meta d)] 'goto-end-of-gud-buffer)
(global-set-key [(alt meta h)] '(lambda () (interactive) 
  (switch-to-buffer "*shell*")
  ; prepare for user input
  (end-of-buffer)))
(global-set-key [(alt meta m)] '(lambda () (interactive) 
  (switch-to-buffer "*MATLAB*")
  ; prepare for user input
  (end-of-buffer)))
(global-set-key [(alt meta s)] '(lambda () (interactive) 
  (switch-to-buffer "*scratch*")))

(global-set-key [(control up)] 'scroll-up-line)
(global-set-key [(control kp-up)] 'scroll-up-line)
(global-set-key [(super up)] 'scroll-up-line)
(global-set-key [(control down)] 'scroll-down-line)
(global-set-key [(control kp-down)] 'scroll-down-line)
(global-set-key [(super down)] 'scroll-down-line)
(global-set-key [f27] 'beginning-of-line)
(global-set-key [f29] 'scroll-down)
(global-set-key [f33] 'end-of-line)
(global-set-key [f35] 'scroll-up)
(global-set-key [(control left)] 'backward-word)
(global-set-key [(control kp-left)] 'backward-word)
(global-set-key [(super left)] 'backward-word)
(global-set-key [(control right)] 'forward-word)
(global-set-key [(control kp-right)] 'forward-word)
(global-set-key [(super right)] 'forward-word)
(global-set-key [(meta up)] 'previous-multiframe-window)
(global-set-key [(meta down)] 'next-multiframe-window)
(global-set-key [(alt up)] 'previous-multiframe-window)
(global-set-key [(alt down)] 'next-multiframe-window)
(global-set-key [(alt q)] 'next-error)
(global-set-key [(meta kp_add)] 'unbury-buffer)
(global-set-key [(meta kp_subtract)] 'bury-buffer)
(global-set-key [UE000] 'unbury-buffer)
(global-set-key [UE001] 'bury-buffer)
(global-set-key [UE002] 'kill-buffer)
(global-set-key [(meta prior)] 'unbury-buffer)
(global-set-key [(meta next)] 'bury-buffer)
(global-set-key [(meta delete)] 'delete-window)
(global-set-key [(meta f27)] 'split-window-vertically)
(global-set-key [(meta f33)] 'delete-window)
(global-set-key [(meta f29)] 'split-window-horizontally)
;(global-set-key [C-f29] 'beginning-of-buffer)
(global-set-key [(control f29)] 'beginning-of-buffer)
(global-set-key [(super f29)] 'beginning-of-buffer)
(global-set-key [(control kp-prior)] 'beginning-of-buffer)
(global-set-key [(super pgup)] 'beginning-of-buffer)
(global-set-key [(control f35)] 'end-of-buffer)
(global-set-key [(super f35)] 'end-of-buffer)
(global-set-key [(control pgdn)] 'end-of-buffer)
(global-set-key [(control kp-next)] 'end-of-buffer)
(global-set-key "\C-x3" 'split-window-horizontally)
(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [(meta backspace)] 'backward-kill-word)
(global-set-key [delete] 'delete-char)
(global-set-key "\C-c\C-e" 'compile)
(global-set-key [(control ?') (control e)] 'compile)
;(global-set-key [(control j)] 'isearch-forward)
(global-set-key "\C-xw" 'write-region)
(global-set-key "\C-x\C-r" 'revert-buffer)
(global-set-key [(control return)] 'call-last-kbd-macro)
(global-set-key [(alt tab)] 'indent-relative)
(global-set-key [(hyper tab)] 'indent-relative)
(global-set-key [(alt /)] 'dabbrev-expand)
(global-set-key [(control /)] 'dabbrev-expand)
(global-set-key [(control \;)] 'date-stamp-here)
(global-set-key [(control \:)] 'insert-my-name)
(global-set-key [(alt m)] 'point-to-register)
(global-set-key [(alt g)] 'jump-to-register)
(global-set-key [(control x)(control y)] 'yank)
(global-set-key "\C-xv=" 'vc-ediff)
(global-set-key [(control ?=)] 'apply-macro-to-region-lines)
(global-set-key [(control ?9)] 'start-kbd-macro)
(global-set-key [(control ?0)] 'end-kbd-macro)
(global-set-key [(find)] 'toolbar-mail)
(global-set-key [(meta \`)] 'next-error)
(global-set-key [(control meta up)] '(lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(control meta down)] '(lambda () (interactive) (scroll-other-window -1)))
(global-set-key [(meta prior)] '(lambda () (interactive) (scroll-other-window-down nil)))
(global-set-key [(meta next)] '(lambda () (interactive) (scroll-other-window nil)))
(global-set-key [f5] 'todo)
(global-set-key [(control f5)] 'view-todo)
(global-set-key [(control shift u)] 'ucs-insert)

(define-key global-map " " 'space-or-undo)
(define-key global-map "\C-x\C-m" 'save-buffers-dont-ask)
(define-key c++-mode-map [(control c) (control e)] 'compile)
(define-key lisp-mode-map [return] 'newline-and-indent)
(define-key emacs-lisp-mode-map [return] 'newline-and-indent)
(define-key tex-mode-map [return] 'newline-and-indent)
(global-set-key [delete] 'delete-backward-char)
(global-set-key [(control delete)] 'backward-kill-word)
(global-set-key [home] 'beginning-of-line)
(global-set-key [prior] 'scroll-down)
(global-set-key [end] 'end-of-line)
(global-set-key [next] 'scroll-up)
(global-set-key [(control prior)] 'beginning-of-buffer)
(global-set-key [(control next)] 'end-of-buffer)
(global-set-key [(control kp_5)] 'move-to-middle-window-line)
(global-set-key [kp_5] 'recenter)
(define-key sgml-mode-map [(meta f2)] 'html-date-line)
(define-key minibuffer-local-completion-map [tab] 'icicle-prefix-complete)
(define-key minibuffer-local-completion-map [backtab] 'icicle-apropos-complete)
(global-set-key [(super f8)] 'toggle-decorations)
(global-set-key [(super f7)] 'toggle-toolbar)
(global-set-key [f4] 'copy-line-to-next)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-xk" 'kill-this-buffer)
  
;(define-key text-mode-map [RET] 'newline-and-indent-relative)
(define-key text-mode-map [return] 'newline-and-indent-relative)
(define-key text-mode-map "\C-m" 'newline-and-indent-relative)



;; Perl mode stuff
;(load "cperl-mode")
(setq auto-mode-alist
      (append '(("\\.[pP][Llm]$" . cperl-mode))  auto-mode-alist ))
(setq interpreter-mode-alist (append interpreter-mode-alist
				     '(("miniperl" . perl-mode))))
; (load "perl-mode")   ; old mode

;;; Cedet - Note! Run make in cedet file!
(load-file (concat emacs-git "/cedet/common/cedet.el"))
(global-ede-mode t)
(semantic-load-enable-minimum-features)
(require 'semantic-ia)

;; Support Qt
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_GUI_EXPORT" . ""))
(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_CORE_EXPORT" . ""))

(setq qt4-base-dir "/usr/include")
(setq qt4-gui-dir "/usr/include/QtGui") 
(semantic-add-system-include qt4-base-dir 'c++-mode)
(semantic-add-system-include qt4-gui-dir 'c++-mode) 
(add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
(add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))

(defun my-cedet-hook ()
  (local-set-key "\M-/" 'semantic-ia-complete-symbol)
  (local-set-key [(control ?.)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-?=" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )

(add-hook 'c-mode-common-hook 'my-cedet-hook)

;; qt docs lookup
(load "qtdoc")
(setq qtdoc-html-root "http://doc.qt.nokia.com/4.7")

;; Other customization
(line-number-mode t)
(column-number-mode t)

(autoload 'tmpl-expand-templates-in-buffer "tmpl-minor-mode"
  "Expand all templates in the current buffer." t)

(setq minibuffer-max-depth nil)

;;; Commands added by calc-private-autoloads on Thu Jan  4 09:13:22 1996.
;(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
;(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
;(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
;(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
;(autoload 'defmath		   "calc" nil t t)
;(autoload 'calc			   "calc" "Calculator Mode" t)
;(autoload 'quick-calc		   "calc" "Quick Calculator" t)
;(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
;(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
;(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
;(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
;(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
;(autoload 'edit-kbd-macro	   "macedit" "Edit Keyboard Macro" t)
;(autoload 'edit-last-kbd-macro	   "macedit" "Edit Keyboard Macro" t)
;(autoload 'read-kbd-macro	   "macedit" "Read Keyboard Macro" t)
;(setq load-path (append load-path (list "/home/dov/lib/emacs/calc")))
;(global-set-key "\e#" 'calc-dispatch)
;;; End of Calc autoloads.

;; Here is where xemacs adds on its customization options
(put 'narrow-to-region 'disabled nil)

;; More customization
(setq my-indent 2)
(setq my-substatement 4)
(setq my-substatement-open 4)

(defun orbo-indent-mode ()
  "Set indent tabs to 4 as is standard at Orbotech."
  (interactive)
  (setq my-indent 4))

(defun xjet-indent-mode ()
  "Set indent tabs to 4 as is standard at Orbotech."
  (interactive)
  (setq my-indent 2)
  (setq my-substatement 2)
  (setq my-substatement-open 0))

(defun gnu-indent-mode ()
  "Set indent tabs to 2 as is standard by gnome."
  (interactive)
  (setq my-indent 2))

(defun outline-keys (map) ""
  (define-key map [(control kp-subtract)] 'hide-subtree)
  (define-key map [(control kp-add)] 'show-subtree)
  (define-key map [(alt kp-add)] 'show-all)
  (define-key map [(alt kp-subtract)] 'hide-sublevels)
  (define-key map [(control up)] 'scroll-up-line)
  (define-key map [(control down)] 'scroll-down-line)
  )

(defun my-cmode-stuff (map) ""
  (setq c-basic-offset my-indent)
  (c-set-offset 'substatement my-substatement)
  (c-set-offset 'substatement-open my-substatement-open)

  (setq indent-tabs-mode nil)
  (define-key map [return] 'newline-and-indent)
  (define-key map [(control c) (control e)] 'compile)
  (outline-minor-mode)
  ; outline key bindings
  (outline-keys map)
  )

(defun my-perlmode-stuff () ""
  (define-key perl-mode-map [return] 'newline-and-indent)
)

(defun do-return-indent (map) ""
  (define-key map [return] 'newline-and-indent)
  (setq indent-tabs-mode nil))

(defun my-change-mode-hook (map) ""
  (setq indent-tabs-mode nil))

;; Control scroll change fonts like in FireFox
(require 'zoom-frm)
(global-set-key (kbd "<C-mouse-5>") 'zoom-in)
(global-set-key (kbd "<C-mouse-4>") 'zoom-out)

(add-hook 'c-mode-hook   (lambda() (my-cmode-stuff c-mode-map)))
(add-hook 'perl-mode-hook (lambda() (my-perlmode-stuff)))
(add-hook 'c++-mode-hook (lambda() (my-cmode-stuff c++-mode-map)))
(add-hook 'tcl-mode-hook (lambda() (do-return-indent tcl-mode-map)))
(add-hook 'py-mode-hook '(lambda() 
                           (define-key py-mode-map [(control m)] 'py-newline-and-indent)
                           (remove-dos-eol)
                           (setq py-indent-offset 2)))
(add-hook 'python-mode-hook '(lambda() 
                               (define-key python-mode-map [return] 'py-newline-and-indent)
                               (remove-dos-eol)))
(add-hook 'csv-mode-hook '(lambda() 
                           (setq truncate-lines t)
                           (setq word-wrap nil)
                           ))
(add-hook 'mediawiki-mode-hook
          '(lambda() 
             (define-key mediawiki-mode-map [(control x) (control s)] 'save-buffer)
             ))

(add-hook 'change-log-mode-hook '(lambda() 
                                   (setq indent-tabs-mode nil)
                                   ))
;(add-hook 'vm-mode-hook  (lambda() (my-vm-bindings vm-mode-map)))

;; override comint-kill-output to have it save the output so it can be
;; yanked.
(defun comint-kill-output-to-kill-ring ()
  "Kills all output from last command and puts it in kill buffer
Does not delete the prompt."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
        (replacement nil)
        (inhibit-read-only t))
    (save-excursion
      (let ((pmark (progn (goto-char (process-mark proc))
                          (forward-line 0)
                          (point-marker))))
        ;; Add the text to the kill ring.
        (copy-region-as-kill comint-last-input-end pmark)
        (delete-region comint-last-input-end pmark)
        (goto-char (process-mark proc))
        (setq replacement (concat "*** output flushed to kill ring ***\n"
                                  (buffer-substring pmark (point))))
        (delete-region pmark (point))))
    ;; Output message and put back prompt
    (comint-output-filter proc replacement)))

(defun toggle-backslash-line ()
  "Toggle all forward slashes to backslashes for the current line."
  (interactive)
  (save-excursion
    (setq myBoundaries (bounds-of-thing-at-point 'line))
    (beginning-of-line)
    (save-restriction
      (narrow-to-region (car myBoundaries) (cdr myBoundaries))
      (if (search-forward "/" nil t)
          (progn
            (beginning-of-line)
            (while (search-forward "/" nil t) (replace-match "\\\\" 'literal)))
        (progn
          (beginning-of-line)
          (while (search-forward "\\" nil t) (replace-match "/")))
        )
      (end-of-line))))

(add-hook 'comint-mode-hook
  (lambda()
    (define-key comint-mode-map [(meta p)] 'comint-previous-matching-input-from-input)
    (define-key comint-mode-map [(meta n)] 'comint-next-matching-input-from-input)
    (define-key comint-mode-map [(control c) (control o)] 'comint-kill-output-to-kill-ring)
    (define-key comint-mode-map [(control x) (control ?\\)] 'toggle-backslash-line)

    ; Save history when the shell is killed
    (make-local-variable 'comint-input-ring-file-name)
    (setq comint-input-ring-file-name (concat emacs-persistance-dir "/comint-history"))
    (setq comint-input-ring-size 10000)
    (setq comint-process-echoes 't)
    (comint-read-input-ring)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'comint-write-input-ring)
  ))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
                              
(add-hook 'gud-mode-hook
  (lambda() 
    (define-key gud-mode-map [(alt n)] 'gud-next)
    (define-key gud-mode-map [(alt s)] 'gud-step)
    (define-key gud-mode-map [(alt u)] 'gud-finish)
;    (define-key gud-mode-map [(alt h)] 'gud-until)
;    (define-key gud-mode-map "\C-i" 'shell-dynamic-complete-filename)
    (make-local-variable 'comint-input-ring-file-name)
    (setq comint-input-ring-file-name (concat emacs-persistance-dir "/gdb-history"))
    (setq comint-input-ring-size 10000)
    (comint-read-input-ring)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'comint-write-input-ring)
    ))

(add-hook 'matlab-shell-mode-hook
  (lambda() 
    (setq comint-input-ring-file-name (concat emacs-persistance-dir "/matlab-history"))
    (setq comint-input-ring-size 5000)
    (define-key matlab-shell-mode-map [up] 'previous-line)
    (define-key matlab-shell-mode-map [down] 'next-line)
    (define-key matlab-shell-mode-map [(control up)] 'scroll-up-line)
    (define-key matlab-shell-mode-map [(control down)] 'scroll-down-line)
    ))

(defun let-aring () (interactive) (insert "å")) 
(defun let-auml () (interactive) (insert "ä"))
(defun let-ouml () (interactive) (insert "ö"))
(defun let-Aring () (interactive) (insert "Å"))
(defun let-Auml () (interactive) (insert "Ä"))
(defun let-Ouml () (interactive) (insert "Ö"))

(defun swedish-keys (map)
  "Create a swedish map"
  (interactive)
  (define-key map [(meta /)] 'let-aring)
  (define-key map [(meta s)] 'let-ouml)
  (define-key map [(meta -)] 'let-auml)
  (define-key map [(meta 63)] 'let-Aring)
  (define-key map [(meta S)] 'let-Ouml)
  (define-key map [(meta _)] 'let-Auml)
) 

;;(swedish-keys global-map)
;(swedish-keys mail-mode-map)
(swedish-keys text-mode-map)
;(swedish-keys tex-mode-map)
;(swedish-keys text-mode-map)

(setq icon-map-list '(x-gtk-stock-map))
(set-scroll-bar-mode 'right)
(setq show-paren-style 'expression)
(setq line-move-visual nil)

(defun gdb-keys (map) 
  "Set key bindings for gdb debugging"
  (interactive)
  (define-key map [(alt n)] 'gdb-next)
  (define-key map [(alt s)] 'gdb-step)
  (define-key map [(control c) (control s)] 'gdb-step)
  (define-key map [(alt u)] 'gdb-finish)
  (define-key map [(alt h)] 'gdb-cont-to)
  (define-key map [(hebrew_finalkaph)] 'gdb-next)
  (define-key map [(hebrew_finalpe)] 'gdb-step)
  (define-key map [(iso-next-group)] nil))

  
;
; Doppke's hack for following the cursor in the compile window
;

(defadvice compile (after put-point-at-end activate)
  "Puts the point at the end of the compilation buffer."
  (let ((win (get-buffer-window "*compilation*"))
	(curwindow (selected-window)))
    (if win
	(progn
	  (select-window win)
	  (goto-char (point-max))
	  (select-window curwindow)))))
(if (featurep 'compile)
    (ad-activate 'compile)
  (if xemacs
      (eval-when (WHEN load) '(ad-activate 'compile))
    (eval-after-load "compile" '(ad-activate 'compile))))

;; Emacs customization

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "MidnightBlue"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "green4"))))
 '(font-mediawiki-bold-face ((((class color) (background light)) (:inherit bold :foreground "Midnight blue"))))
 '(font-mediawiki-italic-face ((((class color) (background light)) (:inherit italic :foreground "Midnightblue"))))
 '(font-mediawiki-sedate-face ((((class color) (background light)) (:foreground "Black" :weight bold))))
 '(show-paren-match ((((class color) (background light)) (:background "#b4eeb4")))))
