; -*- Encoding: utf-8 -*-
;;======================================================================
;;   emacs (not Xemacs) mode
;;
;; To use this file, add lines similar to the following to ~/.emacs.d/init.el:
;;
;;  (setq emacs-git "d:/git/dov/dov-env/emacs")
;;  (setq default-notes-file "w:/users/Dov/git/xjet-git/notes/notes.org")
;;  (load (concat emacs-git "/dov.emacs"))
;;  (setenv "PATH" (concat (getenv "PATH") ";D:\\DevTools\\git\\bin"))
;;----------------------------------------------------------------------

(if (string-match "mingw-nt" system-configuration)
    (progn
      (if (not (boundp 'emacs-git))
          (setq emacs-git "c:/users/dov/emacs"))
      (if (not (boundp 'emacs-persistance-dir))
          (setq emacs-persistance-dir "c:/Document and Settings/dovg"))
;      (set-default-font "-*-Lucida Console-*-*-*-*-15-*-*-*-*-*-*")
      (set-default-font "-*-DejaVu Sans Mono-normal-r-normal-normal-14-*-*-*-*-*-iso10646-1")
      (setq browse-url-generic-program "c:/Program Files (x86)/Mozilla Firefox/firefox.exe")
      (setq my-default-family "DejaVu Sans Mono")

      ;; don't use Hebrew locale!
      (setq system-time-locale "C")

      ;; Load windows utilities
      (load (concat emacs-git "win-utils.el")))
  (progn
;    (setq my-default-family "Liberation Mono")
    (setq my-default-family "InconsolataDov")
    (setq explicit-shell-file-name "/bin/zsh")
    (setq browse-url-generic-program "firefox")
    (if (not (boundp 'emacs-git))
        (setq emacs-git "/home/dov/.config/emacs"))
    (if (not (boundp 'emacs-persistance-dir))
        (setq emacs-persistance-dir "/home/dov/.emacs.d"))
    (if (not (boundp 'my-default-font))
        (setq my-default-font "Liberation Mono 8"))

    ;; Add conversion scripts to path
    (setenv "PATH" (concat emacs-git "scripts:" (getenv "PATH")))

    (condition-case err
     (set-default-font my-default-font)
;    (set-default-font "Consolas 12") 
;     (set-default-font "lucidasanstypewriter-bold-14")
;     (set-default-font "lucidasanstypewriter-bold-12")
;       (set-default-font "Bitstream Vera Sans Mono-11")

     (error "No such font, but who cares"))

    ; Use Miriam mono font for Hebrew
    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "Miriam Mono CLM:bold")

    (setq load-path (append (list
                             "/usr/local/share/emacs/site-lisp/vm"
                             ) load-path))
;    (load "vm")
    (if (and (getenv "HOSTNAME") (string-match "orbotech.com" (getenv "HOSTNAME")))
        (setq add-log-mailing-address "dov@orbotech.com")
      (setq add-log-mailing-address "dov.grobgeld@gmail.com"))
  
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it
          smtpmail-starttls-credentials '(("pod51014.outlook.com" 587 nil nil))
          smtpmail-auth-credentials (expand-file-name "~/.authinfo")
          smtpmail-default-smtp-server "pod51014.outlook.com"
          smtpmail-smtp-server "pod51014.outlook.com"
          smtpmail-smtp-service 587
          smtpmail-debug-info t
          user-mail-address "dov.grobgeld@xjetsolar.com")
    (require 'smtpmail))
  )
                  
(setq load-path (append
                 (list
                  (concat emacs-git "/org-mode/lisp")
                  (concat emacs-git "/org-mode/contrib/lisp")
                  emacs-git
                  )
                 load-path))

;; Emacs 24 support
(when (>= emacs-major-version 24)
  ; Hebrew support
  (setq-default bidi-display-reordering t)
  (setq x-select-enable-primary t)
  (setq x-select-enable-clipboard nil)
  (setq custom-theme-directory (concat emacs-git "themes")))
  
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
(load "js2-mode.el")
(load "scott.emacs")
(load "sgml-mode")
(load "doc-mode")
(load "csharp-mode-0.4.0")
(load "octave-mod")
(load "vc-ediff")
(load "magit")
(load "magit-blame")
(load "markdown-mode")
(setq magit-diff-options '("-w"))
(load "mo-git-blame")
(load "xmsi-math-symbols-input.el")
;(global-set-key [?\C-c ?g ?c] 'mo-git-blame-current)
;(global-set-key [?\C-c ?g ?f] 'mo-git-blame-file)

;; The following hack from http://code.google.com/p/js2-mode/issues/detail?id=50#c7
;; causes js2-mode not to display errors for json files.
(defadvice js2-reparse (before json)
	(setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)

(defadvice js2-parse-statement (around json)
	(if (and (= tt js2-LC)
			js2-buffer-file-name
			(string-equal (substring js2-buffer-file-name -5) ".json")
			(eq (+ (save-excursion
						(goto-char (point-min))
						(back-to-indentation)
						(while (eolp)
							(next-line)
							(back-to-indentation))
						(point)) 1) js2-ts-cursor))
		(setq ad-return-value (js2-parse-assign-expr))
		ad-do-it))
(ad-activate 'js2-parse-statement)

(add-to-list 'load-path (concat emacs-git "/pde"))
;(when (>= emacs-major-version 24)
;  (load "pde-load")
;  ; pde turns this on, which I don't like
;  (ido-mode nil))

(global-set-key "\C-ci" 'magit-status)
(global-set-key "\C-c\C-b" 'magit-blame-mode)

(load "epresent.el")
(load "compile.el")
(setq compilation-scroll-output 'first-error)

; Encryption
(require 'epa-file)
(epa-file-enable)

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
(load "sourcepair")

(define-key global-map "\C-xc" 'sourcepair-load)


(autoload 'octave-help "octave-hlp" nil t)
(load-library "matlab-load")

(require 'color-moccur)
;; TeX
;(load "tex-mode")
;(setq tex-command "xelatex")
;(setq TeX-engine 'xetex)
;(setq TeX-PDF-mode-t)
;(defun tex-view ()
;  (interactive)
;  (tex-send-command "xpdf" (tex-append tex-print-file ".pdf")))


(add-hook 'LaTeX-mode-hook 
   (lambda()
     (%)
     (setq TeX-auto-save t)
     (setq LaTeX-command-style 
    (quote (("\\`fontspec\\'" "xelatex ") 
     ("" "%(PDF)%(latex) %S%(PDFout)"))))
     (custom-set-variables
      '(preview-fast-dvips-command "pdftops -origpagesizes %s.pdf %m/preview.ps"))
     (setq TeX-save-query nil)
     (setq TeX-parse-self t)
     (setq-default TeX-master nil)
     (setq TeX-output-view-style
    (cons '("^pdf$" "." "evince  %o ") TeX-output-view-style))
     (set-default 'preview-default-document-pt 12)
     (set-default 'preview-scale-function 1.2)
     (setq preview-required-option-list 
    (quote ("active" "tightpage" "auctex" "pdftex" (preview-preserve-counters "counters"))))
     (setq preview-default-option-list 
    (quote ("displaymath" "floats" "graphics" "textmath" "showlabels" "sections" )))
     (TeX-global-PDF-mode t)))

;(add-hook 'LaTeX-mode-hook #'my-latex-mode-hook)
;(defun my-latex-mode-hook ()
;  (add-to-list 'TeX-command-list
;               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

;(defun my-latex-mode-hook ()
;  (add-to-list 'TeX-command-list
;               '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;  (setq TeX-command-default
;        (save-excursion
;          (save-restriction
;            (widen)
;            (goto-char (point-min))
;            (let ((re (concat "^\\s-*\\\\usepackage\\(?:\\[.*\\]\\)?"
;                              "{.*\\<\\(?:font\\|math\\)spec\\>.*}")))
;              (if (re-search-forward re 3000 t)
;                  "XeLaTeX"
;                "LaTeX"))))))
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
(def-dmacro-function pwdleaf() (basename (substring (pwd) 0 -1)))
(def-dmacro-function datestring() (format-time-string "%A %Y-%m-%d %R"))
(def-dmacro-function pwdleaf-spacefill ()
  (substring
   (concat
    (basename (substring (pwd) 0 -1))
    "                               ")
   0 20))
;; A version of (file-name) that space fills the result
(def-dmacro-function buffername-spacefill ()
  (substring
   (concat
    (replace-regexp-in-string "\\.\\w+$" "" (buffer-name))
    "                               ")
   0 20))
(global-set-key "\C-cm" 'insert-dmacro)

(setq auto-dmacro-alist '())
(setq auto-dmacro-alist (append '(("\\.h$" . dot-h)
				  ("\\.H$" . dot-h)
				  ("\\.cpp$" . dot-cpp)
				  ("SConstruct" . sconstruct)
				  ("SConscript" . sconscript))
				  auto-dmacro-alist))

(setq gdb-command-name "/usr/bin/gdb")

;; org-mode
(defun kill-visual-line ()
  "Redefined to do kill-line as I believe that lines breaks are for display only!"
  (interactive)
  (kill-line))
(load "screenshot.el")
(load "org-man.el")
(load "org-git-hyperlink.el")
(load "org-wp.el")
(load "org-bullets.el")
(load "ox.el")
(require 'load-theme-buffer-local)

(require 'org)
(require 'org-crypt)
(defun my-org-hook ()
  (local-set-key [(control c) (control ?.)] 'org-time-stamp)
  (local-set-key "\M-I" 'org-toggle-iimage-in-org)
  (local-set-key "\C-c\M-c" 'org-screenshot)
  (local-set-key "\C-c\C-pe" 'org-toggle-emphasis-markers)
  (local-set-key "\C-c\C-pp" 'org-toggle-pretty-entities)
  (local-set-key "\C-c\C-pi" 'org-toggle-iimage-inorg)
  (variable-pitch-mode t)
  (set-face-attribute 'org-table nil :family my-default-family)
  (set-face-attribute 'org-checkbox nil :family my-default-family)
  (set-face-attribute 'org-block nil :family my-default-family)
  (set-face-attribute 'org-verbatim nil :family my-default-family :foreground "green4")
  (org-bullets-mode)
  (setq org-bullets-bullet-list
        '("▸"
          "•"
          "•"
          "•"
          "•"
          "•"
          "•"
          ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
          ;;; Small
          ;; ► • ★ ▸
    ))

  (setq org-hide-emphasis-markers nil)
  (setq org-confirm-babel-evaluate nil)
  (xmsi-mode)
  (org-toggle-pretty-entities)
  (setq bidi-paragraph-direction nil)
  (setq org-export-html-postamble nil)
  (setq org-export-html-validation-link "")
  ;; Use journal theme if requested
  (if (>= emacs-major-version 24)
      (if (string-match "notes.org" (buffer-name) )
          (progn
            (disable-theme 'org-default)
            (load-theme-buffer-local 'org-journal))
        (load-theme-buffer-local 'org-default)))
  (setq org-entities-user '(
    ("models" "\\models" t "&8872;" "[models]" "models" "⊨")
    ("indf" "{\bf 1}" t "&#120128;" "[indf]" "indf" "𝟙")
    ("ell" "\\ell" t "&#2113;" "[ell]" "indf" "ℓ")
    ))

  )
(add-hook 'org-mode-hook 'my-org-hook)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key "C1CC1169")

;; Make all font-lock faces fonts use inconsolata
(dolist (face '(font-lock-builtin-face 	
                font-lock-comment-delimiter-face
                font-lock-comment-face 	
                font-lock-constant-face
                font-lock-doc-face 	
                font-lock-function-name-face
                font-lock-keyword-face 	
                font-lock-negation-char-face
                font-lock-preprocessor-face 	
                font-lock-regexp-grouping-backslash
                font-lock-regexp-grouping-construct 	
                font-lock-string-face
                font-lock-type-face 	
                font-lock-variable-name-face
                font-lock-warning-face))
  (set-face-attribute face nil :family my-default-family))

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if iimage-mode
      (set-face-underline-p 'org-link t)
      (set-face-underline-p 'org-link nil))
  (iimage-mode))

(defun org-toggle-emphasis-markers ()
  "Toggle the emphasis of markers"
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (font-lock-fontify-buffer))
  
(defun org-screenshot ()
  "Insert a screenshot in org-mode"
  (interactive)
  (call-interactively 'screenshot)
  (iimage-recenter nil))

(defun cpp-get-class-name ()
  """Parses the buffer to extract the current class name for C and H files"""
  (interactive)
  (save-excursion
    (if (string-match "\\.h$" (buffer-name))
        (if (re-search-backward "^class")
            (progn 
              (forward-word)
              (forward-word)
              (backward-word)
              )
          (error "No match!"))
      (if (string-match "\\.\\(cc\\|cpp\\)$" (buffer-name))
          ;; Search for constructor
          (progn
            (if (not (re-search-backward "^\\(\\(const *\\)?[a-zA-Z_][a-zA-Z0-9_]*\\)? *[a-zA-Z][a-zA-Z0-9_]*::"))
                (error "No match!"))
            (re-search-forward "::")
            (backward-word))
      (error "Unsupported file type")))
    (message (thing-at-point 'symbol))
    ))

;; XJet Rcomponent conversion functions
(defun rcomp-map (conversion)
  """A dispatcher for running rcomponent.pl"""
  (let* ((class-name (cpp-get-class-name))
         (start-reg (region-beginning))
         (end-reg (region-end))
         (selection (buffer-substring-no-properties start-reg end-reg))
         (cmd (format "rcomponent.pl -ClassName %s -%s" class-name conversion)))
    (shell-command-on-region start-reg end-reg cmd t t)
    (message cmd)))
  
(defun rcomp-def-to-init ()
  (interactive)
  (rcomp-map "def2init"))

(defun rcomp-h-to-def ()
  (interactive)
  (rcomp-map "h2def"))

(defun rcomp-def-to-c ()
  (interactive)
  (rcomp-map "def2c"))

(defun rcomp-h-to-c ()
  (interactive)
  (rcomp-map "h2c"))

(defun rcomp-set ()
  (interactive)
  (rcomp-map "set"))

(require 'iimage)
(add-to-list 'iimage-mode-image-regex-alist
             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                           "\\)\\]")  1))
(load "org-mediawiki.el")

;; Python use python-mode
(setq ipython-command "ipython")
(require 'ipython)
;(setq py-python-command-args '("-pylab" "-p" "pylab" "-colors" "LightBG"))
(setq py-python-command "python")
(setq py-python-command-args nil)

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
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(add-hook 'vala-mode-hook
          (lambda ()
            (define-key c-mode-map [(return)] 'newline-and-indent)))

; Example of how to read argument from minibuffer or use default params.
(defun my-message (&optional arg)
  (interactive
   (list (if current-prefix-arg
             (read-from-minibuffer "MyPrompt: ")
           nil)))
  (if arg
      (message arg)
    (message "NO ARG")))

;(global-set-key "\C-c\C-t" 'my-message)

(defun my-compile-dwim-run (&optional arg)
  (interactive "P")
  (if arg
    (call-interactively 'compile-dwim-run)
    (compile-dwim-run "foo")))

;;; Why can't I get this to run automatically in the perl-mode hook
(defun my-perl-mode-hook ()
  (interactive)
  (define-key perl-mode-map [(control c) (control r)] 'compile-dwim-run)
  (define-key perl-mode-map [(control c) (control s)] 'compile-dwim-compile)
  (define-key perl-mode-map ";" 'self-insert-command)
  (define-key perl-mode-map " " 'self-insert-command)
  (define-key perl-mode-map "{" 'self-insert-command)
  (setq-default abbrev-mode nil)
  (setq cperl-auto-newline nil)
  (setq-default cperl-auto-newline nil)
  (setq-default cperl-indent-level 4)
  (setq cperl-indent-parens-as-block t) 
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-hairy nil)
  (setq cperl-mode-abbrev-table nil)
  
  (abbrev-mode 0)
  (define-key perl-mode-map [(return)] 'newline-and-indent)
  (message "my-perl-mode-hook")
  )

;(add-hook 'pde-hook 'my-perl-mode-hook)
(add-hook 'cperl-mode-hook 'my-perl-mode-hook)
  
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
       (list (cons "\\.js$" 'js2-mode))
       (list (cons "\\.java$" 'java-mode))
       (list (cons "\\.w$" 'cweb-mode))
       (list (cons "\\.mp$" 'metapost-mode))
       (list (cons "\\.mf$" 'metafont-mode))
       (list (cons "\\.cmake$" 'cmake-mode))
       (list (cons "SConstruct" 'python-mode))
       (list (cons "SConscript" 'python-mode))
       (list (cons "\\.md$" 'markdown-mode))
       (list (cons "\\.py$" 'python-mode))
       (list (cons "ChangeLog" 'change-log-mode))
       (list (cons "\\.gob$" 'c++-mode))
       (list (cons "\\.hh$" 'c++-mode))
       (list (cons "\\.H$" 'c++-mode))
       (list (cons "\\.cxx$" 'c++-mode))
       (list (cons "\\.cu$" 'c++-mode))
       (list (cons "\\.vala$" 'vala-mode))
       (list (cons "\\.json$" 'js2-mode))
       (list (cons "\\.pov$"  'pov-mode))
       (list (cons "\\.inc$"  'pov-mode))
       (list (cons "\\.tcl$"  'tcl-mode))
       (list (cons "\\.doc$"  'doc-mode))
       (list (cons "\\.adc$"  'doc-mode))
       (list (cons "\\.m$" 'octave-mode)) 
       (list (cons "\\.xml$" 'xml-mode)) 
       (list (cons "notes.txt" 'mediawiki-mode)) 
       (list (cons "\\.txt$" 'org-mode)) 
       (list (cons "\\.org" 'org-mode)) 
       (list (cons "\\.pl" 'cperl-mode)) 
       (list (cons "\\.nxc$" 'c++-mode)) 
       auto-mode-alist))

;; macros for nxc code
(defun nxt-run ()
  (interactive)
  (save-buffer)
  (let* ((cmd (format "nbc %s -sm- -r -S=usb " (buffer-name))))
    (shell-command cmd)))

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
   (dot . t)
   (asymptote . t)
   (plantuml . t)
   (octave . t)
   (R . t)
   (C . t)
   )) 
(setq org-plantuml-jar-path
      (concat emacs-git "/org-mode/scripts/plantuml.jar"))
(load "org-exp-blocks")
(load "org-mw")
(defun my-org-confirm-babel-evaluate (lang body)
  (not
   (or (string= lang "ditaa")
       (string= lang "dot"))))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Shell colors
(setq ansi-color-names-vector
      '("white" "red" "green" "yellow" "blue" "magenta" "cyan" "black"))


;; Got the follownig from: http://eschulte.github.com/babel-dev/DONE-In-buffer-graphical-results.html
(defun my-iimage-mode-buffer (arg &optional refresh)
"Display/undisplay images.
With numeric ARG, display the images if and only if ARG is positive."
  (interactive)
  (let ((ing (if (numberp arg)
                 (> arg 0)
               iimage-mode))
        (modp (buffer-modified-p (current-buffer)))
        file img)
    (save-excursion
      (goto-char (point-min))
      (dolist (pair iimage-mode-image-regex-alist)
        (while (re-search-forward (car pair) nil t)
          (if (and (setq file (match-string (cdr pair)))
                   (setq file (locate-file file
                                   (cons default-directory
                                         iimage-mode-image-search-path))))
              (if ing
                  (let ((img (create-image file)))
                    (add-text-properties (match-beginning 0) (match-end 0) (list 'display img))
                    (if refresh (image-refresh img)))
                (remove-text-properties (match-beginning 0) (match-end 0) '(display)))))))
    (set-buffer-modified-p modp)))

(defun my-org-iimage-refresh ()
  (interactive)
  (redisplay t)
  (set-face-underline-p 'org-link nil)
  (my-iimage-mode-buffer 1 'refresh)
  (redisplay t))

(add-hook 'org-babel-after-execute-hook 'my-org-iimage-refresh)

; Choose applications to open external files .

(if (string-match "mingw-nt" system-configuration)
    (progn
      (setq org-file-apps
            (append
             '(("png" . "c:/progra~2/IrfanView/i_view32.exe %s"))
             '(("doc" . "\"c:/Program Files (x86)/OpenOffice.org 3/program/soffice.exe\" %s"))

             org-file-apps
             ))
      )
  (progn 
    (setq org-file-apps
          (append
           '(("png" . "eog %s"))
           '(("pdf" . "evince %s"))
           '(("svg" . "inkscape %s"))
           '(("net" . "/usr/local/samiam/runsamiam %s"))
           '(("xcf" . "gimp %s"))
           '(("giv" . "giv %s"))
           '(("doc" . "libreoffice -norestore %s"))
           '(("odt" . "libreoffice -norestore %s"))
           '(("gnumeric" . "gnumeric %s"))
           '(("html" . "firefox %s"))
           org-file-apps))))

(setq org-src-lang-modes
      '(("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . fundamental)
        ("perl" . cperl)
        ("python" . python)
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

(defun perl-execute-buffer()
  "Execute perl on the region or the buffer"
  (interactive)
  (shell-command-on-region
    (min (point) (mark)) (max (point) (mark)) "perl "))

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

(defun date-stamp-full-here ()
  "Insert a date and time stamp"
  (interactive)
  (insert (format-time-string "%A %Y-%m-%d %R ")))

(defun date-stamp-here ()
  "Insert a date and time stamp"
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a")))

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

(defun open-notes-file ()
  "Load my personal todo list"
  (interactive)
  (find-file default-notes-file)
  (font-lock-fontify-buffer)
  (end-of-buffer)
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

(defun current-filename-to-clip-buffer ()
  "Copy the current buffer file name to the clip buffer"
  (interactive)
  (kill-new buffer-file-name)
  (message "%s" buffer-file-name))

;; Hide unregistered files from vc-buffer.
;; Copied from: http://groups.google.com/group/gnu.emacs.bug/msg/4a58d078b4aae650
(defun my-vc-dir-hide-some (states)
  "Hide files whose state is in STATES."
  (interactive
   (list
    (progn
      (unless vc-ewoc
        (error "Not in a vc-dir buffer"))
      (mapcar 'intern
              (completing-read-multiple
               "Hide files that are in state(s): "
               (let (possible-states)
                 (ewoc-map (lambda (item)
                             (let ((state (vc-dir-fileinfo->state item)))
                               (when state
                                 (pushnew state possible-states))
                               nil))
                           vc-ewoc)
                 (mapcar 'symbol-name possible-states))
               nil t)))))
  (let ((inhibit-read-only t))
    (ewoc-filter vc-ewoc
                 (lambda (file)
                   (not (memq (vc-dir-fileinfo->state file) states))))))
(eval-after-load "vc-dir"
  '(define-key vc-dir-mode-map "H" 'my-vc-dir-hide-some))

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
(global-set-key (kbd "A-C-f") 'current-filename-to-clip-buffer)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

;; Find first and return first buffer matching a given pattern
(defun find-first-buffer-match (buffers pattern)
  (let ((f (car buffers)))
    (cond ((eq f '()) nil)
          ((string-match pattern (buffer-name f)) f)
          (t (find-first-buffer-match (cdr buffers) pattern)))))

(defun find-most-recent-pattern-buffer (pattern)
  "find the most recent code buffer in the history and switch to it"
  (let ((f (find-first-buffer-match (cdr (buffer-list)) pattern)))
    (if (not (eq f nil))
        (switch-to-buffer f))))

(defun find-most-recent-python-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.py"))

(defun find-most-recent-c-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.\\(cpp\\|h\\|cc\\|hh\\)$"))

(defun find-most-recent-emacs-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.el\\$\\|dov.emacs"))

(defun find-most-recent-magit-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "magit"))

(defun find-most-recent-org-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.org\$"))

;; git find file
(load "git-find-file.el")
(global-set-key [(control c) ?g] 'git-find-file)

;; git grep
(load "dov-git-grep")
(global-set-key [(control c) ?s] 'dov-git-grep)


;; Shortcuts to go to special buffers
(global-set-key [(alt meta d)] 'goto-end-of-gud-buffer)
(global-set-key [(alt meta c) ?c] 'find-most-recent-c-buffer)
(global-set-key [(alt meta c) ?p] 'find-most-recent-python-buffer)
(global-set-key [(control c) ?b ?c] 'find-most-recent-c-buffer)
(global-set-key [(control c) ?b ?e] 'find-most-recent-emacs-buffer)
(global-set-key [(control c) ?b ?p] 'find-most-recent-python-buffer)
(global-set-key [(control c) ?b ?m] 'find-most-recent-magit-buffer)
(global-set-key [(control c) ?b ?o] 'find-most-recent-org-buffer)
(global-set-key [(alt meta m)] 'find-most-recent-magit-buffer)
(global-set-key [(alt meta y)] 'find-most-recent-python-buffer)
(global-set-key [(alt meta n)] '(lambda () (interactive) 
  (switch-to-buffer "notes.org")))
(global-set-key [(alt meta h)] '(lambda () (interactive) 
  (switch-to-buffer "*shell*")
  ; prepare for user input
  (end-of-buffer)))
(global-set-key [(alt meta o)] '(lambda () (interactive) 
  (switch-to-buffer "*Inferior Octave*")
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
(global-set-key [(control c) (control \;)] 'date-stamp-full-here)
(global-set-key [(control \:)] 'insert-my-name)
(global-set-key [(alt m)] 'point-to-register)
(global-set-key [(alt g)] 'jump-to-register)
(global-set-key [(control x)(control y)] 'yank)
(global-set-key "\C-xv=" 'vc-ediff)
(global-set-key [(control ?=)] 'apply-macro-to-region-lines)
(global-set-key [(control ?8)] 'call-last-kbd-macro)
(global-set-key [(control ?9)] 'start-kbd-macro)
(global-set-key [(control ?0)] 'end-kbd-macro)
(global-set-key [(find)] 'toolbar-mail)
(global-set-key [(meta \`)] 'next-error)
(global-set-key [(meta \~)] '(lambda () (interactive) (next-error -1)))
(global-set-key [(control meta up)] '(lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(control meta down)] '(lambda () (interactive) (scroll-other-window -1)))
(global-set-key [(meta prior)] '(lambda () (interactive) (scroll-other-window-down nil)))
(global-set-key [(meta next)] '(lambda () (interactive) (scroll-other-window nil)))
(global-set-key [f5] 'open-notes-file)

(define-key global-map " " 'space-or-undo)
(define-key global-map "\C-x\C-m" 'save-buffers-dont-ask)
(define-key c++-mode-map [(control c) (control e)] 'compile)
(define-key lisp-mode-map [return] 'newline-and-indent)
(define-key emacs-lisp-mode-map [return] 'newline-and-indent)
;(define-key tex-mode-map [return] 'newline-and-indent)
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

; icicle completion does not work for 
(if (< emacs-major-version 24)
    (define-key minibuffer-local-completion-map [tab] 'icicle-prefix-complete))
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

; This currently doesn't work!
(when (eq emacs-major-version 999923)
  ;;; Cedet - Note! Run make in cedet file!
  (load-file (concat emacs-git "/cedet/common/cedet.el"))
  (global-ede-mode t)
  (require 'cedet/semantic/sb)
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
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
    )

  (add-hook 'c-mode-common-hook 'my-cedet-hook))

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

;; json/javascript indent
(setq javascript-indent-level 2)

(defun update-indent-mode ()
  (setq c-basic-offset my-indent)
  (c-set-offset 'substatement my-substatement)
  (c-set-offset 'substatement-open my-substatement-open)
  (c-set-offset 'access-label my-access-label)
  (c-set-offset 'topmost-intro my-topmost-intro))
  
;; Default indent mode for home projects
(setq my-indent 2)
(setq my-substatement 4)
(setq my-substatement-open 4)
(setq my-access-label 0)
(setq my-topmost-intro 0)
(update-indent-mode)

(defun orbo-indent-mode ()
  "Set indent tabs to 4 as is standard at Orbotech."
  (interactive)
  (setq my-indent 4)
  (setq my-topmost-intro 0)
  (update-indent-mode))

(defun hadassa-indent-mode ()
  "Set indent tabs to 4 as style I use at WIS."
  (interactive)
  (setq my-indent 4)
  (setq my-topmost-intro 0)
  (update-indent-mode))

(defun xjet-indent-mode ()
  "Set indent tabs to the xjet indent mode"
  (interactive)
  ;; C++-python
  (setq my-indent 2)
  (setq my-substatement 2)
  (setq my-substatement-open 0)
  (setq my-access-label 0)
  (setq my-topmost-intro 0)
  (update-indent-mode)

  ;; Python
  (setq py-indent-offset 2)
  )

(defun gnu-indent-mode ()
  "Set indent tabs to 2 as is standard by gnome."
  (interactive)
  (setq my-indent 2)
  (setq my-substatement 2)
  (setq my-substatement-open 2)
  (setq my-access-label 0)
  (setq my-topmost-intro 0)
  (update-indent-mode))

(defun qt-indent-mode ()
  "Set indent tabs to 2 as is standard by gnome."
  "qt sources indent mode"
  (interactive)
  (setq my-indent 4)
  (setq my-substatement 4)
  (setq my-substatement-open 0)
  (setq my-access-label -4)
  (setq my-topmost-intro 0)
  (setq c-recognize-knr-p nil)
  (update-indent-mode))

(defun outline-keys (map) ""
  (define-key map [(control kp-subtract)] 'hide-subtree)
  (define-key map [(control kp-add)] 'show-subtree)
  (define-key map [(alt kp-add)] 'show-all)
  (define-key map [(alt kp-subtract)] 'hide-sublevels)
  (define-key map [(control up)] 'scroll-up-line)
  (define-key map [(control down)] 'scroll-down-line)
  )

(defun find-dov-env ()
  "Edit this file"
  (interactive)
  (find-file (concat emacs-git "/dov.emacs")))

(defun my-cmode-stuff (map) ""
  (update-indent-mode)

  (setq indent-tabs-mode nil)
  (define-key map [return] 'newline-and-indent)
  (define-key map [(control c) (control e)] 'compile)
  (define-key map (kbd "C-?") 'c-comment-selection-or-word)
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
(add-hook 'js2-mode-hook (lambda() (do-return-indent js2-mode-map)))

;(add-hook 'py-mode-hook '(lambda() 
;                           (define-key py-mode-map [(control m)] 'py-newline-and-indent)
;                           ))
(add-hook 'python-mode-hook '(lambda() 
                               (local-set-key (kbd "RET") 'py-newline-and-indent)
                               (remove-dos-eol)
                               (setq py-indent-offset my-indent)
                               (setq python-indent my-indent)
                               (remove-dos-eol)))
(add-hook 'diff-mode-hook '(lambda() 
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

(defun c-comment-selection-or-word ()
  "Put a c comment around the selection or the current word"
  (interactive)
    (if mark-active
        (save-excursion
          (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))
            (delete-region (region-beginning) (region-end))
            (insert "/*")
            (insert selection)
            (insert "*/")
            ))
      (progn
        (skip-chars-forward "-_A-Za-z0-9")
        (insert "*/")
        (backward-char)
        (backward-char)
        (skip-chars-backward "-_A-Za-z0-9")
        (insert "/*")
        (forward-word)
        (forward-word)
        (forward-word)
        (backward-word)
        )
    ))

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
    (define-key comint-mode-map [(tab)] 'comint-dynamic-complete)

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
;; The following is based on:
;; http://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))

(add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)
                              
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
;;  (define-key map [µ] 'gdb-finish)
  (define-key map [(alt f)] 'gdb-finish)
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

;; Emacs customization - this might be overwritten in the .emacs file

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

(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(org-emphasis-alist
   (quote
    (
     ("*" bold "<b>" "</b>")
     ("/" italic "<i>" "</i>")
     ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
     ("=" org-code "<code>" "</code>" verbatim)
     ("~" org-verbatim "<code>" "</code>" verbatim))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
