; -*- Encoding: utf-8; mode:emacs-lisp -*-
;;======================================================================
;;   emacs (not Xemacs) mode
;;
;; To use this file, add lines similar to the following to ~/.emacs on
;; windows:
;;
;;  (setq emacs-git "c:/users/dovg/git/dov-env/emacs/") ; Need slash
;;  (setq default-notes-file "w:/users/Dov/git/xjet-git/notes/notes.org")
;;  (setq tramp-default-method "plink")
;;  ; Point ediff to the diff path.
;;  (setq ediff-diff-program "c:/Program Files/Git/usr/bin/diff.exe")
;;  (setq ediff-diff3-program "c:/Program Files/Git/usr/bin/diff3.exe")
;;  (setenv "GIT_SSH" "C:/Windows/System32/OpenSSH/ssh.exe")
;;  (setenv "PATH" (concat (getenv "PATH") ";c:/Program Files/Git/usr/bin/"))
;;  (setenv "MJQT" "d:/git/dov/MetalJet/XjetApps/MetalJet/Apps/Project/qt/")
;;  (load (concat emacs-git "dov.emacs"))
;;
;;   Other customization (for windows):
;;      ;; Set initial frame size 
;;      (set-frame-size (selected-frame) 1500 1110 1)
;;
;; Here is a sample linux setup
;;
;; (setenv "MJQT" "/home/dov/git/MetalJet/XjetApps/MetalJet/Apps/Project/qt/")
;; (setenv "PE_HOME" "/home/dov/git/MetalJet/")
;; (setq emacs-git "/home/dov/git/dov-env/emacs/")
;; (load-file (concat emacs-git "dov.emacs"))
;; (setq default-notes-file "/home/dov/org/notebooks-groovy.org")
;; (find-file default-notes-file)
;; (setq my-git-repos (make-hash-table :test 'equal))
;; (puthash "xjet" "~/hd/xjet/MetalJet/" my-git-repos)
;;
;;  More customizations (for non-standard places):
;;    - (setenv "WORKON_HOME" "$HOME/anaconda3/envs/" t)
;;----------------------------------------------------------------------

(require 'cl-macs)

(if (or (string-match "mingw-nt" system-configuration)
        (string-match "i686-pc-mingw32" system-configuration)
        (string-match "x86_64-w64-mingw32" system-configuration))
    (progn
      (if (not (boundp 'emacs-git))
          (setq emacs-git "c:/users/dov/emacs"))
      (setq temp-dir "c:/Temp/")
      (if (not (boundp 'emacs-persistance-dir))
          (setq emacs-persistance-dir "c:/Document and Settings/dovg"))
      (setq browse-url-generic-program "C:\\Program Files\\Mozilla Firefox\\firefox.exe")

      ;; don't use Hebrew locale!
      (setq system-time-locale "C")

      ;; Load windows utilities
      (load (concat emacs-git "win-utils.el")))
    
      ;; Add conversion scripts to path
      (setenv "PATH" (concat emacs-git "\\scripts;" (getenv "PATH")))

  (progn
;    (setq my-fixed-font "Liberation Mono")
    (setq temp-dir "/tmp/")
    (setq explicit-shell-file-name "/bin/zsh")
    (setq browse-url-generic-program "firefox")
    (if (not (boundp 'emacs-git))
        (setq emacs-git "/home/dov/.config/emacs"))
    (if (not (boundp 'emacs-persistance-dir))
        (setq emacs-persistance-dir "/home/dov/.emacs.d"))

    ;; Add conversion scripts to path
    (setenv "PATH" (concat emacs-git "scripts:" (getenv "PATH")))

    ;; Default scripts that are the same across most of my environments
    (if (eq (getenv "WORKON_HOME") nil)
        (setenv "WORKON_HOME" "$HOME/anaconda3/envs" t))

                                        ; Use Miriam mono font for Hebrew
;    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "David CLM")
;    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "Nachlieli CLM")
;     (set-fontset-font "fontset-default" '(#x2b24 . #x2b24) "xft:-PfEd-DejaVu Sans-normal-normal-normal-*-16-*-*-*-*-0-iso10646-1")
;     (set-fontset-font "fontset-default" '(#x25ef . #x25ef) "xft:-PfEd-DejaVu Sans-normal-normal-normal-*-16-*-*-*-*-0-iso10646-1")
;     (set-fontset-font "fontset-default" '(#x600 . #x2fff) "xft:-PfEd-DejaVu Sans-normal-normal-normal-*-28-*-*-*-*-0-iso10646-1")
     (set-fontset-font "fontset-default" '(#x600 . #x2fff) "DejaVu Sans")
    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "Nachlieli CLM")
;    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "Simple CLM")
;    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "Miriam Fixed")
    (ignore-errors
      (set-face-font 'default "fontset-default"))
    (setq load-path (append (list
                             "/usr/local/share/emacs/site-lisp/vm"
                             "/usr/local/share/emacs/site-lisp/rtags/"
			     emacs-git
                             (concat emacs-git "flycheck"))
                            load-path))
;    (load "vm")
    (load "dash")
    (setq add-log-mailing-address "dov.grobgeld@gmail.com")))

;; Add trailing slash to emacs-git
(setq emacs-git (file-name-as-directory emacs-git))

;  add melpa to package list
(setq package-user-dir (concat emacs-git "packages"))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Setup my prefered font. This should work on both linux and
;; windows with and without my prefered fonts installed
(defun get-first-font-from-list (font-name-list)
  "Given a list of font names, return the first that exists"
  (cl-dolist (name font-name-list)
    (if (find-font (font-spec :name name))
        (cl-return name))))

(if (or (not (boundp 'my-variable-font))
        (not (find-font (font-spec :name my-variable-font))))
    (setq my-variable-font (get-first-font-from-list
                            (list
                             "DejaVu Sans"
                             "Arial"))))

(if (or (not (boundp 'my-fixed-font))
        (not (find-font (font-spec :name my-fixed-font))))
    (setq my-fixed-font (get-first-font-from-list
                         (list
                          "InconsolataDov"
                          "Inconsolata"
                          "DejaVu Sans Mono"
                          "Consolas"))))

(if (not (boundp 'my-default-font))
    (setq my-default-font (concat my-fixed-font " 12")))

(setq Info-default-directory-list
      (append (list (concat emacs-git "info"))
              Info-default-directory-list))

;; Font for all frames
(set-frame-font my-fixed-font)
(add-to-list 'default-frame-alist
             (cons 'font my-default-font))

;; Takes care of some failure of autocomplete-mode
(eval-after-load 'auto-complete '(global-auto-complete-mode 1))

;; Always use utf-8
; (setq coding-system-for-read 'utf-8) - Conflicts with zip file moe

(setq load-path (append
                 (list
                  (concat emacs-git "wgrep")
                  (concat emacs-git "ein/lisp")
                  (concat emacs-git "skewer-mode")
                  (concat emacs-git "company")
                  (concat emacs-git "flycheck")
                  (concat emacs-git "multiple-cursors")
                  (concat emacs-git "wat-mode")
                  emacs-git
                  )
                 load-path))

;; Use a visible bell through the ring-bell-function
(setq visible-bell nil
      ring-bell-function (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))


(ignore-errors
  (require 'init-eglot))
(require 'init-compat)
(require 'init-use-package)
(require 'init-org)
(require 'init-multiple-cursors)
(require 'init-helm)
(require 'init-cmake)
(require 'init-default-text-scale)
;(require 'init-telega)
(require 'sticky-w)
(require 'init-transient)
(require 'init-with-editor)
(require 'init-emojify)
(require 'init-anaphora)
(require 'init-markdown)
(require 'init-literate-calc)
(require 'init-polymode)
;(require 'init-ein)
;(require 'init-all-the-icons)
(require 'transient)
(require 'init-magit)
(require 'init-magit-imerge)
(require 'init-lua-mode)
;(require 'init-company-mode)
(require 'init-csv-mode)
(require 'init-bind-key)
(require 'init-doom-themes)
(require 'init-visual-fill-column)
(require 'init-deadgrep)
(require 'init-indent-bars)
(ignore-errors
  (require 'init-skeletor))
(require 'init-copilot)
(require 'init-copilot-chat)

;; Emacs 24 support
(when (>= emacs-major-version 24)
  ; Hebrew support
  (setq-default bidi-display-reordering t)
  (setq x-select-enable-primary t)
  (setq x-select-enable-clipboard t)
  (setq custom-theme-directory (concat emacs-git "themes")))
  
;; terminal support
(unless window-system
  (define-key input-decode-map "[6~" [next])
  (define-key input-decode-map "[5~" [prior])
  (global-set-key "\C-c[" 'find-matching-keyword)

  )

;; The following key should be fixed!
(if window-system
    (global-set-key "\M-[" 'find-matching-keyword))
    
(defconst inhibit-startup-message t)
(prefer-coding-system 'utf-8)

(menu-bar-mode 't)
(tool-bar-mode 'nil)

; New options for emacs 28
(setq use-short-answers t)
(setq next-error-message-highlight t)
(global-set-key (kbd "\C-x t T") 'toggle-frame-tab-bar)
(setq dired-kill-when-opening-new-dired-buffer nil)

;; See if this helps with timeout issue when editing the notes on
;; on a CIFS.

(setq backup-by-copying t)

;; Get newer private versions of standard libraries
(autoload 'rec-mode "rec-mode" nil t)
(autoload 'robot-mode "robot-mode" nil t)
(autoload 'conf-mode "conf-mode" nil t)
;(load "vc")
(load "gdb-libtool")
(autoload 'gtk-lookup-symbol "gtk-look" nil t)
(autoload 'icicle-apropos-complete "icicles" nil t)
(autoload 'icicle-prefix-complete "icicles" nil t)
(autoload 'ps-mode "ps-mode" nil t)
(autoload 'meson-mode "meson-mode" nil t)
(autoload 'pyvenv-workon "pyvenv" nil t)
(autoload 'asy-mode "asy-mode" nil t)

;(icy-mode)
;(load "icicles-xmas")
;(load "icicles-menu-xmas")
;(load "vala-mode")
(autoload 'js2-mode "js2-mode.el" nil t)
;(load "scott.emacs")
(autoload 'find-matching-keyword "scott.emacs.el" nil t)
(autoload 'basename "scott.emacs.el" nil t)
(autoload 'sgml-mode "sgml-mode" nil t)
(autoload 'nsis-mode "nsis-mode" nil t)
(autoload 'qt-pro-mode "qt-pro-mode" nil t)
(autoload 'doc-mode "doc-mode" nil t)
(autoload 'octave-mode "octave" nil t)
(autoload 'vc-ediff "vc-ediff" nil t)
(setq with-editor-file-name-history-exclude 'nil)
;(autoload 'magit-status "magit" "Open a Magit status buffer […]" t nil)
;(load "with-editor")
(load "magit-popup")
(load "ghub")
;(load "magit")
;(condition-case err
;    (progn
;      (require 'magit-gh-pulls)
;      (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
;  (error "magit-gh not available"))
(autoload 'with-editor-file-name-history-exclude "with-editor" "with-editor" t nil)
(autoload 'xjet-remote-python-file "xjet-remote-client" nil t)

(setq git-commit-summary-max-length 80)
(autoload 'magit-blame "magit-blame" nil t)
(setq magit-diff-options '("-w"))
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(autoload 'xmsi-mode "xmsi-math-symbols-input" "Load xmsi minor mode for inputting math (Unicode) symbols." t)
;(load "xml-rpc")
(global-set-key [?\C-c ?j] 'ein:notebooklist-open)  ; j for jupyter

;(require 'org-loaddefs)
(require 'ein-loaddefs)
(require 'wgrep)
(require 'pretty-mode)
(require 'browse-kill-ring)
(require 'pydoc)
(require 'pcre2el)
(require 'visual-regexp-steroids)
(global-set-key "\M-y" 'browse-kill-ring)

;; When you want to add multiple cursors not based on continuous lines, but based on
;; keywords in the buffer, use:

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;(require 'subword)
;(add-hook 'python-mode-hook #'pretty-mode 1)

;; case fold by default
(setq isearch-case-fold-search nil)

;; Don't ask async questions by default

(setq async-shell-command-buffer "rename-buffer")

(defun dov-org-present ()
  """Turn emacs into presentation mode"""
  (interactive)
  (require 'dov-emacs-present)
  (org-present))

(defun pcre-re-search-forward (re)
  """Search forward for a pcre regular expression"""
  (interactive)
  (search-forward-regexp (rxt-pcre-to-elisp re)))

(defun pcre-re-search-backward (re)
  """Search forward for a pcre regular expression"""
  (interactive)
  (search-backward-regexp (rxt-pcre-to-elisp re)))

;; Note that substrings may be extracted with
;; etc (match-string 2 s)
(defun pcre-string-match (re string)
  """Match a string with pcre syntax"""
  (interactive)
  (string-match (pcre-to-elisp re) string))

(defun pcre-keep-lines (re)
  """Search forward for a pcre regular expression"""
  (interactive "MKeep lines containing match for regexp: ")
  (keep-lines (rxt-pcre-to-elisp re)))

(defun pcre-drop-lines (re)
  """Drop all lines matching re"""
  (interactive "MDrop lines containing match for regexp: ")
  (save-excursion
    (while (re-search-forward (rxt-pcre-to-elisp re) nil t)
      (unless (eobp)  ; Check if at the end of buffer
        (beginning-of-line)
        (kill-line)
        (kill-line))))
  (message "Matching lines dropped."))

(defun hide-non-matching-lines (pattern)
  """Make all lines not matching PATTERN invisible."""
  (interactive "sEnter pattern: ")
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((start (point))
            (end (progn (forward-line 1) (point))))
        (goto-char start)
        (unless (re-search-forward (rxt-pcre-to-elisp pattern) end t)
          (put-text-property start end 'invisible t))
        (goto-char end)))))

(defun pcre-occur (re)
  """Show all matching re lines in another buffer"""
  (interactive "MList lines matching regexp: ")
  (occur (rxt-pcre-to-elisp re)))

(defun whitespace-strip (s)
  """Remove all whitespaces in a string"""
  (interactive)
  (replace-regexp-in-string " " "" s))

; Redefine the subword functions so that they work with underscores
(defun subword-forward-fnc ()
  """Search forward for a subword break. This could probably be replaced with a search-forward expression"""
  (setq foundFlag-p nil )
  (setq i (1+ (point)))
  (let ((case-fold-search nil))
    (while
        (and (not foundFlag-p) (< i (buffer-end 1)))
      (setq s (buffer-substring-no-properties i (+ 2 i)))
      (if
       (string-match "\\W\\w\\|_\\w\\|[a-z][A-Z]\\|\\w\\W" s)
          (setq foundFlag-p t)
        (setq i (1+ i))))
    (1+ i)))

(defun subword-forward ()
  (interactive)
  (goto-char (subword-forward-fnc)))

(defun subword-backward-fnc ()
  (setq foundFlag-p nil )
  (setq i (- (point) 2))
  (let ((case-fold-search nil))
    (while
        (and (not foundFlag-p) (> i 0))
      (setq s (buffer-substring-no-properties i (+ 2 i)))
      (if
       (string-match "\\W\\w\\|_\\w\\|[a-z][A-Z]" s)
          (setq foundFlag-p t)
        (setq i (- i 1))))
    (1+ i)))

(defun subword-backward ()
  (interactive)
  (goto-char (subword-backward-fnc)))

(defun subword-kill ()
  "Do the same as `kill-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `kill-word'."
  (interactive)
  (kill-region (point) (subword-forward)))

(defun subword-transpose ()
  "Transpose"
  (interactive)
  (subword-forward)
  (subword-backward)
  (while (string-match "_" (buffer-substring-no-properties (1- (point)) (point)))
    (backward-char))
  (kill-region (point) (subword-forward))
  (subword-backward)
  (while (string-match "_" (buffer-substring-no-properties (1- (point)) (point)))
    (backward-char))
  (yank))

;; Minibuffer commands
(defun mb-expand-tilde-and-copy ()
  "Expand tilde to full path in the minibuffer and copy it to the kill buffer.
Nice for copying"
  (interactive)
;  (goto-char (point-min))
  (call-interactively 'move-beginning-of-line)
  (while (search-forward-regexp "~" nil t)
    (replace-match (getenv "HOME") nil t))
  (call-interactively 'move-beginning-of-line)
  (call-interactively 'kill-line)
  (call-interactively 'minibuffer-keyboard-quit))

(defun get-referenced-filename ()
  "Get the filename of the buffer or the file pointed to by dired"
  (let* ((winbuf (window-buffer (minibuffer-selected-window))))
    (if (eq (buffer-local-value 'major-mode winbuf)
            'dired-mode)
        (save-window-excursion
          (switch-to-buffer winbuf)
          (dired-get-filename))
      (buffer-file-name winbuf))))

(defun shell-quote-argument-maybe (arg)
  "Get the name of the file and quote only if necessary"
  (interactive)
  (if (pcre-string-match "[ \\*\\&]" arg)
      (shell-quote-argument arg)
    arg))

(defun name-of-the-file ()
  "Gets the shell quoted name of the file the current buffer is based on."
  (interactive)
  (insert (shell-quote-argument-maybe (get-referenced-filename))))

(defun name-nondirectory-of-the-file ()
  "Gets the shell quoted name of the file the current buffer is based on."
  (interactive)
  (insert (shell-quote-argument-maybe (file-name-nondirectory (get-referenced-filename)))))

(defun name-of-the-buffer ()
  "Gets the name of current buffer."
  (interactive)
  (insert (buffer-name (window-buffer (minibuffer-selected-window)))))

(defun browse-upstream-url ()
  "For a github cloned repo, this function opens a browser to the github page of the upstream"
  (interactive)
  (let* ((cmd (format "git rev-parse --abbrev-ref --symbolic-full-name '@{u}'"))
         (remote-branch (shell-command-to-string cmd))
         (remote (car (split-string remote-branch "/")))
         (remote-url (shell-command-to-string (format "git config --get remote.%s.url" remote)))
         (https-url
          (string-trim
           (replace-regexp-in-string (pcre-to-elisp "git@github.com:(\\w+)") "https://github.com/\\1" remote-url))))
    (browse-url https-url)))

;; Experiment area for new keybindings
(defun dov-test ()
  "dov-test"
  (interactive)
  (let* ((bounds (bounds-of-date-at-point)))
         (message (format "%s" bounds))))

(global-set-key (kbd "C-c C-M-d") 'dov-test)
;;--------------

(define-key minibuffer-local-map [(control c) (control k)] 'mb-expand-tilde-and-copy)
(define-key minibuffer-local-map (kbd "C-c f") 'name-of-the-file)
(define-key minibuffer-local-map (kbd "C-c b") 'name-nondirectory-of-the-file)
(define-key minibuffer-local-map (kbd "C-c B") 'name-of-the-buffer)

(defun subword-backward-kill ()
  "Do the same as `backward-kill-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-kill-word'."
  (interactive)
  (kill-region (subword-backward-fnc) (point)))

(defun kill-region-or-last-word ()
  "Kill the region if there is a selection otherwise the last word"
  (interactive)
  (if (use-region-p)
    (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w") 'kill-region-or-last-word)

(defun end-of-buffer-beginning-of-line ()
  "Move to the beginning of the line of the last line in the file"
  (interactive)
  (move-bottom)
  (move-to-column 1)
  (move-beginning-of-line nil)
  )
(global-set-key (kbd "C-M->") 'end-of-buffer-beginning-of-line)


;; Example of how to call and external script on a region and replace the output.
;; These kinds of scripts should be placed in the dov-env environment.
(defun camel-case-to-upper (start end)
  "Convert the string s from AbcDef to ABC_DEF notation"
  (interactive "r")
  (if (use-region-p)
      (shell-command-on-region start end "/home/dov/scripts/camel_case_to_upper " (current-buffer) t)))


;(add-hook 'python-mode-hook #'pretty-mode 1)

;(global-set-key [?\C-c ?g ?c] 'mo-git-blame-current)
;(global-set-key [?\C-c ?g ?f] 'mo-git-blame-file)

;; The following hack from http://code.google.com/p/js2-mode/issues/detail?id=50#c7
;; causes js2-mode not to display errors for json files.
(defadvice js2-reparse (before json)
	(defvar js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)

(defvar tt nil)
(defvar js2-buffer-file-name nil)
(defadvice js2-parse-statement (around json)
	(if (and (= tt js2-LC)
			js2-buffer-file-name
                        ;; change this to a list of known json based file types
			(or
                         (string-equal (substring js2-buffer-file-name -5) ".json")
                         (string-equal (substring js2-buffer-file-name -5) ".fern"))
			(eq (+ (save-excursion
						(goto-char (point-min))
						(back-to-indentation)
						(while (eolp)
							(forward-line 1)
							(back-to-indentation))
						(point))
                               1)
                            js2-ts-cursor))
		(setq ad-return-value (js2-parse-assign-expr))
		ad-do-it))
(ad-activate 'js2-parse-statement)

(add-to-list 'load-path (concat emacs-git "pde"))

;(when (>= emacs-major-version 24)
;  (load "pde-load")
;  ; pde turns this on, which I don't like
;  (ido-mode nil))


(defun update-indent-mode ()
  (setq c-basic-offset my-indent)
  (c-set-offset 'substatement my-substatement)
  (c-set-offset 'substatement-open my-substatement-open)
  (c-set-offset 'access-label my-access-label)
  (c-set-offset 'topmost-intro my-topmost-intro)
  (c-set-offset 'topmost-intro-cont '+))
  
(require 'init-cc)
(require 'init-dabbrev)
(require 'init-yassnippet)
(require 'init-copilot)

(global-set-key "\C-ci" nil)
(global-set-key "\C-cii" 'magit-status)
(global-set-key "\C-cif" 'magit-file-popup)
(global-set-key "\C-cib" 'magit-diff-buffer-file-popup)
(global-set-key "\C-ciB" 'magit-blame-popup)
(global-set-key "\C-cid" 'magit-diff-popup)
(global-set-key "\C-c\C-b" 'magit-blame-mode)
(global-set-key "\C-c\C-o" 'org-open-at-point)

(load "compile.el")
(setq compilation-scroll-output 'first-error)

; Encryption
(require 'epa-file)
(epa-file-enable)

(load "sourcepair")
(setq sourcepair-source-path    '( "." "./*" ".." "../*"))
(setq sourcepair-header-path    '( "." "./*" ".." "../*"))
(setq sourcepair-source-extensions (append (list ".cu") sourcepair-source-extensions))

(define-key global-map "\C-xc" 'sourcepair-load)

(autoload 'octave-help "octave-hlp" nil t)
(autoload 'python-mode "python" nil t)
(setq url-user-agent "foo")   ; Needed to prevent mediamode from crashing
(autoload 'mediawiki-mode "mediawiki" nil t)
(autoload 'mediawiki-open "mediawiki" nil t)
(autoload 'mediawiki-site "mediawiki" nil t)
(autoload 'ag "ag" nil t)

;(require 'mediawiki)

;; Emacs users care more for WikEmacs than Wikipedia :-). 
;; In any case, do not forget the slash at the end of the URL.
(setq mediawiki-site-default "xjet")


;(load "dired+")
(load "dired-details")
(load "dired-details+")

;(autoload 'matlab-shell "matlab" "matlab.el" nil t)
;(autoload 'matlab-load "matlab" "matlab.el" nil t)

(require 'color-moccur)
;; TeX
;(load "tex-mode")
;(setq tex-command "xelatex")
;(setq TeX-engine 'xetex)
;(setq TeX-PDF-mode-t)
;(defun tex-view ()
;  (interactive)
;  (tex-send-command "xpdf" (tex-append tex-print-file ".pdf")))

; ignore pdf loader install error
(condition-case err
    (pdf-loader-install)
  (error "pdf-loader-install not available"))

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
     (setq and thisd-option-list 
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

;; Tramp
(require 'tramp)
(setq password-cache-expiry nil)
;(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq tramp-remote-path
      (append (list
               "/data/data/com.termux/files/usr/bin"
               "/data/data/com.termux/files/usr/bin/applets")
              tramp-remote-path))
(add-to-list 'tramp-remote-process-environment "LC_ALL=en_US.UTF-8")
(add-to-list 'tramp-remote-process-environment "LANGUAGE=en_US.UTF-8")


;; Text mode stuff
(add-hook 'text-mode-hook 'visual-line-mode)

(defun remove-dos-eol ()
  "Removes the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; Delayed loading of dmacro
;(autoload 'insert-dmacro "dmacro" "dmacro.el" nil t)

(with-eval-after-load "dmacro"
  ;(load "dired")  ;; Since I am using a dired function below
  (dmacro-load (concat emacs-git "dov.dmacro"))
  (def-dmacro-function pwdleaf() (basename (substring (pwd) 0 -1)))
  (def-dmacro-function datestring() (format-time-string "%A %Y-%m-%d %R"))
  (def-dmacro-function org-date-string() (format-time-string "%Y-%m-%d %a"))
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
     0 20)))

(require 'dmacro)
(global-set-key "\C-cm" 'insert-dmacro)

(setq auto-dmacro-alist '())
(setq auto-dmacro-alist (append '(("\\.h$" . dot-h)
				  ("\\.H$" . dot-h)
				  ("\\.cpp$" . dot-cpp)
				  ("\\.org$" . dot-org)
				  ("SConstruct" . sconstruct)
				  ("SConscript" . sconscript)
				  ("meson.build" . mesonbuild)
                                  ("CMakeLists.txt" . cmakebuild)
                                  )
				  auto-dmacro-alist))

(setq gdb-command-name "/usr/bin/gdb")

(defun my-nxml-mode-hook ()
  (rng-validate-mode 0) 
  (define-key nxml-mode-map (kbd "\C-x[") 'sgml-skip-tag-forward)
  (define-key nxml-mode-map (kbd "\C-x]") 'sgml-skip-tag-backward))

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

(defun kill-visual-line ()
  "Redefined to do kill-line as I believe that lines breaks are for display only!"
  (interactive)
  (kill-line))
(require 'load-theme-buffer-local)

(defun org-show-all ()
  "Make org buffer as literal as possible"
  (interactive)
  (progn
    (org-remove-from-invisibility-spec '(org-link))
    (org-remove-from-invisibility-spec '(org-cwidth))
    (setq org-pretty-entities nil)
    (setq org-hide-emphasis-markers nil)
    (org-restart-font-lock)
    (setq org-descriptive-links nil)))

(defun org-hide-all ()
  "Make buffer as marked up as possible"
  (interactive)
  (progn
    (add-to-invisibility-spec '(org-link))
    (add-to-invisibility-spec '(org-cwidth))
    (setq org-pretty-entities t)
    (setq org-hide-emphasis-markers t)
    (org-restart-font-lock)
    (setq org-descriptive-links t)))

(require 'org-crypt)
(require 'org-habit)

;; Don't ever make Return accept a competion as that may insert
;; something different than what you are typing!
(with-eval-after-load 'company
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<C-return>") 'company-complete-selection))

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
  (set-face-attribute face nil :family my-fixed-font))

(defun refresh-iimages ()
  "Refresh all the iimages"
  (interactive)
  (if iimage-mode
      (progn
        (clear-image-cache nil)
        (message "refreshed images"))))

(defun foo () "foo"
       (interactive)
       (call-interactively 'iimage-mode)
       (message (format "mode is %s" iimage-mode)))

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
         (cmd (format "%s %s/rcomponent.py --classname %s --%s"
                      my-python-interpreter
                      (concat emacs-git "scripts")
                      class-name conversion)))
    (shell-command-on-region start-reg end-reg cmd t t)
    (exchange-point-and-mark)
    (kill-ring-save (point) (mark))
    (message cmd)))
  
(defun testbind ()
  """testin gof different bindings"""
  (interactive)
  (message (format "Is string: %s" (nth 3 (syntax-ppss)))))

(defun is-pos-in-string ()
  """whether the current position is in a string"""
  (interactive)
  (nth 3 (syntax-ppss)))

(defun is-pos-in-comment ()
  """whether the current position is in a string"""
  (interactive)
  (nth 4 (syntax-ppss)))

(defun search-percent ()
  """Search for next % that is not a string"""
  (interactive)
  (point-to-register ?p)
  (forward-char)
  (setq break nil)
  (condition-case nil
      (while (not break)
        (if (re-search-forward "%")
            (progn
              (backward-char)
              (if (or
                   (is-pos-in-string)
                   (is-pos-in-comment))
                  ; Search for next match
                  (forward-char)
                ; we found a match
                (setq break 1)))
          (progn
            (setq break 1)
            (register-to-point ?p)
        (message "else-"))))
    (error
     (register-to-point ?p)
     (message "No match!")
    )))

(defun rcomp-def-to-init ()
  (interactive)
  (rcomp-map "def2init"))

(defun rcomp-yank-to-init ()
  (interactive)
  (set-mark-command nil)  
  (yank)
  (exchange-point-and-mark)
  (setq deactivate-mark nil)
  (rcomp-map "def2init")
  )

(defun rcomp-h-to-def ()
  (interactive)
  (rcomp-map "h2def"))

(defun rcomp-def-to-c ()
  (interactive)
  (rcomp-map "def2c"))

(defun rcomp-def-to-h ()
  (interactive)
  (rcomp-map "def2h"))

(defun rcomp-yank-to-c ()
  (interactive)
  (set-mark-command nil)  
  (yank)
  (exchange-point-and-mark)
  (setq deactivate-mark nil)
  (rcomp-map "def2c")
  )

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

;; Python use python-mode
(setq ipython-command "ipython")

; View pkl files by running an external executable
(define-derived-mode pkl-mode fundamental-mode "pkl"
  "Major mode for viewing pkl files."
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (call-process "python3" nil t t (concat emacs-git  "scripts/pkl-cat.py") buffer-file-name)
  (set-buffer-modified-p nil)
  (read-only-mode)
  (toggle-truncate-lines)
  (setq scroll-preserve-screen-position 'always)
  (beginning-of-buffer))

; ediff options
(setq ediff-patch-options "")

; Add option to merge both alternatives in ediff by the "d" key.
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


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

(add-hook 'java-mode-hook
          (lambda ()
            (font-lock-mode)
            (interactive)
            (setq my-indent 4)
            (setq my-topmost-intro 0)
            (update-indent-mode)
            (define-key java-mode-map [(return)] 'newline-and-indent)))
(add-hook 'sgml-mode-hook 'font-lock-mode)
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))
(add-hook 'vala-mode-hook
          (lambda ()
            (define-key vala-mode-map [(return)] 'newline-and-indent)
            (setq indent-tabs-mode nil)))

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

(defun my-mode-compile (&optional arg)
  (interactive "P")
  (if arg
    (call-interactively 'mode-compile)
    (mode-compile "foo")))
  
;;; Why can't I get this to run automatically in the perl-mode hook
(defun my-perl-mode-hook ()
  (interactive)
  (define-key cperl-mode-map [(control c) (control c)] 'shell-perl-on-buffer)
  (define-key cperl-mode-map ";" 'self-insert-command)
  (define-key cperl-mode-map " " 'self-insert-command)
  (define-key cperl-mode-map "{" 'self-insert-command)
  (define-key cperl-mode-map [return] 'newline-and-indent)

  (setq-default abbrev-mode nil)
  (setq cperl-auto-newline nil)
  (setq-default cperl-auto-newline nil)
  (setq-default cperl-indent-level 4)
  (setq cperl-indent-parens-as-block t) 
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-hairy nil)
  (setq cperl-invalid-face nil) ; Don't show trailing white space!
  (setq cperl-mode-abbrev-table nil)
  (setq cperl-highlight-variables-indiscriminately t)
  
  (abbrev-mode 0)
  (define-key cperl-mode-map [(return)] 'newline-and-indent)
  (message "my-perl-mode-hook")
  )

;(add-hook 'pde-hook 'my-perl-mode-hook)
(add-hook 'cperl-mode-hook 'my-perl-mode-hook)
(add-hook 'lua-mode-hook 'my-lua-mode-hook)
  
(autoload 'vala-mode "vala-mode.el" "Valamode" t)
(autoload 'pov-mode "pov-mode.el" "PoVray scene file mode" t)
;(autoload 'sather-mode "sather.el" "Sather mode" t nil)
(autoload 'cweb-mode "cweb.el" "CWeb mode" t nil)
(autoload 'rust-mode "rust-mode.el" "Rust mode" t nil)
(autoload 'octave-mode "octave.el" "Octave mode" t nil)
;(autoload 'sgml-mode "sgml-mode.el" "SGML mode" t nil)
(autoload 'doc-mode "doc-mode.el" "Doc load" t nil)
(autoload 'web-mode "web-mode.el" "WEB mode" t nil)
(autoload 'elisp-mode "elisp-mode.el" "ELisp" t nil)
(autoload 'wat-mode "wat-mode.el")
(load "python-mode.el")
;(load "python.el")

(add-hook 'lua-mode-hook
          (lambda ()
            (setq lua-indent-level 2)
            (define-key lua-mode-map [(return)] 'newline-and-indent)))

;; Set some auto mode
(setq auto-mode-alist
      (append
;       (list (cons "\\.sa$" 'sather-mode))
       (list (cons "\\.el$" 'emacs-lisp-mode))
       (list (cons "\\.cs$" 'csharp-mode))
       (list (cons "\\.css$" 'css-mode))
       (list (cons "\\.csv$" 'csv-mode))
       (list (cons "\\.js$" 'js2-mode))
       (list (cons "\\.java$" 'java-mode))
       (list (cons "\\.w$" 'cweb-mode))
       (list (cons "\\.mp$" 'metapost-mode))
       (list (cons "\\.mf$" 'metafont-mode))
       (list (cons "\\.cmake$" 'cmake-mode))
       (list (cons "CMakeLists.txt" 'cmake-mode))
       (list (cons "\\.R$" 'ess-r-mode))
       (list (cons "SConstruct" 'python-mode))
       (list (cons "SConscript" 'python-mode))
       (list (cons "\\.md$" 'markdown-mode))
       (list (cons "\\.py$" 'python-mode))
       (list (cons "ChangeLog" 'change-log-mode))
       (list (cons "\\.gob$" 'c++-mode))
       (list (cons "\\.hh$" 'c++-mode))
       (list (cons "\\.H$" 'c++-mode))
       (list (cons "\\.cxx$" 'c++-mode))
       (list (cons "\\.c$" 'c-mode))
       (list (cons "\\.cu$" 'c++-mode))
       (list (cons "\\.cuh$" 'c++-mode))
       (list (cons "\\.cc$" 'c++-mode))
       (list (cons "\\.vert$" 'c++-mode))
       (list (cons "\\.frag$" 'c++-mode))
       (list (cons "\\.geom$" 'c++-mode))
       (list (cons "\\.[hc]pp$" 'c++-mode))
       (list (cons "\\.glsl$" 'c++-mode))
       (list (cons "\\.cpp$" 'c++-mode))
       (list (cons "\\.cc$" 'c++-mode))
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
       (list (cons "\\.txt$" 'text-mode)) 
       (list (cons "\\.org$" 'org-mode)) 
       (list (cons "\\.rst$" 'rst-mode)) 
       (list (cons "\\.p[lm]$" 'cperl-mode)) 
       (list (cons "\\.nxc$" 'c++-mode)) 
       (list (cons "\\.mw" 'mediawiki-mode)) 
       (list (cons "\\.ps" 'ps-mode)) 
       (list (cons "meson.build" 'meson-mode)) 
       (list (cons "\\.el\\.gz" 'emacs-lisp-mode))
       (list (cons "\\.lua$" 'lua-mode)) 
       (list (cons "\\.rs$" 'rust-mode)) 
       (list (cons "\\.html$" 'web-mode)) 
       (list (cons "\\.vue$" 'web-mode)) 
       (list (cons "\\.nsi\\(s\\)?$" 'nsis-mode)) 
       (list (cons "\\.pr[io]$" 'qt-pro-mode))
       (list (cons "\\.rec$" 'rec-mode))
       (list (cons "\\.was?t$" 'wat-mode))
       (list (cons "\\.robot$" 'robot-mode))
       (list (cons "\\.(conf|ini)$" 'conf-mode))
       (list (cons "\\.(asy)$" 'asy-mode))
       (list (cons "\\.(tar)$" 'tar-mode))
       (list (cons "\\.pkl$" 'pkl-mode))
       (list (cons "\\.pickle$" 'pkl-mode))
       (list (cons "\\.pio$" 'asm-mode))
       (list (cons "\\.FCMacro$" 'python-mode))
       (list (cons "\\.pth$" 'archive-mode))
       auto-mode-alist))

;; macros for nxc code
(defun nxt-run ()
  (interactive)
  (save-buffer)
  (let* ((cmd (format "nbc %s -sm- -r -S=usb " (buffer-name))))
    (shell-command cmd)))

;; Shell colors
(setq ansi-color-names-vector
      '("white" "red" "green" "brown" "blue" "magenta" "cyan" "black"))
(setq ansi-color-map (ansi-color-make-color-map))
(defun apply-ansi-color (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

;; Solve colorizing of e.g. epylint
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Colorize the output in the shell command
(defadvice display-message-or-buffer (before ansi-color activate)
  "Process ANSI color codes in shell output."
  (let ((buf (ad-get-arg 0)))
    (and (bufferp buf)
         (string= (buffer-name buf) "*Shell Command Output*")
         (with-current-buffer buf
           (ansi-color-apply-on-region (point-min) (point-max))))))

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

(defun buffer-local-set-key (key func)
  (interactive "KSet key on this buffer: \naCommand: ")
  (let ((name (format "%s-magic" (buffer-name))))
    (eval
     `(define-minor-mode ,(intern name)
        "Automagically built minor mode to define buffer-local keys."))
    (let* ((mapname (format "%s-map" name))
           (map (intern mapname)))
      (unless (boundp (intern mapname))
        (set map (make-sparse-keymap)))
      (eval
       `(define-key ,map ,key func)))
    (funcall (intern name) t)))

(defun browse-current-file ()
  (interactive)
  (browse-url (buffer-file-name)))

(defun etan-indent-mode ()
  (interactive)
  (setq web-mode-markup-indent-offset 4))

(defun my-web-mode ()
  (interactive)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (local-set-key [(control c) (control v)] 'browse-current-file)
  (setq web-mode-extra-snippets
      '((nil . (("jquery" . "<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js\"></script>")))
        ))
  (web-mode-on-engine-setted)  ; Needed to rebuild web-mode-extra-snippets
  (setq web-mode-enable-auto-indentation nil)  ; This is seriously broken
  )

(add-hook 'web-mode-hook 'my-web-mode)

; Choose applications to open external files .

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
(autoload 'dlint-minor-mode "dlint" nil t)
(add-hook 'd-mode-hook (lambda () (dlint-minor-mode 1)))

(setq sendmail-program "/home/dov/scripts/gmail-sendmail")
(setq lpr-command "poe")
(setq mc-pgp-user-id "dov@menora")

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
                                   "[ <]dov@orbotech.com"))

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
  (if (string-match "\\.phtml" buffer-file-name)
      (let ((htmlfilename (concat (file-name-sans-extension buffer-file-name) ".html")))
	(if (shell-command (concat "eperl " buffer-file-name "> " htmlfilename))
	    (browse-url-of-file htmlfilename)))
    (browse-url-of-file))
)
(defun run-asciidoc-maybe-and-preview-html()
  "Runs asciidoc for txt files and then runs the preview."
  (interactive)
  (if (string-match "\\.txt" buffer-file-name)
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

(defun open-work-notes-file ()
  "Load my main work notes file list"
  (interactive)
  (find-file work-notes-file)
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
  (switch-to-buffer (find-most-recent-pattern-buffer "\\*gud"))
  ; prepare for user input
  (end-of-buffer))

(defun goto-end-of-compilation-buffer ()
  (interactive) 
  (switch-to-buffer (find-most-recent-pattern-buffer "\\*compilation"))
  ; prepare for user input
  (end-of-buffer))

(defun current-filename-to-clip-buffer ()
  "Copy the current buffer file name to the clip buffer"
  (interactive)
  (kill-new buffer-file-name)
  (message "%s" buffer-file-name))

(defun current-buffername-to-clip-buffer ()
  "Copy the current buffer file name to the clip buffer"
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

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
                                 (cl-pushnew state possible-states))
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

(defun py-jump-exception-line ()
  "This function parses a python file reference"
  (interactive)
  (save-selected-window
    (let ((file nil)
          (line nil)
          (buffer (current-buffer)))
      (beginning-of-line)
                                        
      (if (looking-at "^> ") ; Recognize pdb lines
          (progn
            (forward-char)
            (forward-char)
            (set-mark-command nil)
            (search-forward "(")
            (backward-char)
            (exchange-point-and-mark)
            (setq file (buffer-substring-no-properties (point) (mark)))
            (exchange-point-and-mark)
            (forward-word)
            (backward-word)
            (set-mark-command nil)
            (forward-word)
            (setq line (buffer-substring-no-properties (point) (mark))))
        (progn              ; Recognize exception lines
          (forward-word)
          (forward-char)
          (forward-char)
          (set-mark-command nil)
          (search-forward "\"")
          (backward-char)
          (exchange-point-and-mark)
          (setq file (buffer-substring-no-properties (point) (mark)))
          (exchange-point-and-mark)
          (forward-word)
          (forward-word)
          (backward-word)
          (set-mark-command nil)
          (forward-word)
          (exchange-point-and-mark)
          (setq line (buffer-substring-no-properties (point) (mark)))
          ))
      (deactivate-mark)
      (beginning-of-line)
      (find-file-other-window file)
      (goto-line (string-to-number line))
      )))
(global-set-key (kbd "C-`") 'py-jump-exception-line)
(global-set-key (kbd "C-c '") 'py-jump-exception-line)

(global-set-key "\M-]" 'c-beginning-of-defun)
(global-set-key [(control ?') ?'] 'find-matching-keyword)
(global-set-key [(control ?') (control a)] 'save-buffer)
(global-set-key [(control ?,)] 'undo)
(global-set-key " " 'space-or-undo)
(global-set-key "\C-x\C-m" 'save-buffers-dont-ask)
(global-set-key [(control h) (control j)] 'gtk-lookup-symbol)
(global-set-key [(control h) (control q)] 'qtdoc-lookup)
(global-set-key [(control h) (control g)] 'google-lookup)
(global-set-key [(control h) (control p)] 'python-lookup)
(global-set-key [(control h) (control c)] 'cpp-lookup)
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
(global-set-key [(button5)] (lambda () (interactive) (scroll-up 5)))
(global-set-key [(button4)] (lambda () (interactive) (scroll-down 5)))
(global-set-key [(shift button5)] (lambda () (interactive) (scroll-up 1)))
(global-set-key [(shift button4)] (lambda () (interactive) (scroll-down 1)))
(global-set-key [(control button5)] (lambda () (interactive) (scroll-up)))
(global-set-key [(control button4)] (lambda () (interactive) (scroll-down)))
(global-set-key [(super kp-enter)] 'scroll-down-line)
(global-set-key [(control meta ?S)] 'vr/isearch-forward)
(global-set-key [(control meta ?R)] 'vr/isearch-backward)
(global-set-key [(control meta ?%)] 'vr/query-replace)
;(global-set-key [(control f31)] 'move-to-middle-window-line)
;(global-set-key [f31] 'recenter)
(global-set-key "\C-x\C-k" 'kill-compilation)
(global-set-key [(alt d)] 'goto-end-of-gud-buffer)
(global-set-key (kbd "A-C-f") 'current-filename-to-clip-buffer)
(global-set-key (kbd "A-C-b") 'current-buffername-to-clip-buffer)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)
;; Use shift as a subword indicator
(global-set-key [(meta shift ?f)] 'subword-forward)
(global-set-key [(meta shift ?b)] 'subword-backward)
(global-set-key [(control shift right)] 'subword-forward)
(global-set-key [(control shift left)] 'subword-backward)
(global-set-key [(meta shift backspace)] 'subword-backward-kill)
(global-set-key [(meta shift ?d)] 'subword-kill)
(global-set-key [(meta shift ?t)] 'subword-transpose)

;; Make shift-backspace erase subword
(global-set-key [(control shift backspace)]
                (lambda ()
                  (interactive)
                  (progn
                    (set-mark-command nil)
                    (subword-backward)
                    (kill-region (mark) (point)))))


;; Find first and return first buffer matching a given pattern
(defun old-find-first-buffer-match (buffers pattern)
  (let ((f (car buffers)))
    (message (buffer-name f))
    (cond ((eq f '()) nil)
          ((string-match pattern (buffer-name f)) f)
          (t (find-first-buffer-match (cdr buffers) pattern)))))

(defun find-first-buffer-match (buffers pattern)
  (dolist (f buffers)
    (let ((case-fold-search nil)) ;; case-sensitive
      (when (string-match pattern (buffer-name f))
        (cl-return f)))))

(defun find-most-recent-pattern-buffer (pattern)
  "find the most recent code buffer in the history and switch to it"
  (let ((f (find-first-buffer-match (cdr (buffer-list)) pattern)))
    (if (not (eq f nil))
        (switch-to-buffer f))))

(defun find-most-recent-python-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.py"))

(defun find-most-recent-mpremote-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "mpremote"))

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

(defun find-most-reset-copilot-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\*[cC]opilot.*"))

(defun find-most-recent-org-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.org\$"))

;; git find file
(load "git-find-file.el")
(global-set-key [(control c) ?g] 'git-find-file)

;; git grep - use deadgrep instead
(load "dov-git-grep")
;(global-set-key [(control c) ?s] 'dov-git-grep)
;(global-set-key [(control c) (control s)] 'dov-git-grep-here)

;; neo-tree configuration
(eval-after-load "neotree"
  '(setq neo-hidden-regexp-list
         '(
           "^\\."
           "\\.pyc$"
           "~$"
           "^#.*#$"
           "\\.elc$"
           "\\.bak$")))
(load "neotree.el")
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t)
(autoload 'neotree-toogle "neotree" nil t)
(global-set-key [f8] 'neotree-toggle)

;; Shortcuts to go to special buffers
(global-set-key [(alt meta d)] 'goto-end-of-gud-buffer)
(global-set-key [(alt meta k)] 'goto-end-of-compilation-buffer)
(global-set-key [(control c) ?b ?c] 'find-most-recent-c-buffer)
(global-set-key [(control c) ?b ?e] 'find-most-recent-emacs-buffer)
(global-set-key [(control c) ?b ?p] 'find-most-recent-python-buffer)
;(global-set-key [(control c) ?b ?p] 'find-most-recent-mpremote-buffer)
(global-set-key [(control c) ?b ?m] 'find-most-recent-magit-buffer)
(global-set-key [(control c) ?b ?o] 'find-most-recent-org-buffer)
(global-set-key [(control c) ?b ?j] (lambda () (interactive) 
  (switch-to-buffer (find-most-recent-pattern-buffer "\\*ein: http"))))
(global-set-key [(alt meta m)] 'find-most-recent-magit-buffer)
(global-set-key [(alt meta c)] 'find-most-reset-copilot-buffer)
(global-set-key [(alt meta y)] 'find-most-recent-python-buffer)
(global-set-key [(alt meta n)] (lambda () (interactive) 
  (switch-to-buffer (find-most-recent-pattern-buffer "notes.*\\.org"))))
(global-set-key [(alt meta h)] (lambda () (interactive) 
  (switch-to-buffer (find-most-recent-pattern-buffer "\\*shell"))
  ; prepare for user input
  (end-of-buffer)))
(global-set-key [(alt meta o)] (lambda () (interactive) 
  (switch-to-buffer "*Inferior Octave*")
  ; prepare for user input
  (end-of-buffer)))
(global-set-key [(alt meta s)] (lambda () (interactive) 
  (switch-to-buffer "*scratch*")))
;(global-set-key [(alt meta p)] (lambda () (interactive) 
;  (find-most-recent-pattern-buffer "\\*Python")))
(global-set-key [(alt meta p)] (lambda () (interactive) 
  (find-most-recent-pattern-buffer "*mpremote")))
(global-set-key [(alt meta j)] (lambda () (interactive) 
  (switch-to-buffer (find-most-recent-pattern-buffer "\\*ein:notebook"))))

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
;(global-set-key [(meta delete)] 'delete-window)
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
;(global-set-key [(control j)] 'isearch-forward)
(global-set-key "\C-xw" 'write-region)
(global-set-key "\C-x\C-r" 'revert-buffer)
(global-set-key [(alt tab)] 'indent-relative)
(global-set-key [(hyper tab)] 'indent-relative)
(global-set-key [(hyper tab)] 'indent-relative)
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
(global-set-key "\C-x&" 'call-last-kbd-macro)
(global-set-key [(find)] 'toolbar-mail)
(global-set-key [(meta \`)] 'next-error)
(global-set-key [(meta \~)] (lambda () (interactive) (next-error -1)))
(global-set-key [(control meta up)] (lambda () (interactive) (scroll-other-window 1)))
(global-set-key [(control meta down)] (lambda () (interactive) (scroll-other-window -1)))
(global-set-key [(meta prior)] (lambda () (interactive) (scroll-other-window-down nil)))
(global-set-key [(meta next)] (lambda () (interactive) (scroll-other-window nil)))
(global-set-key [f5] 'open-notes-file)
(global-set-key [(meta f5)] 'open-work-notes-file)

(global-set-key "\C-cT" 'toggle-truncate-lines)
(define-key global-map " " 'space-or-undo)
(define-key global-map "\C-x\C-m" 'save-buffers-dont-ask)
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
(autoload 'cperl-mode "cperl-mode" nil t)
(setq interpreter-mode-alist (append interpreter-mode-alist
				     '(("miniperl" . perl-mode))))

(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
; (load "perl-mode")   ; old mode

;; qt docs lookup
(load "qtdoc")
(setq qtdoc-html-root "file:///usr/share/doc/qt5/qtwidgets") 
(load "google-look")

;; Other customization
(line-number-mode t)
(column-number-mode t)

(autoload 'tmpl-expand-templates-in-buffer "tmpl-minor-mode"
  "Expand all templates in the current buffer." t)

(setq minibuffer-max-depth nil)

;; Here is where xemacs adds on its customization options
(put 'narrow-to-region 'disabled nil)

;; json/javascript 
(setq javascript-indent-level 2)
(require 'js-doc)

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

(defun agg-indent-mode ()
  "Set indent tabs to 4 as style I use at WIS."
  (interactive)
  (setq my-indent 3)
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

  ;; Json
  (setq js2-basic-offset 2)
  (setq js-indent-level 2)

  ;; web-mode
  (setq web-mode-markup-indent-offset 2)
  )

(defun xjet4-indent-mode ()
  "Similar to xjet-mode but with four spaces"
  (interactive)
  ;; C++-python
  (setq my-indent 4)
  (setq my-substatement 2)
  (setq my-substatement-open 0)
  (setq my-access-label 0)
  (setq my-topmost-intro 0)
  (update-indent-mode)

  ;; Python
  (setq py-indent-offset 2)

  ;; Json
  (setq js2-basic-offset 2)
  (setq js-indent-level 2))


(defun standard-python-indent ()
  """Setup standard python indentation"""
  (interactive)
  (setq py-indent-offset 4)
  (setq indent-bars-spacing 4)
  (indent-bars-mode t))

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

(defun my-outline-minor-map-hook ()
  "Set outline mapping stuff"
  (define-key outline-minor-mode-map [(control ?2) ?h] 'outline-hide-subtree)
  (define-key outline-minor-mode-map [(control kp-subtract)] 'outline-hide-subtree)
  (define-key outline-minor-mode-map [(control ?2) ?s] 'outline-show-subtree)
  (define-key outline-minor-mode-map [(control kp-add)] 'outline-show-subtree)
  (define-key outline-minor-mode-map [(control ?2) (control ?f)] 'outline-forward-same-level)
  (define-key outline-minor-mode-map [(control ?2) (control ?b)] 'outline-backward-same-level))

(add-hook 'outline-minor-mode-hook 'my-outline-minor-map-hook)

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
  (find-file (concat emacs-git "dov.emacs"))
  (lisp-mode))

(defun do-return-indent (map) ""
  (define-key map [return] 'newline-and-indent)
  (setq indent-tabs-mode nil))

(defun my-change-mode-hook (map) ""
  (setq indent-tabs-mode nil))

;; Control scroll change fonts like in FireFox
(require 'zoom-frm)
(global-set-key (kbd "<C-mouse-5>") 'zoom-in)
(global-set-key (kbd "<C-mouse-4>") 'zoom-out)

(add-hook 'tcl-mode-hook (lambda() (do-return-indent tcl-mode-map)))

;; Turn on horizontal scrolling with mouse wheel
(global-set-key (kbd "<mouse-7>") (lambda () (interactive) (scroll-left 8)))
(global-set-key (kbd "<mouse-6>") (lambda () (interactive) (scroll-right 8)))
(global-set-key (kbd "<wheel-left>") (lambda () (interactive) (scroll-left 8)))
(global-set-key (kbd "<wheel-right>") (lambda () (interactive) (scroll-right 8)))
(global-set-key (kbd "<M-mouse-7>") (lambda () (interactive) (scroll-left 32)))
(global-set-key (kbd "<M-mouse-6>") (lambda () (interactive) (scroll-right 32)))
(global-set-key (kbd "<A-mouse-5>") (lambda () (interactive) (scroll-left 32)))
(global-set-key (kbd "<A-mouse-4>") (lambda () (interactive) (scroll-right 32)))
(global-set-key (kbd "<A-wheel-up>") (lambda () (interactive) (scroll-left 32)))
(global-set-key (kbd "<A-wheel-down>") (lambda () (interactive) (scroll-right 32)))

(global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 4)))
(global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 4)))
(global-set-key (kbd "<M-mouse-5>") (lambda () (interactive) (scroll-up 16)))
(global-set-key (kbd "<M-mouse-4>") (lambda () (interactive) (scroll-down 16)))

(defun rhino-js-eval-region-or-buffer ()
  "Evaluate the current buffer (or region if mark-active),
   and return the result into another buffer,
   which is to be shown in a window."
  (interactive)
  (let ((debug-on-error t) (start 1) (end 1))
    (cond
     (mark-active
      (setq start (point))
      (setq end (mark)))
     (t
      (setq start (point-min))
      (setq end (point-max))))
    (setq tmpfile (make-temp-file "js"))
    (write-region start end tmpfile t)
    (setq res (shell-command-to-string (concat "rhino " tmpfile)))
    (message res)
    (setq deactivate-mark t))) ; deactive the region, regardless

(add-hook 'js2-mode-hook
          (lambda()
            (do-return-indent js2-mode-map)
            (define-key js2-mode-map "\C-c\C-d" 'js-doc-insert-function-doc)
            (define-key js2-mode-map "@" 'js-doc-insert-tag)
            (define-key js2-mode-map "\C-c\C-c" 'rhino-js-eval-region-or-buffer)
            (setq js2-strict-trailing-comma-warning nil)
            (setq js2-strict-missing-semi-warning nil)
            ))

;(add-hook 'py-mode-hook (lambda() 
;                           (define-key py-mode-map [(control m)] 'py-newline-and-indent)
;                           ))
(defun my-python-mode-hook ()
  (local-set-key (kbd "RET") 'py-newline-and-indent)
  (remove-dos-eol)
  (setq py-indent-offset my-indent)
  (setq python-indent my-indent)
  (remove-dos-eol)
  (local-set-key [(alt ? )] 'gud-break)
  (local-set-key [(alt ?b)] 'left-word)
  (local-set-key [(alt ?f)] 'right-word)
  (local-set-key [(control c) (control s)] 'python-shell-send-buffer)
  (local-set-key [(control c) (control c)] 'shell-python-on-buffer)
  (local-set-key [(control c) (control j)] 'xjet-python-buffer)
  (local-set-key [(control c) (control e)] 'goto-compilation-directory-and-compile)
  (local-set-key [(control up)] 'scroll-up-line)
  (local-set-key [(control down)] 'scroll-down-line)
  (local-set-key [(control right)] 'forward-word)
  (local-set-key [(control left)] 'backward-word)
  ;; I don't like interactive shell for python commands by default
  (setq py-fast-process-p nil)
  (eldoc-mode 0)
  (setq-local eldoc-documentation-function #'ignore)
  (indent-bars-mode)
  ;; restore backward erase word
  (local-set-key [(control backspace)] 'backward-kill-word)
                                        ;     (company-mode -1)   ; Doesn't work!
)
  
(add-hook 'python-mode-hook 'my-python-mode-hook)

(global-eldoc-mode -1) ;; Don't use this at the moment...

(setq company-global-modes '(not python-mode))
(add-hook 'diff-mode-hook (lambda() 
                             (remove-dos-eol)))
(add-hook 'csv-mode-hook (lambda() 
                           (setq truncate-lines t)
                           (setq word-wrap nil)
                           ))
(add-hook 'mediawiki-mode-hook
          (lambda() 
             (local-set-key [(control x) (control s)] 'mediawiki-save)
             (local-set-key [(control up)] 'scroll-up-line)
             (local-set-key [(control down)] 'scroll-down-line)
             ))

(add-hook 'change-log-mode-hook (lambda() 
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

(defun calc-eval-region (&optional insert)
  "Send the selection to calc-eval and insert the result. Also turns asterisks (*) into times(×) signs."
  (interactive)
  (if (not (use-region-p))
      (error "Need region!"))
  (let ((res (calc-eval (buffer-substring-no-properties (region-beginning) (region-end)))))
    ;; Turn * into ×
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (beginning-of-buffer) ; since we are narrowing!
      (while (search-forward "*" nil t) (replace-match "×" 'literal))
      ; Uncomment if you like division signs as well
;      (beginning-of-buffer)
;      (while (search-forward "/" nil t) (replace-match "÷" 'literal))
      (end-of-buffer))
    (deactivate-mark t)
    (insert "=")
    (insert res)
    (message res)))

(defun hm2dec (hm)
  "Convert a hour minute string to a decimal string"
  (save-match-data ; is usually a good idea
    (and (string-match "\\`\\([0-9]+\\):\\([0-9]+\\)\\'" hm)
         (setq hour (string-to-number (match-string 1 hm))
               min (string-to-number (match-string 2 hm) ) ))
    (+ hour (/ min 60.0))))

(defun region-hm2dec (&optional insert)
  "Convert the selected area from hour:min to decimal"
  (interactive)
  (if (not (use-region-p))
      (error "Need region!"))
  (let ((res (format "%.2f" (hm2dec (buffer-substring-no-properties (region-beginning) (region-end))))))
    (kill-region (region-beginning) (region-end))
    (insert res)))

(defun toggle-backslash-line ()
  "Toggle all forward slashes to backslashes for the current line."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (setq myBoundaries (cons (region-beginning) (region-end)))
        (setq myBoundaries (bounds-of-thing-at-point 'line)))
      
    (save-restriction
      (goto-char (car myBoundaries))
      (narrow-to-region (car myBoundaries) (cdr myBoundaries))
      (if (search-forward "/" nil t)
          (progn
            (goto-char (car myBoundaries))
            (while (search-forward "/" nil t) (replace-match "\\\\" 'literal)))
        (progn
          (goto-char (car myBoundaries))
          (while (search-forward "\\" nil t) (replace-match "/")))
        )
      ))
    (if (use-region-p)
        (progn
          (set-mark (car myBoundaries))
          (goto-char (cdr myBoundaries)))))

(global-set-key [(control x) (control ?\\)] 'toggle-backslash-line)

(defun replace-region (bounds new-string)
  """Replace the region given by bounds with new-string"""
  (kill-region (car bounds) (cdr bounds))
  (insert new-string))

(defun bounds-of-date-at-point ()
  "Return the bounds of the date like string at the point. It recognizes
   dashes and slashes, but not periods yet (because they may be put
   after the date as a real period)"
  (interactive)
  (save-excursion
    (let* ((org-point (point))
           (non-date-chars "[^/-9A-Z_a-z-]")
           (bound-start (+ (search-forward-regexp non-date-chars) 1)))
      (goto-char org-point)
      (let* ((bound-end (- (search-backward-regexp non-date-chars) 1)))
        (cons bound-start bound-end)))))

(defun toggle-date-iso ()
  "Toggle iso dates to hebrew dates and vise verse"
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-date-at-point))
           (word (current-word)))
      (if (pcre-string-match "(\\d{4})-(\\d{2})-(\\d{2})" word )
          (let* ((year (match-string 1 word))
                 (month (match-string 2 word))
                 (day (match-string 3 word)))
            (replace-region bounds (format "%d/%d/%s"
              (string-to-number day)
              (string-to-number month)
              year))
            )
          (if (pcre-string-match "(\\d{1,2})/(\\d{1,2})/(\\d{4})" word )
              (let* ((day (match-string 1 word))
                     (month (match-string 2 word))
                     (year (match-string 3 word)))
                (replace-region bounds
                  (format "%s-%02d-%02d"
                    year (string-to-number month) (string-to-number day))))
              (error "no match"))))))

(defun tilde-expand-line ()
  "Do shell exansion of a tilde"
  (interactive)
  (if (use-region-p)
      (setq myBoundaries (cons (region-beginning) (region-end)))
    (setq myBoundaries (bounds-of-thing-at-point 'line)))
  (let*((reg-beg (car myBoundaries))
        (reg-end (cdr myBoundaries))
        (res (expand-file-name (buffer-substring-no-properties reg-beg reg-end))))
    (kill-region reg-beg reg-end)
    (insert res)))

;; Cod definitions
(defun set-cod ()
  "Set the current default compilation directory environment variable"
  (interactive)
  (let* ((dir (expand-file-name default-directory)))
    (setenv "COD" dir)
    (setenv "CLANGD_FLAGS" (concat "--compile-commands-dir=" dir))
    (message (concat "COD set to " dir))))

(defun goto-cod ()
  "Open the cod directory"
  (interactive)
  (find-file (getenv "COD")))
(defun insert-cod ()
  "Insert cod into the buffer"
  (interactive)
  (insert (getenv "COD")))
  
(defun copy-path-to-clip-buffer ()
  "Copy the current visited path to the clip buffer"
  (interactive)
  (kill-new default-directory)
  (message default-directory))

; cod related keybindings
(define-key dired-mode-map (kbd "\C-c h") 'set-cod)
(define-key dired-mode-map (kbd "\C-c w") 'copy-path-to-clip-buffer)
(define-key shell-mode-map (kbd "\C-c h") 'set-cod)
(define-key shell-mode-map (kbd "\C-c w") 'copy-path-to-clip-buffer)
(global-set-key (kbd "\C-x C") 'insert-cod)

(defun goto-compilation-directory-and-compile()
  "chdir to the COD (compilation directory) and compile"
  (interactive)
  ; turn off helm
  (let ((completion-in-region-function 'completion--in-region))
    (if (equal current-prefix-arg nil)
        (let ((default-directory (getenv "COD")))
          (call-interactively #'compile))
        (call-interactively #'compile))))

(global-set-key [(control c) (control e)] 'goto-compilation-directory-and-compile)
(global-set-key [(control ?') (control e)] 'goto-compilation-directory-and-compile)

(global-set-key [(control x) ?~] 'tilde-expand-line)

(defun jenkins-mangle ()
  "Mangle the Jenkins data path to its backup location. (XJet specific)"
  (interactive)
  (save-excursion
    (save-restriction
      (toggle-backslash-line)
      (while (search-forward "D:/Jenkins/workspace/DeveloperBuild/RunningEnv" nil t) (replace-match "file://sshx:dov@dovg:/mnt/Software-Temp/MetalJetData")))))

(add-hook 'comint-mode-hook
  (lambda()
    (define-key comint-mode-map [(meta p)] 'comint-previous-matching-input-from-input)
    (define-key comint-mode-map [(meta n)] 'comint-next-matching-input-from-input)
    (define-key comint-mode-map [(control c) (control o)] 'comint-kill-output-to-kill-ring)
    (define-key comint-mode-map [(control x) (control ?\\)] 'toggle-backslash-line)
;    (define-key comint-mode-map [(tab)] 'comint-dynamic-complete)
    (define-key comint-mode-map [(tab)] 'completion-at-point)

    ; Save history when the shell is killed
    (make-local-variable 'comint-input-ring-file-name)
    (setq comint-input-ring-file-name (concat emacs-persistance-dir "/comint-history"))
    (setq comint-input-ring-size 10000)
    (setq comint-process-echoes 't)
    (comint-read-input-ring)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'comint-write-input-ring)
  ))

(defun my-shell-mode-hook ()
  ;; Keep C-c C-o for compilation "everywhere"
  (define-key shell-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile)
  (ansi-color-for-comint-mode-on))
(add-hook 'shell-mode-hook 'my-shell-mode-hook)

(defun my-nsis-mode-hook ()
  (define-key nsis-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile))
(add-hook 'nsis-mode-hook 'my-nsis-mode-hook)

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
    (define-key gud-mode-map [(alt f)] 'gud-finish)
    (define-key gud-mode-map [(control c) (control e)] 'goto-compilation-directory-and-compile)
;    (define-key gud-mode-map [(alt h)] 'gud-until)
;    (define-key gud-mode-map "\C-i" 'shell-dynamic-complete-filename)
    (make-local-variable 'comint-input-ring-file-name)
    (setq comint-input-ring-file-name (concat emacs-persistance-dir "/gdb-history"))
    (setq comint-input-ring-size 10000)
    (comint-read-input-ring)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'comint-write-input-ring)
    (setenv "PYTHONPATH" nil)   ;; Solve gdb python3 problems!

    ;; Makes pdb tracking work inside gud
    (require 'python)
    (add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)))

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
(defun let-twospaces () (interactive) (insert "  "))
(global-set-key [(super tab)] 'let-twospaces)
(defun delete-two-chars () (interactive) (backward-delete-char 2))

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

;; -- new function
(defun shell-command-on-buffer (command extension)
  "Send the current buffer to a shell command"
  (interactive)
  (let* ((remote-maybe (file-remote-p default-directory))
         (tramp-prefix (if remote-maybe remote-maybe ""))
         (cmd-buffer-name (concat "*" (capitalize command) " Output*"))
         (cmd-filename
          (if (buffer-modified-p)
              (concat tramp-prefix temp-dir "/buffer." extension)
           (buffer-file-name))))
         (if (buffer-modified-p)
             (write-region (point-min) (point-max) cmd-filename))
    (if remote-maybe
       ;; If remote, we have to get rid of the tramp prefix
       (let* ((remote-prefix-len (length remote-maybe))
              (fn (substring cmd-filename remote-prefix-len)))
         (shell-command (concat my-remote-shell " -c '" command " \"" fn "\" '") cmd-buffer-name))
       (shell-command (concat command " \"" cmd-filename "\"") cmd-buffer-name))
  ;; The following makes it easy to go to the resulting output buffer
  (setq my-buffer (current-buffer))
  (switch-to-buffer cmd-buffer-name)
  (switch-to-buffer my-buffer)))

(defun async-shell-command-on-buffer (command extension)
  "Send the current buffer to an async shell command"
  (interactive)
  (let* ((remote-maybe (file-remote-p default-directory))
         (tramp-prefix (if remote-maybe remote-maybe ""))
         (cmd-buffer-name
          (concat "*" (capitalize (file-name-nondirectory command)) " Output:" (buffer-name) "*"))
         (cmd-filename
          (if (buffer-modified-p)
              (concat tramp-prefix temp-dir "/buffer." extension)
           (buffer-file-name))))
         (if (buffer-modified-p)
             (write-region (point-min) (point-max) cmd-filename))
    (if remote-maybe
       ;; If remote, we have to get rid of the tramp prefix
       (let* ((remote-prefix-len (length remote-maybe))
              (fn (substring cmd-filename remote-prefix-len)))
         (async-shell-command (concat my-remote-shell " -c '" command " \"" fn "\" '") cmd-buffer-name))
       (async-shell-command (concat command " \"" cmd-filename "\"") cmd-buffer-name))
  ;; The following makes it easy to go to the resulting output buffer
  (setq my-buffer (current-buffer))
  (switch-to-buffer cmd-buffer-name)
  ; locally reset the key "up" to (previous-line)
  (local-set-key [up] 'previous-line)
  (local-set-key [down] 'next-line)
  (switch-to-buffer my-buffer)))

(defun shell-python-on-buffer ()
  "Send the current (python) buffer to be evaluated by the python shell"
  (interactive)
  (async-shell-command-on-buffer my-python-interpreter "py"))

(defun shell-perl-on-buffer ()
  "Send the current (python) buffer to be evaluated by the python shell"
  (interactive)
  (async-shell-command-on-buffer "perl" "pl"))

(defun shell-lua-on-buffer ()
  "Send the current (lua) buffer to be evaluated by the lua shell"
  (interactive)
  (async-shell-command-on-buffer "lua" "lua"))

;; switch different python interpreters
(defun choose-python-interpreter ()
  (interactive)
  (let ((choice (completing-read "Choose python interpreter: " '("system" "conda"))))
    (if (string= choice "system")
        (setq my-python-interpreter "/usr/bin/python")
      (setq my-python-interpreter "/home/dov/miniforge/bin/python"))
    (message "my-python-interpreter is %s" my-python-interpreter)))

;; From: https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version#29757750
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun ediff-copy-both-ba-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    )))
(defun add-d-to-ediff-mode-map
    ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)
  (define-key ediff-mode-map "D" 'ediff-copy-both-ba-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(setq my-remote-shell "zsh")
(setq my-python-interpreter "python3")

(defun xjet-python-buffer ()
  "Send the current (python) buffer to be evaluated in the MetalJet Application"
  (interactive)
  (if (buffer-modified-p)
      (progn
        (setq filename (concat temporary-file-directory "/buffer.py"))
        (write-region (point-min) (point-max) filename))
    (setq filename (replace-regexp-in-string "^/sshx:groxjet:" "" buffer-file-name))
    )
    
  (xjet-remote-python-file filename))

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

  
;; Some git convenience commands
(defun gitlg ()
  "Run git log -1 and copy the result to the clip buffer"
  (interactive)
  (shell-command "git log -1" "*git-log-1*")
  (switch-to-buffer "*git-log-1*")
  (clipboard-kill-ring-save (point-min) (point-max))
  (kill-buffer "*git-log-1*"))

;; Bind some keybindings to sound. Did when I had a throat
;; infection and couldn't talk.
(defun play-ogg (sound)
  (interactive)
  (let ((fn (concat emacs-git "../sounds/" sound ".ogg")))
    (if (string-match "x86_64-w64-mingw32" system-configuration)
        ;; A hack just to her something!
        (call-process "powershell" nil "*foo*" nil "-c (New-Object Media.SoundPlayer \"C:\\Windows\\Media\\chimes.wav\").PlaySync();")
      (call-process "ogg123" nil "*foo*" nil fn)))) 
;; None of this works!!!
;; (w32-shell-execute "open" "c:/users/dovg/git/dov-env/sounds/open-ended.ogg")
;; (w32-shell-execute "cmd" (concat "/C \"c:\\Program Files\\videolan\\vlc\\vlc.exe\" --qt-start-minimized --play-and-exit " "c:/users/dovg/git/dov-env/sounds/open-ended.ogg"))
;; (call-process "c:\\Program Files\\videolan\\vlc\\vlc.exe" nil "*foo*" nil "c:/users/dovg/git/dov-env/sounds/open-ended.ogg")
;; (call-process "wmplayer" nil "*foo*" nil "c:/users/dovg/git/dov-env/sounds/open-ended.ogg")
;(call-process "powershell" nil "*foo*" nil "-c (New-Object Media.SoundPlayer \"C:\\Windows\\Media\\notify.wav\").PlaySync();")
;(call-process "powershell" nil "*foo*" nil "-c (New-Object Media.SoundPlayer \"c:\\users\\dovg\\git\\dov-env\\sounds\\open-ended.ogg\").PlaySync();")

;; Why doesn't this work?
(defun play-wav (sound)
  (interactive)
  (let ((fn (concat emacs-git "../sounds/" sound ".wav")))
    (play-sound-file fn)))

(defun play-pling ()
  (interactive)
  (play-ogg "open-ended"))
(defun play-buzz ()
  (interactive)
  (play-ogg "one-buzz"))

(global-set-key [(control ?x) (control ?8)] 'play-pling)
(global-set-key [(control ?נ) (control ?8)] 'play-pling)
(global-set-key [(control ?x) (control ?7)] 'play-buzz)
(global-set-key [(control ?נ) (control ?7)] 'play-buzz)


;; some motion bindings in Hebrew mode that reflect key
;; positions for Dvorak.
(global-set-key [(control ?ש)] 'move-beginning-of-line)
(global-set-key [(control ?ג)] 'move-end-of-line)
(global-set-key [(control ?ר)] 'previous-line)
(global-set-key [(control ?ך)] 'next-line)
(global-set-key [(control ?ה)] 'kill-line)
(global-set-key [(control ?ף)] 'isearch-forward)
(global-set-key [(control ?ם)] 'isearch-backward)
(global-set-key [(control ?ץ)] 'scroll-up-command)
(global-set-key [(meta ?ץ)] 'scroll-down-command)
(global-set-key [(meta ?ט)] 'forward-word)
(global-set-key [(control ?ט)] 'forward-char)
(global-set-key [(meta ?מ)] 'backward-word)
(global-set-key [(control ?מ)] 'backward-char)
(define-key isearch-mode-map [(control ?ף)] 'isearch-repeat-forward)
(define-key isearch-mode-map [(control ?ם)] 'isearch-repeat-backward)
(global-set-key [(control ?נ) (control ?ף)] 'save-buffer)

(global-set-key [(meta ?ֱ)] 'beginning-of-buffer)
(global-set-key [(meta ?ֲ)] 'end-of-buffer)
(global-set-key [(meta ?ו)] 'goto-line)

(global-set-key (kbd "C-\"") 'undo)

;; dired default applications
(setq dired-guess-shell-alist-user
      (list
       (list "\\.png$" "giv");; fixed rule
       (list "\\.blend$" "blender");; fixed rule
       (list "\\.jpe?g$" "giv");; fixed rule
       ))
;; Setup for terminal mode (typically run in from tablet)
(if (not window-system)
    (progn
      (xterm-mouse-mode 1)))
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
      (cl-eval-when (WHEN load) '(ad-activate 'compile))
    (eval-after-load "compile" '(ad-activate 'compile))))

;; eww - Use google search by default
(setq eww-search-prefix "https://google.com/search?q=")

;; Emacs customization - this might be overwritten in the .emacs file

(add-hook 'after-init-hook
  (lambda ()
    (if (or (string= window-system "x") (string= window-system "w32"))
        (custom-set-faces
         '(font-lock-function-name-face ((t (:foreground "blue" :family my-fixed-font))))
         '(link ((t (:foreground "RoyalBlue3" :underline t))))
         '(helm-selection ((t (:background "#b5ffd1" :distant-foreground "black" :foreground "black"))))
         '(font-lock-builtin-face ((t (:foreground "dark slate blue"))))
         '(font-lock-comment-face ((t (:foreground "Firebrick"))))
         '(font-lock-constant-face ((((class color) (min-colors 88) (background light)) (:foreground "MidnightBlue"))))
         '(font-lock-keyword-face ((t (:foreground "purple"))))
         '(font-lock-type-face ((t (:foreground "ForestGreen"))))
         '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "green4"))))
         '(font-mediawiki-bold-face ((((class color) (background light)) (:inherit bold :foreground "Midnight blue"))))
         '(font-mediawiki-italic-face ((((class color) (background light)) (:inherit italic :foreground "Midnightblue"))))
         '(font-mediawiki-sedate-face ((((class color) (background light)) (:foreground "Black" :weight bold))))
         (list 'org-code (list (list t (list
                                        :family my-fixed-font
                                        :foreground "darkgreen"))))
         (list 'markdown-code-face (list (list t (list
                                                  :family my-fixed-font
                                                  :foreground "darkgreen"))))
         (list 'org-block (list (list t (list
                                        :family my-fixed-font
                                        :foreground "darkgreen"))))
         (list 'org-level-2 (list (list t (list
                                           :family my-variable-font
                                           :inherit 'outline-2
                                           :weight 'bold
                                           :height 1.4))))
         (list 'org-level-3 (list (list t (list
                                           :family my-variable-font
                                           :inherit 'outline-3
                                           :weight 'bold
                                           :height 1.2))))
         (list 'org-level-4 (list (list t (list
                                           :family my-variable-font
                                           :inherit 'outline-4
                                           :weight 'bold
                                           :height 1.1))))
         (list 'org-level-5 (list (list t (list
                                           :family my-variable-font
                                           :inherit 'outline-5
                                           :weight 'bold
                                           :height 1.05))))
         (list 'org-level-6 (list (list t (list
                                           :family my-variable-font
                                           :inherit 'outline-6
                                           :weight 'bold
                                           :height 1.03))))
         '(show-paren-match ((((class color) (background light)) (:background "#b4eeb4"))))
         '(region ((t (:background "#e0e8ff"))))
         '(org-hide ((((background light)) (:foreground "gray85"))))
         '(lsp-ui-sideline-code-action ((t (:foreground "gray55"))))
         '(minibuffer-prompt ((t (:foreground "black"))))
         )
    ;; else
      ;; Colors for green on black terminal
      (progn
      (custom-set-faces
       '(default ((t (:foreground "white"))))
       '(font-lock-constant-face ((t (:foreground "Orange"))))
       '(font-lock-function-name-face ((t (:foreground "Yellow" :family my-fixed-font))))
       '(font-lock-comment-face ((t (:foreground "#db4545"))))
       '(font-lock-keyword-face ((t (:foreground "Orange" :family my-fixed-font))))
       '(font-lock-string-face ((t (:foreground "green" :family my-fixed-font))))
       '(font-lock-type-face ((t (:foreground "maroon2" :family my-fixed-font))))
       '(font-lock-builtin-face ((t (:foreground "Orange" :family my-fixed-font))))
       '(org-verbatim ((t (:foreground "green"))))
       '(org-code ((t (:foreground "green"))))
  
       '(py-builtins-face ((t (:foreground "#f84" :family my-fixed-font))) t)
       '(minibuffer-prompt ((t (:foreground "green"))))
       '(show-paren-match ((t (:background "#228"))))
       '(region ((t (:background "#501280"))))
       '(org-link ((t (:inherit link :foreground "#09f"))))
       '(org-document-info ((t (:foreground "#9370db"))))
       '(org-document-title ((t (:foreground "#9370db"))))
       '(org-table ((t (:foreground "#cyan2"))))
       '(org-meta-line ((t (:foreground "#9370db"))))
       '(org-verbatim ((t (:foreground "green"))))
       '(org-code ((t (:foreground "green"))))
       '(helm-selection ((t (:background "ForestGreen" :distant-foreground "black" :foreground "black"))))
       '(lsp-ui-sideline-code-action ((t (:foreground "gray55"))))
       )
      ;; I don't understand why this doesn't work as a normal attribute!
      (set-face-attribute 'org-hide nil :foreground "gray30")
      )
    )))

;; -*- lexical-binding:t -*-
(defun my-clone-frame ()
  """Clone the selected frame"""
  (interactive)
  (let* ((src (selected-window))
         (src-point (copy-marker (window-point)))
         (src-start (copy-marker (window-start)))
         (clone (frame-root-window (make-frame))))
    (add-hook 'post-command-hook
              (lambda ()
                (unless (= src-point (window-point src))
                  (move-marker src-point (window-point src))
                  (set-window-point clone src-point))
                (unless (= src-start (window-start src))
                  (move-marker src-start (window-start src))
                  (set-window-start clone src-start))))))

; Ignore warnings
(add-to-list 'warning-suppress-log-types '(unlock-file))
(add-to-list 'warning-suppress-types '(unlock-file))
(setq warning-minimum-level :error)
(setq create-lockfiles nil)

(custom-set-variables
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

