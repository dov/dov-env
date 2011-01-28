;;; gtk-look.el --- lookup Gtk and Gnome documentation.

;; Copyright 2004, 2006, 2007 Kevin Ryde
;;
;; gtk-look.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; gtk-look.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; http://www.gnu.org/licenses/gpl.txt, or you should have one in the file
;; COPYING which comes with GNU Emacs and other GNU programs.  Failing that,
;; write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA.


;;; Commentary:

;; M-x gtk-lookup-symbol displays HTML documentation for Gtk and Gnome
;; functions and variables, similar to what M-x info-lookup-symbol does for
;; info files.
;;
;; The documentation is expected to be in the style of Debian packages like
;; libgtk2.0-doc, meaning HTML files with devhelp indices.  The location of
;; the indices can be customized in `gtk-lookup-devhelp-indices'.
;;
;; `browse-url' is used for display.  This is done in an "other window",
;; like info-look if `browse-url' uses a window (as opposed to running
;; mozilla or whatever).
;;
;; The default symbol for M-x gtk-lookup-symbol is found using info-look, so
;; it's context-sensitive and follows the major mode.  Hyphens "-" in a
;; symbol are changed to underscores "_", so that lookups of guile-gtk
;; scheme functions like gdk-keyval-name are converted to C names like
;; gdk_keyval_name.

;;; Install:

;; Put gtk-look.el somewhere in your `load-path', and in your .emacs put
;;
;;     (autoload 'gtk-lookup-symbol "gtk-look" nil t)
;;
;; This makes M-x gtk-lookup-symbol available, but you will probably want to
;; bind it to a key.  C-h C-j is one possibility, being close to C-h C-i for
;; `info-lookup-symbol'.  For instance to do that globally,
;;
;;     (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol)

;;; Emacsen:

;; Designed for GNU Emacs 21.3.

;;; History:

;; Version 1 - the first version.
;; Version 2 - correction to usual .devhelp file locations.
;; Version 3 - recognise devhelp2 format.
;; Version 4 - home page link, more parens on funcs in index, fix lookup
;;             done from within an existing browser buffer.


;;; Code:

(require 'info-look)

(defgroup gtk-lookup nil
  "GTK/GNOME documentation lookup."
  :prefix "gtk-lookup-"
  :group 'languages
  :link  '(url-link :tag "gtk-look home page"
                    "http://www.geocities.com/user42_kevin/gtk-look/index.html"))

;; this is an alist so it can be passed to completing-read
(defvar gtk-lookup-cache 'uninitialized
  "Cache of targets for `gtk-lookup-symbol'.
The current format is (NAME . (BASE . LINK)), where NAME is a symbol string,
and BASE and LINK will be concatenated to make a URL.  BASE and LINK are
separate to save a bit of memory, since the BASE part is shared by all the
links in one manual.

When `gtk-lookup-cache' is not yet initialized the value is the symbol
`uninitialized'.")

(defun gtk-lookup-reset ()
  "Discard data cached for `gtk-lookup-symbol'.
This can be used to get newly installed documents recognised."
  (interactive)
  (setq gtk-lookup-cache 'uninitialized))

(defcustom gtk-lookup-devhelp-indices
  '(;; usual place (see /usr/share/doc/devhelp/README
    "/usr/share/gtk-doc/html/*/*.devhelp*"
    ;; possible locally installed stuff
    "/usr/local/share/gtk-doc/html/*/*.devhelp*")
  "List of devhelp index files containing GTK/GNOME documentation.
Shell wildcards can be used, and gzip compressed files are allowed."
  :set (lambda (sym val) (custom-set-default sym val) (gtk-lookup-reset))
  :type '(repeat string)
  :group 'gtk-lookup)

(defun gtk-lookup-cache-init ()
  "Initialize `gtk-lookup-cache', if not already done."
  (when (eq gtk-lookup-cache 'uninitialized)
    (setq gtk-lookup-cache nil)
    (let ((found nil)
          (old-auto-compress -1))
      (auto-compression-mode 1)
      (unwind-protect
          (with-temp-buffer

            (let ((filelist
                   ;; file-truename and delete (below) eliminate duplicate
                   ;; filenames arising from symlinks or repeat matches by
                   ;; wildcards in gtk-lookup-devhelp-indices
                   (mapcar 'file-truename
                           (apply 'append
                                  (mapcar 'file-expand-wildcards
                                          gtk-lookup-devhelp-indices)))))

              ;; if we've got both .devhelp and .devhelp2 files, then only
              ;; look in the .devhelp2
              (mapcar (lambda (filename)
                        (if (string-match "\\(.*\\)\\.devhelp2\\(\\.gz\\)\\'"
                                          filename)
                            (let ((base (match-string 1 filename)))
                              (setq filelist
                                    (delete (concat base ".devhelp")
                                            filelist))
                              (setq filelist
                                    (delete (concat base ".devhelp.gz")
                                            filelist)))))
                      filelist)

              (while filelist
                (let ((filename (car filelist)))
                  (message "Processing %s" filename)
                  (setq found t)
                  (let ((base (concat "file:" (file-name-directory filename))))
                    ;; In Emacs 21.3 jka-compr doesn't erase the buffer
                    ;; properly under the "replace" argument to
                    ;; insert-file-contents, so use erase-buffer instead.
                    ;; (This is fixed for Emacs 22, or whatever the next
                    ;; release of the mainline will be.)
                    (erase-buffer)
                    (insert-file-contents filename)

                    ;; "<function ...>" is devhelp format 1
                    (while (re-search-forward "<function name=\"\\(struct \\|union \\|enum \\)?\\([a-zA-Z0-9_-]+\\)[ ()]*\" link=\"\\([^\"]+\\)\"/>"
                                              (point-max) t)
                      (setq gtk-lookup-cache
                            (cons (cons (match-string 2)
                                        (cons base (match-string 3)))
                                  gtk-lookup-cache)))

                    ;; "<keyword ...>" is devhelp format 2
                    ;; the name field can be
                    ;;     "enum foo"
                    ;;     "foo()"
                    ;;     "foo ()"
                    (goto-char (point-min))
                    (while (re-search-forward "<keyword type=\"\\(enum\\|function\\|macro\\|struct\\|typedef\\|union\\|variable\\)\" name=\"\\([^\"]*\\)\" link=\"\\([^\"]+\\)\""
                                              (point-max) t)
                      (let ((name (match-string 2))
                            (link (match-string 3)))

                        ;; lose leading "enum" or "union" from name
                        (if (string-match "\\`\\(enum\\|struct\\|union\\) \\(.*\\)"
                                          name)
                            (setq name (match-string 2 name)))

                        ;; lose trailing "()" or " ()" on name for functions
                        (if (string-match "\\`\\(.*?\\) ?()\\'" name)
                            (setq name (match-string 1 name)))

                        (setq gtk-lookup-cache
                              (cons (cons name (cons base link))
                                    gtk-lookup-cache)))))

                  (setq filelist (delete filename filelist))))))
        (auto-compression-mode old-auto-compress))
      (unless found
        (message "No devhelp files found")))))

(defun gtk-lookup-canonicalize-symbol (str)
  "Return canonicalized symbol STR, which means \"-\" becomes \"_\".
This is intended for use with guile-gtk where Scheme names are like
gdk-keyval-to-lower, which should convert to gdk_keyval_to_lower for the
devhelp C function indices."
  (if str
      (while (string-match "-" str)
        (setq str (replace-match "_" t t str))))
  str)

(defun gtk-lookup-symbol-interactive-arg ()
  "Symbol argument reading for interactive `gtk-lookup-symbol'.
Return a list (SYMBOL) which is the user-selected symbol name."
  (gtk-lookup-cache-init)
  (let* ((mode (cond ((info-lookup->mode-value 'symbol
                                               (info-lookup-select-mode))
                      info-lookup-mode)
                     ((info-lookup-change-mode 'symbol))))
         (default (gtk-lookup-canonicalize-symbol
                   (info-lookup-guess-default 'symbol mode)))
	 (completion-ignore-case (info-lookup->ignore-case 'symbol major-mode))
	 (enable-recursive-minibuffers t)
	 (symbol (gtk-lookup-canonicalize-symbol
                  (completing-read
                   (if default
                       (format "Describe symbol (default %s): " default)
                     "Describe symbol: ")
                   gtk-lookup-cache nil nil nil
                   ;; share the info-look history
                   'info-lookup-history default))))
    (list (or symbol default ""))))

(defun gtk-lookup-symbol (symbol)
  "Lookup Gtk/Gnome documentation for SYMBOL.
When invoked interactively, a SYMBOL is prompted for, with completions from
among those known through `gtk-lookup-devhelp-indices'.

Display uses `browse-url', in an \"other-window\" if that function uses a
window (as opposed to spawning a new frame or something)."

  (interactive (gtk-lookup-symbol-interactive-arg))
  (gtk-lookup-cache-init)
  (let ((entry (or (assoc symbol gtk-lookup-cache)
                   (assoc-ignore-case symbol gtk-lookup-cache))))
    (or entry
        (error "Unknown symbol %s" symbol))
    (gtk-lookup-browse-url-other-window (concat (cadr entry) (cddr entry)))))

(defun gtk-lookup-browse-url-other-window (url)
  "`browse-url' but in an \"other-window\", if it uses a window."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((orig-buffer (current-buffer)))
    (save-selected-window
      (save-current-buffer
        (let ((win-conf  (current-window-configuration))
              (dummy-buf (get-buffer-create "*gtk-look-dummy-buffer*")))
          (switch-to-buffer-other-window dummy-buf)
          (browse-url url)

          ;; if browse-url leaves the current buffer equal to dummy-buf,
          ;; then it's some external browser, not buffer based, so put the
          ;; windows back how they were
          ;;
          ;; if browse-url leaves the current buffer equal to our original
          ;; buffer then we're in a buffer based browser and doing a browse
          ;; from within that browser buffer; in this case put the windows
          ;; back how they were (don't split to two views of the browser
          ;; buffer)
          ;;
          (if (or (eq (current-buffer) dummy-buf)
                  (eq (current-buffer) orig-buffer))
              (set-window-configuration win-conf))

          (kill-buffer dummy-buf))))))

(defun gtk-lookup-browse-url-of-buffer-other-window (buffer)
  "`browse-url-of-buffer' but in an \"other-window\", if it uses a window."
  (let ((orig-buffer (current-buffer)))
    (save-selected-window
      (save-current-buffer
        (let ((win-conf  (current-window-configuration))
	      (dummy-buf (get-buffer-create "*gtk-look-dummy-buffer*")))
	  (switch-to-buffer-other-window dummy-buf)
          (browse-url-of-buffer buffer)

          ;; if browse-url leaves the current buffer equal to dummy-buf,
          ;; then it's some external browser, not buffer based, so put the
          ;; windows back how they were
          ;;
          ;; if browse-url leaves the current buffer equal to our original
          ;; buffer then we're in a buffer based browser and doing a browse
          ;; from within that browser buffer; in this case put the windows
          ;; back how they were (don't split to two views of the browser
          ;; buffer)
          ;;

          (if (or (eq (current-buffer) dummy-buf)
                  (eq (current-buffer) orig-buffer))
              (set-window-configuration win-conf))
	  (kill-buffer dummy-buf))))))

(defun display-link (lst)
  (insert "<a href=\"" (cadr lst) (cddr lst))
  (insert "\">")
  (insert (car lst))
  (insert "</a><br>"))

(defun gtk-lookup-search-symbol (symbol)
  "Search Gtk/Gnome documentation for SYMBOL."
  (interactive "sSymbol to search: ")
  (gtk-lookup-cache-init)
  (let ((matches (remove-if-not (lambda (elt)
				  (string-match 
				   (concat ".*" symbol ".*")
				   (car elt)))
				gtk-lookup-cache))
	(results-buffer (get-buffer-create "*gtk-lookup-results*")))
    (save-excursion
      (switch-to-buffer-other-window results-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
      (insert "<html>")
      (mapcar 'display-link matches)
      (insert "</html>")
      (goto-char 1)
      (setq buffer-read-only t)
      (browse-url-of-buffer results-buffer)
      (kill-buffer results-buffer))))

(provide 'gtk-look)

;;; gtk-look.el ends here
