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

(defun run-eperl-maybe-and-preview-html()
  "Runs eperl for phtml files and then runs the preview."
  (interactive)
  (if (string-match "\\.phtml" buffer-file-name)
      (let ((htmlfilename (concat (file-name-sans-extension buffer-file-name) ".html")))
	(if (shell-command (concat "eperl " buffer-file-name "> " htmlfilename))
	    (browse-url-of-file htmlfilename)))
    (browse-url-of-file)))

(global-set-key [(f2)] 'perl-pe-region)

(defun shell-perl-on-buffer ()
  "Send the current (python) buffer to be evaluated by the python shell"
  (interactive)
  (async-shell-command-on-buffer "perl" "pl"))


;; Perl mode stuff
(autoload 'cperl-mode "cperl-mode" nil t)
(setq interpreter-mode-alist (append interpreter-mode-alist
				     '(("miniperl" . perl-mode))))

(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook 'my-perl-mode-hook)

(provide 'init-perl)
