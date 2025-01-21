(setq load-path (append
                 (list
                  (concat emacs-git "org-extra")
                  (concat emacs-git "packages/org-bullets-20200317.1740")
                  (concat emacs-git "packages/org-9.6.1")
                  (concat emacs-git "packages/org-present-20220806.1847")
                  )
                 load-path))
(use-package
  org-bullets)
(use-package
  org)
(use-package
  org-present)
 
;; Compatibility
(defun org-font-lock-ensure (x y)
  (font-lock-ensure x y))

;; Bug workaround for Tab not working in tables
(defalias 'org-font-lock-ensure
        (if (fboundp 'font-lock-ensure)
            #'font-lock-ensure
          (lambda (&optional _beg _end)
            (with-no-warnings (font-lock-fontify-buffer)))))

(defun my-org-present-mode-hook ()
  ;;; Restore the left and right keys
  (define-key org-present-mode-keymap [right]         'right-char)
  (define-key org-present-mode-keymap [left]          'left-char)
  (define-key org-present-mode-keymap [(alt right)]   'org-present-next)
  (define-key org-present-mode-keymap [(alt left)]    'org-present-prev)
  (define-key org-present-mode-keymap [(control right)]   'org-present-next)
  (define-key org-present-mode-keymap [(control left)]    'org-present-prev)

  (org-present-big)
  (org-display-inline-images)
  (setq org-src-fontify-natively t)
  (font-lock-update)

  ; Set cursor color to green
  (set-cursor-color "#008f00")

  (setq x-pointer-shape x-pointer-top-left-arrow)
  (set-mouse-color "#008f00")

  )

(defun turn-last-into-embedded-link ()
  (interactive)
  (set-mark-command nil)
  (pcre-re-search-backward ":")
  (forward-char)
  (copy-region-as-kill (point) (mark))
  (pcre-re-search-backward "\\s\\S")
  (forward-char)
  (insert "[[")
  (exchange-point-and-mark)
  (insert "][")
  (yank)
  (insert "]]")
  )

(defun my-org-hook ()
  (load "org-git-hyperlink.el")
  (load "org-comeet-hyperlink.el")
  (load "org-clickup-hyperlink.el")
  (load "org-redmine-hyperlink.el")
  (load "org-jira-hyperlink.el")
  (load "org-pydoc-hyperlink.el")
  (load "org-jira-hyperlink.el")
  (load "org-wp.el")
  (load "org-bullets.el")
  (load "ox-slidy.el")
  (load "screenshot.el")
  (load "org-man.el")
  (require 'ox-mediawiki)
  (require 'ox-reveal)

   (autoload 'org-present "org-present" nil t)
   (add-hook 'org-present-mode-hook 'my-org-present-mode-hook)
   (add-hook 'org-present-mode-quit-hook
             (lambda ()
               (org-present-small)
               (org-remove-inline-images)))

  (local-set-key [(control c) (control ?.)] 'org-time-stamp)
  (local-set-key "\M-I" 'org-toggle-iimage-in-org)
  (local-set-key "\M-R" 'refresh-iimages)
  (local-set-key "\C-c\M-c" 'org-screenshot)
  (local-set-key "\C-c\C-pa" 'org-show-all)
  (local-set-key "\C-c\C-ph" 'org-hide-all)
  (local-set-key "\C-c\C-pe" 'org-toggle-emphasis-markers)
  (local-set-key "\C-c\C-pp" 'org-toggle-pretty-entities)
  (local-set-key "\C-c\C-pi" 'org-toggle-iimage-in-org)
  (local-set-key "\C-c\C-pl" 'org-toggle-link-display)
  (local-set-key "\C-c\C-x," 'org-insert-structure-template)
  (local-set-key "\C-c\C-x\C-e" 'org-export-dispatch)
  (local-set-key "\C-co" 'org-mark-ring-goto)
  (local-set-key "\C-c]" 'turn-last-into-embedded-link)
  (local-set-key "\C-c\C-e" 'goto-compilation-directory-and-compile)

  (local-set-key (kbd "C-M-=") 'calc-eval-region)
  (local-set-key (kbd "C-c M-/") 'toggle-date-iso)

  ;; variable pitch mode makes emacs rescale!
  (variable-pitch-mode t)
  (emojify-mode)
  (set-face-attribute 'org-table nil :family my-fixed-font)
  (set-face-attribute 'org-checkbox nil :family my-fixed-font)
  (set-face-attribute 'org-block nil :family my-fixed-font)
  (set-face-attribute 'org-verbatim nil :family my-fixed-font :foreground "green4")
  (setq truncate-lines nil)
  (setq org-export-allow-bind-keywords t)
  (setq org-html-doctype "html5")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  ; Use this in order not to show subscript and superscripts. Curlies still forces sub and superscript behavior.
  (setq org-use-sub-superscripts nil)
  (if (or (string= window-system "x") (string= window-system "w32"))
      (progn
        (setq org-bullets-bullet-list
          '("►"
            "•"
            "•"
            "•"
            "•"
            "•"
            "•"
            ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
            ;;; Small
            ;; ► • ★ ▸
            )
          )
        (org-bullets-mode)
        )
      (setq org-bullets-bullet-list
        '("*" "*" "*" "*" "*" "*" "*"
          ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
          ;;; Small
          ;; ► • ★ ▸
      )))

  (setq org-hide-emphasis-markers nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-fontify-natively nil)
  (xmsi-mode)
  (org-toggle-pretty-entities)
  (setq bidi-paragraph-direction nil)
  (setq org-export-html-postamble nil)
  (setq org-export-html-validation-link "")
    
  ;; Use journal theme if requested
  (setq org-entities-user '(
    ("models" "\\models" t "&8872;" "[models]" "models" "⊨")
    ("indf" "{\bf 1}" t "&#120128;" "[indf]" "indf" "𝟙")
    ("ell" "\\ell" t "&#2113;" "[ell]" "indf" "ℓ")
    ))
;  (require 'org-table)

  ; Don't use company mode in "text" buffers
;  (company-mode -1)

  ;; Customize colors
;  (require 'cl)   ; for delete*
;  (setq org-emphasis-alist
;        (cons '("+" '(:strike-through t :foreground "gray30"))
;              (delete* "+" org-emphasis-alist :key 'car :test 'equal))))

  (custom-set-variables
   '(org-emphasis-alist
     (quote
      (("*" bold)
       ("/" italic)
       ("_" underline)
       ("=" org-verbatim verbatim)
       ("~" org-code verbatim)
       ("+" (:strike-through t :foreground "gray30")))))
   )
  )

(add-hook 'org-mode-hook 'my-org-hook)

(if (string-match "x86_64-w64-mingw32" system-configuration)
    (progn
      (setq org-file-apps
            (append
;             '(("png" . "c:/progra~2/IrfanView/i_view32.exe %s"))
             '(("doc" . "\"c:/Program Files (x86)/OpenOffice.org 3/program/soffice.exe\" %s"))
             '(("pdf" . "\"c:/Program Files (x86)/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe\" %s"))
;             org-file-apps
             ))
      )
  (progn 
    (setq org-file-apps
          (append
           '(("doc" . "libreoffice %s"))
           '(("docx" . "libreoffice %s"))
           '(("odt" . "libreoffice %s"))
           '(("png" . "feh %s"))
           '(("pdf" . "evince %s"))
           '(("svg" . "inkscape %s"))
           '(("net" . "/usr/local/samiam/runsamiam %s"))
           '(("xcf" . "gimp %s"))
           '(("giv" . "giv %s"))
           '(("stl" . "qtfern %s"))
           '(("doc" . "libreoffice -norestore %s"))
           '(("docx" . "libreoffice -norestore %s"))
           '(("odt" . "libreoffice -norestore %s"))
           '(("gnumeric" . "gnumeric %s"))
           '(("html" . "firefox %s"))
           '(("xopp" . "xournalpp %s"))
           '(("ora" . "krita %s"))
           '(("kra" . "krita %s"))
           '(("blend" . "blender %s"))
           '(("FCStd" . "FreeCAD %s"))
           org-file-apps))))

(setq org-src-lang-modes
      '(("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . fundamental)
        ("perl" . cperl)
        ("python" . python)
        ("lua" . lua)
        ))

(setq org-latex-packages-alist
      '(
;        (""     "grffile"   t)
        (""     "svg"   t)
        ))


(setq org-latex "lualatex")
(setq org-latex-pdf-process '(
  "%latex --shell-escape -interaction nonstopmode -output-directory %o %f"
  "%latex --shell-escape -interaction nonstopmode -output-directory %o %f"
  "%latex --shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun my-org-iimage-refresh ()
  (interactive)
  (redisplay t)
  (my-iimage-mode-buffer 1 'refresh)
  (redisplay t))

(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (clear-image-cache nil)
  (if iimage-mode
      (set-face-underline 'org-link t)
      (set-face-underline 'org-link nil))
  (call-interactively 'iimage-mode))

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

(add-hook 'org-babel-after-execute-hook 'my-org-iimage-refresh)

;(org-crypt-use-before-save-magic)
;
;(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;;; GPG key to use for encryption
;;; Either the Key ID or set to nil to use symmetric encryption.
;;(setq org-crypt-key "C1CC1169")  ;; My secrets
;(setq org-crypt-key "95B648B1")  ;; Dov Org Secrets
 
; This is a bug work around
(defun org-element-cache-reset (&optional all) (interactive))
(setq org-babel-sh-command "zsh")

(eval-after-load 'org-export-latex
  '(progn
     (add-to-list 'org-latex-classes
    '("moderncv"
      "\\documentclass[11pt,a4paper,sans]{moderncv}"
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}" . "\\paragraph*{%s}")
       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 ;; beamer class, for presentations
     (add-to-list 'org-latex-classes
    '("beamer"
       "\\documentclass[11pt]{beamer}\n
        \\mode<{{{beamermode}}}>\n
        \\usetheme{{{{beamertheme}}}}\n
        \\usecolortheme{{{{beamercolortheme}}}}\n
        \\beamertemplateballitem\n
        \\setbeameroption{show notes}
        \\usepackage[utf8]{inputenc}\n
        \\usepackage[T1]{fontenc}\n
        \\usepackage{hyperref}\n
        \\usepackage{color}
        \\usepackage{listings}
        \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
    frame=single,
    basicstyle=\\small,
    showspaces=false,showstringspaces=false,
    showtabs=false,
    keywordstyle=\\color{blue}\\bfseries,
    commentstyle=\\color{red},
    }\n
        \\usepackage{verbatim}\n
        \\institute{{{{beamerinstitute}}}}\n          
         \\subject{{{{beamersubject}}}}\n"
  
       ("\\section{%s}" . "\\section*{%s}")
       
       ("\\begin{frame}[fragile]\\frametitle{%s}"
         "\\end{frame}"
         "\\begin{frame}[fragile]\\frametitle{%s}"
         "\\end{frame}")))
  ))

;; mapping between languages and their major mode  (in Emacs)
;(setq org-export-htmlized-org-css-url "/home/dov/tmp/org-mode/ORGWEBPAGE/org.css")
;(load "htmlize")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
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
   (lua . t)
   )) 
(setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")

;(load "org-exp-blocks")
;(load "org-mw")
(defun my-org-confirm-babel-evaluate (lang body)
  (not
   (or (string= lang "ditaa")
       (string= lang "dot"))))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(provide 'init-org)
