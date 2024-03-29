;; Source: https://systemcrafters.net/emacs-tips/presentations-with-org-present/

;;; Configure Package Archives -----------------------------

;; Initialize package sources
(require 'package)

;; org-present is in the "nongnu" package archive.  This line isn't needed in
;; Emacs 28!
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

;; This will be needed if you decide to use doom-themes!
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Set up package.el and refresh package archives if it hasn't been done yet
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;; Basic Appearance ---------------------------------------

;; More minimal UI
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Let the desktop background show through
(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(require 'color)
(set-face-attribute 'org-block nil :background "gray90")
(set-face-attribute 'org-level-1 nil :foreground "red3")

;;; Theme and Fonts ----------------------------------------

;; Install doom-themes
(unless (package-installed-p 'doom-themes)
  (package-install 'doom-themes))

;; Load up doom theme
;(load-theme 'doom-palenight t)
;(load-theme 'doom-acario-light t)

;; Set reusable font name variables
;(defvar my/fixed-width-font "JetBrains Mono"
;  "The font to use for monospaced (fixed width) text.")
(defvar my/fixed-width-font "InconsolataDov"
  "The font to use for monospaced (fixed width) text.")

;(defvar my/variable-width-font "Iosevka Aile"
;  "The font to use for variable-pitch (document) text.")
(defvar my/variable-width-font "Candara"
  "The font to use for variable-pitch (document) text.")

;; NOTE: These settings might not be ideal for your machine, tweak them as needed!
;(set-face-attribute 'default nil :font my/fixed-width-font :weight 'light :height 180)
(set-face-attribute 'fixed-pitch nil :font my/fixed-width-font :weight 'light :height 0.7)
(set-face-attribute 'variable-pitch nil :font my/variable-width-font :weight 'light :height 1.3)

;;; Org Mode Appearance ------------------------------------

;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font my/variable-width-font :weight 'medium :height (cdr face)))

(set-face-attribute 'org-level-1 nil :font my/variable-width-font :weight 'bold :height 1.3)
(set-face-attribute 'org-level-2 nil :font my/variable-width-font :weight 'bold :height 1.15)


;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil
                    :font my/variable-width-font
                    :weight 'bold
                    :height 1.3
                    :foreground "red3"
                    )

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;;; Centering Org Documents --------------------------------

;; Install visual-fill-column
(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)

;;; Org Present --------------------------------------------

;; Install org-present if needed
(unless (package-installed-p 'org-present)
  (package-install 'org-present))

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-fold-show-all)

  ;; Show only direct subheadings of the slide but don't expand them
;  (org-show-children)
  )

(defun my/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.25) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1)

  ;; Show font highlighted text
  (setq org-src-fontify-natively 1)
  (font-lock-update)
  )

(defun my/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; Turn on variable pitch fonts in Org Mode buffers
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(provide 'dov-emacs-present)
