;; Configure eglot
(use-package eglot
  :ensure t
  :config
  ;; Optional: Add any additional configuration for eglot here

  (define-key completion-in-region-mode-map (kbd "C-n") 'minibuffer-next-completion)
  (define-key completion-in-region-mode-map (kbd "C-p") 'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-<return>") 'minibuffer-choose-completion)

  ;; Dov - I prefer to turn on eglot manually at the moment

;  (add-hook 'python-mode-hook 'eglot-ensure)
  ;; Add more hooks for other languages as needed
)

; This should possibly be moved to a utility library
(defun get-regexp-group-content (input-string regexp group-number)
  "Return the content of GROUP-NUMBER from INPUT-STRING matching REGEXP.
GROUP-NUMBER is 1-based."
  (if (string-match regexp input-string)
      (match-string group-number input-string)
    (error "No match found or group number out of range")))

;; Note: When outputting the response string string, the following conversion is made in the message
;; buffer "`" → "‘"!
(defun my-eglot-deduce-auto ()
  "Conver `auto` to the eglot resolved codecode."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (current-word (thing-at-point 'word t)))
    (if (and bounds (string= current-word "auto"))
        (let* ((start (car bounds))
               (end (cdr bounds))
               (response (jsonrpc-request
                          (eglot--current-server-or-lose)
                          :textDocument/hover
                          (eglot--TextDocumentPositionParams))))
          (if-let ((contents (plist-get response :contents)))
              (let ((type (cond
                           ((stringp contents) contents)
                           ((and (listp contents) (stringp (plist-get contents :value)))
                            (plist-get contents :value))
                           ((and (listp contents) (stringp (cdr (assq 'value contents))))
                            (cdr (assq 'value contents)))
                           (t nil))))
                (if type
                    (let ((cleaned-type
                           (get-regexp-group-content type "```cpp\n\\(.*\\)\n```" 1)))
                      (delete-region start end)
                      (insert cleaned-type)
                      )
                  (message "Could not resolve type for `auto`.")))
            (message "Could not resolve type for `auto`.")))
      (message "Not on an `auto` keyword."))))

;; TBD - Not the best keybinding, but it works and is easily accessible
(global-set-key (kbd "C-c M-t") 'my-eglot-deduce-auto)


;;; testing
;; (setq s "### type-alias ‘auto‘  
;; 
;; ---
;; A filesystem path  
;; @ingroup filesystem  
;; @headerfile filesystem  
;; @since C++17  
;; 
;; ---
;; ‘‘‘cpp
;; fs::path
;; ‘‘‘
;; ")
;; (get-regexp-group-content s "‘‘‘cpp\n\\(.*\\)\n‘‘‘" 1)

;; Don't let eglot reformat my code!
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (remove-hook 'post-self-insert-hook #'eglot--post-self-insert-hook t)))
