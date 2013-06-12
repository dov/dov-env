;;; pretty-mode.el --- Pretty-print lambdas and other functions

;; Author: Mark Triggs <mst@dishevelled.net>, Dov Grobgeld<dov.grobgeld@gmail.com>
;; 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This idea is *completely* stolen from Luke Gorrie's pretty-lambda.el.  I'm
;; just curious to see how easily it can be done with font-locking alone.
;;
;; Modified by Dov Grobgeld to support changing lots of variables
;; to make python look like APL. This is probably a very bad idea, but
;; quite fun. :-)


;;; Code:

(setq pretty-symbols (make-hash-table :test 'equal))

;; Setup the display table various matchings
(puthash "enumerate" "€" pretty-symbols)
(puthash "lambda"    "λ" pretty-symbols)
(puthash "for"       "∀" pretty-symbols)
(puthash "in"        "∈" pretty-symbols)
(puthash "range"     "ℜ" pretty-symbols)

(defun hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys
  )
)

(defun join-list (my-list glue)
  "Join a string list with glue between the list items"
   (let ((joined-list (car my-list)))
     (mapcar (lambda (v) (setq joined-list (concat joined-list glue v))) (cdr my-list))
     joined-list))

;; build the regex
(setq pretty-regex
      (concat
       "\\b\\("
       (join-list (hash-keys pretty-symbols) "\\|")
       "\\)\\b"
       ))
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun is-point-in-string ()
  """whether the current position is in a string"""
  (interactive)
  (nth 3 (syntax-ppss)))

(defun is-point-in-comment ()
  """whether the current position is in a string"""
  (interactive)
  (nth 4 (syntax-ppss)))

(defun pretty-fontify (beg end)
  (save-excursion
    (pretty-unfontify beg end)
    ;; Mark incorrect uses of spacing.
    (goto-char beg)
    (while (re-search-forward pretty-regex end t)
      (let ((o (car (overlays-at (match-beginning 1)))))
        (if (and (not (and o (eq (overlay-get o 'type) 'pretty)))
                 (not (or (is-point-in-comment)
                          (is-point-in-string)))
                 )
          (let* ((overlay (make-overlay (match-beginning 1) (match-end 1)))
                 (match-text (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
            (message (get-char-property (match-beginning 1) 'read-face-name))
            (overlay-put overlay 'type 'pretty)
            (overlay-put overlay 'evaporate t)
            (overlay-put overlay 'display (gethash match-text pretty-symbols))))))))

(defun pretty-unfontify (beg end)
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'pretty)
              (delete-overlay o)))
        (overlays-in beg end)))


(define-minor-mode pretty-mode
  "Indicate where only a single space has been used."
  nil " λ" nil
  (cond ((not pretty-mode)
         (jit-lock-unregister 'pretty-fontify)
         (pretty-unfontify (point-min) (point-max)))
        (t (pretty-fontify (point-min) (point-max))
           (jit-lock-register 'pretty-fontify))))

(provide 'pretty-mode)
;;; pretty-mode.el ends here
