;;; org-git-hyperlink.el --- Define git hyperlinks

;; Copyright (C) 2012 Free Software Foundation, Inc.
;;
;; Author:  <dov.grobgeld@gmail.com>
;; Maintainer:  <dov.grobgeld@gmail.com>
;; Created: 19 Apr 2012
;; Version: 0.01
;; Keywords

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-git-hyperlink)

;;; Code:

;;; org-learning.el - Support for my learning git

(require 'org)

(org-add-link-type "git" 'org-git-hyperlink-open)

(defun org-git-hyperlink-open (path)
  "Visit the file in learning-git"
  (let* ((parts (split-string path "::"))
         (repo-name (car parts))
         (filename (cadr parts))
         (repo (gethash repo-name my-git-repos))
         )
    (if (eq repo nil) (error (concat "No such repo: " repo-name)))
    (git-find-file-in-repo repo filename)))
;;; org-learning.el ends here

(provide 'org-git-hyperlink)
;;; org-git-hyperlink.el ends here
