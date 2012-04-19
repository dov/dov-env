;;; org-wikipedia.el - Support for links to wikipediapages in Org

(require 'org)

(org-add-link-type "wp" 'org-wikipedia-open)

(defun org-wikipedia-open (path)
  "Visit the wikipediapage on PATH.
PATH should be a topic that can be thrown at the wikipedia comwikipediad."
  (browse-url (concat "http://en.wikipedia.org/wiki/" path)))

(provide 'org-wikipedia)

;;; org-wikipedia.el ends here