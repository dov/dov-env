;;; xjet-wiki-open.el - Support for links to the xjet wiki in Org

(require 'org)

(org-add-link-type "xjwiki" 'xjet-wiki-open)

(defun xjet-wiki-open (path)
  "Visit the xjet wiki page on PATH.
PATH should be a topic that can be thrown at the wikipedia comwikipediad."
  (browse-url (concat "http://swteam:8080/index.php/" path)))

(provide 'xjet-wiki-open)

;;; xjet-wiki-open.el ends here
