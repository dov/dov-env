;;; org-redmine-hyperlink.el --- Define redmine hyperlinks

(require 'org)
(org-link-set-parameters "redmine" :follow #'org-redmine-hyperlink-open)

(defun org-redmine-hyperlink-open (id)
  "Visit the file in learning-git"
  (let* ()
    (browse-url (concat "http://swteam/issues/" id))))

(provide 'org-redmine-hyperlink)
;;; org-redmine-hyperlink.el ends here
