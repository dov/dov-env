;;; org-jira-hyperlink.el --- Define jira hyperlinks

(require 'org)
(org-link-set-parameters "jira" :follow #'org-jira-hyperlink-open)

(defun org-jira-hyperlink-open (id)
  "Visit the jira link"
  (let* ()
    (browse-url (concat "https://xjetsw.atlassian.net/browse/" id))))

(provide 'org-jira-hyperlink)
;;; org-jira-hyperlink.el ends here
