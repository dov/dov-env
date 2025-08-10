;;; org-jira-hyperlink.el --- Define jira hyperlinks

(require 'org)
(org-link-set-parameters "jira" :follow #'org-jira-hyperlink-open)

;(defun org-jira-hyperlink-open (id)
;  "Visit the jira link"
;  (let* ()
;    (browse-url (concat "https://xjetsw.atlassian.net/browse/" id))))

(defun org-jira-hyperlink-open (id)
  "Visit the Jira link. If ID starts with '?filter=', use the filter URL. Otherwise, visit the issue page."
  (let ((url (if (string-prefix-p "?filter=" id)
                 (concat "https://xjetsw.atlassian.net/issues/" id)
               (concat "https://xjetsw.atlassian.net/browse/" id))))
    (browse-url url)))

(provide 'org-jira-hyperlink)
;;; org-jira-hyperlink.el ends here
