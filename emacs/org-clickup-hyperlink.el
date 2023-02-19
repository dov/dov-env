;;; org-clickup-hyperlink.el --- Define clickup hyperlinks

(require 'org)
(org-link-set-parameters "clickup" :follow #'org-clickup-hyperlink-open)

(defun org-clickup-hyperlink-open (id)
  "Visit the clickup link"
  (let* ()
    (browse-url (concat "https://app.clickup.com/" id))))

(provide 'org-clickup-hyperlink)
;;; org-clickup-hyperlink.el ends here
