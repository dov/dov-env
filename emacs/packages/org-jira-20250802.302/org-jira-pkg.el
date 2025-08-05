;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-jira" "20250802.302"
  "Syncing between Jira and Org-mode."
  '((emacs   "24.5")
    (cl-lib  "0.5")
    (request "0.2.0")
    (dash    "2.14.1"))
  :url "https://github.com/ahungry/org-jira"
  :commit "c7259fa13b8eb99a988b06cdaf3a04f9109904de"
  :revdesc "c7259fa13b8e"
  :keywords '("ahungry" "jira" "org" "bug" "tracker")
  :maintainers '(("Matthew Carter" . "m@ahungry.com")))
