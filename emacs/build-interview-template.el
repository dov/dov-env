(defun build-interview-template ()
  """Build an interview template from the line in the emacs buffer"""
  (interactive)
  (save-excursion 
    (setq bol (point))
    (pcre-re-search-forward " - (comeet:.*?) - (.*)$")
    (let ((cm (match-string 1))
          (name (whitespace-strip (match-string 2))))
      (delete-region bol (point))
      (insert (concat "    - file:Interview-" name ".org"))
      (beginning-of-line)
      (next-line)

      ;; create the new interview file. Will be created with the org template
      (find-file (concat "Interview-" name ".org"))
      (end-of-buffer)
      (insert
       (concat
        (format-time-string "<%Y-%m-%d %a>\n")
        "** Personal\n"
        "  - " cm "\n"
        "\n"
        "** Preinterview impressions\n"
        "  - ")))))

(provide 'build-interview-template)
