; A few utilities for dealing with excel input

(defun xlsx-copy-line-to-csv ()
  "Process the current line as tab-separated fields:
1. Embed fields starting with [A-Z] (case insensitive) in double quotes.
2. Remove commas from numeric fields with embedded commas.
3. Convert fields ending with a percent sign into fraction values (not quoted).
4. Replace tabs with commas.
5. Trim leading and trailing spaces from the final result."
  (interactive)
  (let* ((line (thing-at-point 'line t)) ; Get the current line as a string
         (fields (split-string line "\t")) ; Split the line into fields by tabs
         (processed-fields
          (mapcar
           (lambda (field)
             (setq field (string-trim field)) ; Trim spaces around the field
             (cond
              ;; 1. Embed fields starting with [A-Z] (case insensitive) in double quotes
              ((string-match-p "^[A-Za-z]" field)
               (format "\"%s\"" field))
              ;; 2. Remove commas from numeric fields with embedded commas
              ((string-match-p "^[0-9,]+$" field)
               (replace-regexp-in-string "," "" field))
              ;; 3. Convert fields ending with a percent sign into fraction values
              ((string-match "\\([0-9.]+\\)%$" field)
               (let ((percent-value (string-to-number (match-string 1 field))))
                 (format "%.4f" (/ percent-value 100.0))))
              ;; Default: Leave the field unchanged
              (t field)))
           fields)))
    ;; Replace the current line with the processed fields joined by commas
    (delete-region (line-beginning-position) (line-end-position))
    (insert (string-trim (string-join processed-fields ", ")))))
