(defun qt-auto-include ()
  "Scan Eglot/Flymake diagnostics for unknown Qt type names and insert matching #include lines.

Steps:
1. If Eglot is not active in the current buffer, warn and exit.
2. Use Flymake diagnostics (populated by Eglot) to find errors whose message
   mentions \"Unknown type name\" (clang wording).
3. For each such diagnostic, heuristically detect Qt type names
   (symbols starting with 'Q').
4. Find the first empty line following initial #include statements at the top
   of the file; if none are found, use the first line of the file.
5. Insert one #include line per *unique* Qt symbol at that position, skipping
   any that are already present in the buffer.

Headers are assumed to match the symbol name directly, e.g. QString -> <QString>."
  (interactive)
  ;; 1. Check if eglot is active
  (if (not (eglot-managed-p))
      (message "qt-auto-include: Eglot is not active in this buffer.")
    (let ((qt-types (qt-auto-include--collect-qt-types-from-flymake)))
      (if (null qt-types)
          (message "qt-auto-include: No unknown Qt types found.")
        (save-excursion
          (let ((insert-pos (qt-auto-include--find-include-position)))
            (goto-char insert-pos)
            (beginning-of-line)
            ;; Ensure we’re on an empty line for neat formatting
            (unless (looking-at-p "^[ \t]*$")
              (open-line 1))
            ;; 2. Loop over unique Qt symbols and insert includes if missing
            (dolist (sym qt-types)
              (let* ((header (qt-auto-include--header-for-type sym))
                     (include-line (format "#include <%s>" header)))
                (unless (qt-auto-include--have-include-p include-line)
                  (insert include-line "\n")))))
          (message "qt-auto-include: Inserted includes for: %s"
                   (mapconcat #'identity qt-types ", ")))))))

(defun qt-auto-include--collect-qt-types-from-flymake ()
  "Return a list of unique Qt type names from Flymake diagnostics.

A Qt type name is any symbol at point of a diagnostic whose message contains
\"Unknown type name\" and whose symbol name matches `Q[A-Za-z0-9_]*`."
  (let ((qt-types '()))
    (dolist (diag (flymake-diagnostics))
      (let* ((msg (flymake-diagnostic-text diag))
             (beg (flymake-diagnostic-beg diag)))
        (when (and msg
                   (string-match-p "Unknown type name" msg)
                   beg)
          (save-excursion
            (goto-char beg)
            (let ((sym (thing-at-point 'symbol t)))
              (when (and sym
                         (string-match-p "\\`Q[A-Za-z0-9_]*\\'" sym))
                (add-to-list 'qt-types sym)))))))
    qt-types))

(defun qt-auto-include--find-include-position ()
  "Return buffer position where new #include lines should be inserted.

This is:
- The first empty line following consecutive #include lines at
  the top of the file, or
- If no such empty line exists, the first line of the file."
  (save-excursion
    (goto-char (point-min))
    (let ((last-include-end nil)
          (continue t))
      (while (and continue (not (eobp)))
        (cond
         ;; Lines starting with optional whitespace then #include
         ((looking-at "^[ \t]*#include\\b")
          (setq last-include-end (line-end-position))
          (forward-line 1))
         ;; Stop when we hit a non-include, non-blank line
         ((looking-at "^[ \t]*$")
          (forward-line 1))
         (t
          (setq continue nil))))
      (if last-include-end
          (progn
            (goto-char last-include-end)
            (forward-line 1)
            (let ((found-empty nil))
              (while (and (not found-empty) (not (eobp)))
                (if (looking-at "^[ \t]*$")
                    (setq found-empty t)
                  (forward-line 1)))
              (if found-empty
                  (point)
                ;; No empty line after includes; insert after last include
                (1+ last-include-end))))
        ;; No includes at top: place at the first line of the file
        (point-min)))))

(defun qt-auto-include--have-include-p (include-line)
  "Return non-nil if INCLUDE-LINE already exists verbatim in the buffer.

INCLUDE-LINE should be a string like \"#include <QString>\"."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (concat "^[ \t]*"
                           (regexp-quote include-line)
                           "[ \t]*$"))
          (found nil))
      (while (and (not found) (re-search-forward pattern nil t))
        (setq found t))
      found)))

(defun qt-auto-include--header-for-type (sym)
  "Return header name for Qt type SYM (a string).

By default this just returns SYM itself, so type Foo becomes <Foo>.
Customize this if you need module prefixes like QtCore/QString."
  ;; Simple default mapping; adjust as needed.
  sym)

(provide 'qt-auto-include)
