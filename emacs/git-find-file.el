;;; git-find-file.el --- Find files in a git project quickly.

;; Author: Justin Zhu
;;
;; Copied by Dov from: https://github.com/hjz/emacs/blob/master/jz/git-find-file.el
;;
;; Based on URL: http://www.emacswiki.org/cgi-bin/wiki/FindFileInProject
;;               http://github.com/re5et/find-file-in-git-repo

(setq git-find-file-files-cache (make-hash-table :test 'equal))

(defun build-files-cache (repo)
  (let ((old-dir default-directory)
        (file-alist nil))
  (cd repo)
  (setq ret
     (mapcar (lambda (file)
       (let ((file-cons (cons (file-name-nondirectory file) file)))
         (add-to-list #'file-alist file-cons)
         file-cons))
             (split-string (shell-command-to-string "git ls-files") "\n")))
  (puthash repo ret git-find-file-files-cache)
  (cd old-dir)
  ret))

(defun ffip-project-files (repo)
  "Return an alist of all filenames in the project and their path."
  (setq ret (gethash repo git-find-file-files-cache))
  (if (not ret)
      (setq ret (build-files-cache repo)))
  ret)

(defun find-git-repo (dir)
  "Find base git directory"
  (if (or (string= (expand-file-name "../" dir) dir)
          (string= dir "/")
      )
      (error "Error! Not in a git repo.")
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (find-git-repo (expand-file-name "../" dir)))))

(defun ffip-completing-read (prompt names)
  "Perform a completing read over NAMES prompted by PROMPT.
ido is used for the completing read if available."
  (if (and (boundp 'ido-mode) ido-mode)
      (ido-completing-read prompt names nil t)
    (helm-comp-read prompt names)))

(defun last-component (str sep)
  (car (last (split-string str sep t))))

(defun git-find-file-in-repo (root file-name)
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((project-files (ffip-project-files root))
         (files (delete-dups (mapcar 'car project-files))))
    (if (eq nil file-name)
        (find-file root)
      (let*
        ((file-paths (delq 'nil (mapcar (lambda (file-cons)
                                          (when (string= file-name (car file-cons))
                                            (cdr file-cons))) project-files)))
         (file-path (if (cdr file-paths)
                        (ffip-completing-read "Disambiguate: " file-paths)
                      (car file-paths))))
        (find-file (concat root file-path))))))

(defun git-find-file ()
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((root (find-git-repo default-directory))
         (project-files (ffip-project-files root))
         (files (delete-dups (mapcar 'car project-files)))
         (file-name (ffip-completing-read (concat (last-component root "/") ": ") files))
         (file-paths (delq 'nil (mapcar (lambda (file-cons)
                                           (when (string= file-name (car file-cons))
                                             (cdr file-cons))) project-files)))
         (file-path (if (cdr file-paths)
                        (ffip-completing-read "Disambiguate: " file-paths)
                      (car file-paths))))
    (find-file (concat root file-path))))

(defun get-file-path-in-git-repo (file-name)
  "Return the full path of FILENAME in the current Git repository if it exists, otherwise return nil."
  (let* ((root (find-git-repo default-directory))
         (project-files (ffip-project-files root))
;         (files (delete-dups (mapcar 'car project-files)))
         (file-paths (delq 'nil (mapcar (lambda (file-cons)
                                           (when (string= file-name (car file-cons))
                                             (cdr file-cons))) project-files)))
         (file-path (if (cdr file-paths) nil (car file-paths))))
    (if file-path (concat root "/" file-path) nil)))

(defun git-find-file-rehash ()
  (interactive)
  (let* ((repo (find-git-repo default-directory)))
    (remhash repo git-find-file-files-cache)
    (build-files-cache repo)))
  

;;;###autoload
(defalias 'gffip 'git-find-file)

(provide 'git-find-file)
;;; git-find-file.el ends here
