;; Find the root dir for git or for svn
(defun svn-root-dir ()
  "Return the svn root dir"
  (interactive)
  (let ((dir (file-name-directory buffer-file-name))
        (olddir nil))
    (while (file-exists-p (expand-file-name ".svn/entries" dir))
      (setq olddir dir)
      (setq dir (expand-file-name ".." dir)))
    (message olddir)))

(defun git-root-dir ()
  "Return the git root dir"
  (interactive)
  (let ((dir (file-name-directory buffer-file-name)))
    (while
        (and
         (not (string= dir "/"))
         (not (file-exists-p (expand-file-name ".git/config" dir))))
      (setq dir (expand-file-name ".." dir)))
    (if (string= dir "/")
        nil
      (message dir))))
  
(defun vc-root-dir ()
  "Find the root dir of the current vc used"
  (interactive)
  (let ((dir (svn-root-dir)))
    (if dir
        (message dir)
      (message (git-root-dir)))))

(defun vc-ack (command-args)
  """Run ack in the rot directory of the current directory"""
  (interactive 
   (list (read-from-minibuffer "Run ack (like this): "
                               (ack-build-command)
                               nil
                               nil
                               'ack-history)))
  (let ((dir (vc-root-dir)))
    (if dir
        (progn
          (dired dir)
          (compilation-start command-args 'ack-mode)))))
