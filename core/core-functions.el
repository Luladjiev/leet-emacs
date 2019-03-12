;;; core-functions.el -*- lexical-binding: t; -*-

(defun leet/ensure-folder (folder)
  "Ensure that a FOLDER exists."
  (let ((dir (expand-file-name folder user-emacs-directory)))
    (unless (file-directory-p dir)
      (message (concat "Creating " dir " folder"))
      (make-directory dir))))

(defun leet/project-root ()
  "Get the path to the root of your project."
  (ignore-errors (projectile-project-root)))

(defun leet/new-buffer ()
  "Create new buffer and set it as current."
  (interactive)
  (let ((buffer (generate-new-buffer "*untitled*")))
    (when buffer
      (display-buffer buffer '(display-buffer-same-window)))))

(defun leet/kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (if (y-or-n-p "Kill other buffers? ")
      (dolist (buffer (buffer-list))
	(unless (eq buffer (current-buffer))
	  (kill-buffer buffer)))))

(defun leet/find-init-file ()
  "Find leet init.el file."
  (interactive)
  (find-file-existing (expand-file-name "init.el" user-emacs-directory)))

(provide 'core-functions)
