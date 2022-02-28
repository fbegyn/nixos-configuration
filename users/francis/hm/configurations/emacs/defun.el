;; extra functions for emacs
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                      str)
    (setq str (replace-match "" t t str)))
  str)

(defun eshell/e (arg)
  "opens a given file in emacs from eshell"
  (find-file arg))

(defun eshell/eh (arg)
  "opens a file in emacs from shell horizontally"
  (split-window-vertically)
  (other-window 1)
  (find-file arg))

(defun eshell/ev (arg)
  "opens a file in emacs from shell vertically"
  (split-window-horizontally)
  (other-window 1)
  (find-file arg))

 ;; update all buffers after a git chance
 (defun revert-all-buffers ()
   "Iterate through the list of buffers and revert them, e.g. after a
    new branch has been checked out."
    (interactive)
    (when (yes-or-no-p "Are you sure - any changes in open buffers will be lost! ")
      (let ((frm1 (selected-frame)))
        (make-frame)
        (let ((frm2 (next-frame frm1)))
          (select-frame frm2)
          (make-frame-invisible)
          (dolist (x (buffer-list))
            (let ((test-buffer (buffer-name x)))
              (when (not (string-match "\*" test-buffer))
                (when (not (file-exists-p (buffer-file-name x)))
                  (select-frame frm1)
                  (when (yes-or-no-p (concat "File no longer exists (" (buffer-name x) "). Close buffer? "))
                    (kill-buffer (buffer-name x)))
                  (select-frame frm2))
                (when (file-exists-p (buffer-file-name x))
                  (switch-to-buffer (buffer-name x))
                  (revert-buffer t t t)))))
          (select-frame frm1)
          (delete-frame frm2)))))

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))
