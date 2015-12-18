;;; my-note.el --- My note utilities

;;; Commentary:
;;
;;   This package provides utilities to make markdown files
;;   and search the files with helm-ag.

(require 'helm-ag)
(defvar mynote:base-dir "~/my-note/")
(defvar mynote:file-format "%Y/%m/%d-%H%M%S.md")
(defvar mynote:find-file-function 'find-file-other-window)

(defun mynote:find-file ()
  (interactive)
  (let* ((file (format-time-string
                (concat mynote:base-dir mynote:file-format) (current-time)))
         (dir (file-name-directory file))
         (file-nondir (file-name-nondirectory file))
         (file-name (read-string "Enter file name: " file-nondir))
         (full-path (concat dir file-name))
         (keywords (read-string "Enter keywords separated by comma: "))
         (buffer))
    (make-directory dir t)
    (setq buffer (funcall mynote:find-file-function full-path))
    (with-current-buffer buffer
      (insert (concat "Keywords: " keywords "\n\n# ")))))

(defun mynote:search ()
  (interactive)
  (unless (file-exists-p mynote:base-dir) (error "Note files not found"))
  (helm-do-ag mynote:base-dir))

(provide 'my-note)
