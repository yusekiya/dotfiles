(eval-when-compile (require 'cl))
(defun mmemo-list-drive ()
  (interactive)
  (let ((result nil))
    (mapcar
     (function
      (lambda (x)
        (if (file-exists-p
             (format "%s:/" x))
            (setq result
                (cons
                 (format "%s:" x) result)))))
     '(A B C D E F G H I J K L M N
     O P Q R S T U V W X Y Z))
    (reverse result)))

(defun network-dired (comm drive-list &optional use)
    (require 'widget)
    (let ((drvL) (drive (mmemo-list-drive))
          (search-regexp
           (if use
               "[A-Z]+: *\\\\\\\\[^ ]+"
             "\\\\\\\\[^ ]+")))
      (with-temp-buffer
      (shell-command comm (current-buffer))
      (while (re-search-forward search-regexp nil t nil)
        (setq drvL (cons (split-string (match-string 0)) drvL))))
      (pop-to-buffer "*NET DIR LIST*")
      (erase-buffer)
      (widget-minor-mode 1)
      (when drive-list
        (mapcar
         (lambda (x)
           (lexical-let ((x x))
             (if (assoc x drvL)
                 ()
               (widget-create 'push-button
                              :notify (lambda (widget &rest ignore)
                                        (kill-buffer (current-buffer))
                                        (dired x))
                              (concat x))
               (widget-insert "\n"))))
         drive))
      (mapcar
       (lambda (x)
         (lexical-let ((x x))
           (widget-create 'push-button
                          :notify (lambda (widget &rest ignore)
                                    (kill-buffer (current-buffer))
                                    (dired (car x)))
                          (concat (car x) "  " (cadr x))))
         (widget-insert "\n"))
       drvL)
      (goto-char (point-min))))

(defun netdir()
  (interactive)
  (network-dired "net use" t t))

(defun netcomp ()
  (interactive)
  (network-dired "net view" nil))