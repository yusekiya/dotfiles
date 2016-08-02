;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager
;; Must be loaded before the other packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; location of packages
;; installed packages are located at default directory
(setq package-user-dir "~/.emacs.d/elpa")
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting to synchronize packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package f
  :ensure t)
(use-package s
  :ensure t)
(use-package dash
  :ensure t)

(defvar my:package_list_file (f-join user-emacs-directory ".package_list")
  "Path to file including emacs package list (one package per line)")
(defvar my:package_list_change_log_dir (f-join user-emacs-directory "change_log/")
  "Path to directory including change log of package list")
(f-mkdir my:package_list_change_log_dir)

(defun my:get_local_package_list()
  "return list including installed packages"
  (package-initialize)
  (reverse (-flatten (--map (-take 1 it) package-alist))))

(defun my:save_package_diff_log (add_list remove_list)
  "Output diff of package list
input
    add_list: symbol list added to `my:package_list_file'
    remove_list: symbol list removed from `my:package_list_file'"
  (let ((file_name)
        (text)
        (text_add_list)
        (text_remove_list))
    ;; Make file name
    (setq file_name (f-join my:package_list_change_log_dir
                            (concat "package_diff-" (format-time-string "%Y%m%d-%H%M%S") ".log")))
    ;; Make content such as
    ;; + added package 1
    ;; + added package 2
    ;; + added package 3
    ;; - removed package 1
    (setq text_add_list (--map (concat "+ " (symbol-name it)) add_list))
    (setq text_remove_list (--map (concat "- " (symbol-name it)) remove_list))
    (setq text (s-join "\n" (append text_add_list text_remove_list)))
    ;; Output content to file
    (f-write-text text 'utf-8 file_name)
    ))

(defun my:save_package_log (package_list)
  "Output package list log"
  (let ((file_name))
    (setq file_name (f-join my:package_list_change_log_dir
                            (concat "package_list-" (format-time-string "%Y%m%d-%H%M%S") ".log")))
    (f-write-text (mapconcat 'symbol-name package_list "\n") 'utf-8 file_name))
  )

(defun my:save_package_list()
  "Output installed package list to `my:package_list_file'"
  (interactive)
  (let ((local_package_list)
        (emacs_package_list)
        (add_list)
        (remove_list)
        (buffer-name "*Package Sync*"))
    ;; Get packages as symbol list
    (setq local_package_list (my:get_local_package_list))
    (setq emacs_package_list (my:load_package_list))
    (setq add_list (-difference local_package_list emacs_package_list))
    (setq remove_list (-difference emacs_package_list local_package_list))
    (when (and (or add_list remove_list)
               (yes-or-no-p (if (and (<= (length add_list) 5) (<= (length remove_list) 5))
                                (format "Add: %s\nRemove: %s\nUpdate emacs package list? "
                                        (mapconcat 'symbol-name add_list ", ")
                                        (mapconcat 'symbol-name remove_list ", "))
                              (my:show_message_in_new_buffer buffer-name
                                                             (format "Add:\n%s\n\nRemove:\n%s"
                                                                     (mapconcat 'identity
                                                                                (-map (lambda (list) (mapconcat 'symbol-name list ", "))
                                                                                      (-partition-all 3 add_list)) ",\n")
                                                                     (mapconcat 'identity
                                                                                (-map (lambda (list) (mapconcat 'symbol-name list ", "))
                                                                                      (-partition-all 3 remove_list)) ",\n")))
                              "Update emacs package list? ")))
      ;; Output local_package_list to file
      (f-write-text (mapconcat 'symbol-name local_package_list "\n") 'utf-8 my:package_list_file)
      ;; Output change log
      (my:save_package_log local_package_list)
      (message (concat "Emacs package list file updated! Check change log in " my:package_list_change_log_dir)))))

(defun my:load_package_list()
  "Import package list from `my:package_list_file', and return the list"
  (if (f-exists? my:package_list_file)
      (let ((package_list))
        ;; Import package list from text file
        (setq package_list (f-read-text my:package_list_file))
        ;; Split string at break
        (setq package_list (s-lines package_list))
        ;; Trimming extra whitespace
        (setq package_list (-map 's-trim package_list))
        ;; Remove blank string
        (setq package_list (--remove (string= it "") package_list))
        ;; Covert string to symbol
        (-map 'intern package_list)
        )))

(defun my:package_delete (pkg)
  "Delete package. Input PKG is given as a symbol (not a string)"
  (let ((pkg_desc (car (assoc-default pkg package-alist))))
    (package-delete pkg_desc)
    ))

(defun my:show_message_in_new_buffer (buffer_name message)
  "Put message in buffername"
  (let ((buf))
    (setq buf (get-buffer-create buffer_name))
    (with-current-buffer buf
      (erase-buffer)
      (insert message))
    (display-buffer buf)
    ))

(defun my:synchronize_packages ()
  "Synchronize packages with the packages listed in `my:package_list_file'"
  (let* ((local_package (my:get_local_package_list))
        (emacs_package (my:load_package_list))
        (to_be_installed (-difference emacs_package local_package))
        (to_be_deleted (-difference local_package emacs_package))
        (flag_maybe_installed nil)
        (flag_maybe_deleted nil)
        (flag_update nil)
        (buffer-name "*Package Sync*"))
    (when to_be_installed
      (when (yes-or-no-p
             (cond ((= (length to_be_installed) 1)
                    (format "Package Sync: Install package `%s'? " (symbol-name (car to_be_installed))))
                   ((<= (length to_be_installed) 5)
                    (format "Package Sync: Install these %d packages: %s? "
                            (length to_be_installed)
                            (mapconcat 'symbol-name to_be_installed ", ")))
                   (t
                    (my:show_message_in_new_buffer buffer-name
                                                   (concat "New packages to be installed\n\n"
                                                           (mapconcat 'identity
                                                                      (-map (lambda (list) (mapconcat 'symbol-name list ", "))
                                                                            (-partition-all 3 to_be_installed)) ",\n")))
                    (format "Package Sync: Install %d packages listed in %s? "
                            (length to_be_installed) buffer-name))
                   )
             )
        (package-refresh-contents)
        (mapc 'package-install to_be_installed)
        (setq flag_update t)
        (message (format "Package Sync: Installed: %s" (mapconcat 'symbol-name to_be_installed ", ")))
        )
      (setq flag_maybe_installed t)
      )
    (when to_be_deleted
      (when (yes-or-no-p
             (cond ((= (length to_be_deleted) 1)
                    (format "Package Sync: Delete package `%s'? " (symbol-name (car to_be_deleted))))
                   ((<= (length to_be_deleted) 5)
                    (format "Package Sync: Delete these %d packages: %s? "
                            (length to_be_deleted)
                            (mapconcat 'symbol-name to_be_deleted ", ")))
                   (t
                    (my:show_message_in_new_buffer buffer-name
                                                   (concat "Packages to be deleted\n\n"
                                                           (mapconcat 'identity
                                                                      (-map (lambda (list) (mapconcat 'symbol-name list ", "))
                                                                            (-partition-all 3 to_be_deleted)) ",\n")))
                    (format "Package Sync: Delete %d packages listed in %s? "
                            (length to_be_deleted) buffer-name))
                   )
             )
        (mapc 'my:package_delete to_be_deleted)
        (setq flag_update t)
        (message (format "Package Sync: Deleted: %s" (mapconcat 'symbol-name to_be_deleted ", ")))
        )
      (setq flag_maybe_deleted t)
      )
    (if flag_update (package-initialize))
    (if (or flag_maybe_installed flag_maybe_deleted) (my:save_package_list))
    )
  )

;; Synchronize packages
(if (f-exists? my:package_list_file) (my:synchronize_packages) (my:save_package_list))

;; Update emacs package file,
;; when packages are installed or deleted through through package-menu-execute
(advice-add 'package-menu-execute :after 'my:save_package_list)
;; and when installed through package-install-from-buffer
;; (c.f. package-install-file calls package-install-from-buffer)
(advice-add 'package-install-from-buffer :after 'my:save_package_list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)

;; Function to get ime mode as string
(defun my:mac-get-ime()
  (if (string-match "\\.roman$" (mac-input-source))
      nil
    (mac-input-source)))

(defun my:get-ime ()
  (if (equal system-type 'darwin)
      (my:mac-get-ime)
    current-input-method))

;; Function to go into a next blank brackets
(defun my:goto-blank-brackets-forward ()
  (interactive)
  (let ((init-pos (point))
        (point-eol)
        (iter 0)
        (max-iter 10)
        (is-successful nil))
    (block my-exit
      (while (< iter max-iter)
        (setq point-eol (save-excursion (end-of-line) (point)))
        (if (re-search-forward "\\(()\\|{}\\|\\[\\]\\)" point-eol t)
            (progn
              (setq is-successful t)
              (backward-char 1)
              (return-from my-exit))
          (progn
            (setq iter (1+ iter))
            (forward-line)
            (when (eobp) (return-from my-exit))))))
    (unless is-successful (message "Can't find any blank brackets around here") (goto-char init-pos))))

;; Function to go into a previous blank brackets
(defun my:goto-blank-brackets-backward ()
  (interactive)
  (let ((init-pos (point))
        (point-bol)
        (iter 0)
        (max-iter 10)
        (is-successful nil))
    (block my-exit
      (while (< iter max-iter)
        (setq point-bol (save-excursion (beginning-of-line) (point)))
        (if (re-search-backward "\\(()\\|{}\\|\\[\\]\\)" point-bol t)
            (progn
              (setq is-successful t)
              (forward-char 1)
              (return-from my-exit))
          (progn
            (setq iter (1+ iter))
            (forward-line -1)
            (when (bobp) (return-from my-exit))
            (end-of-line)))))
    (unless is-successful (message "Can't find any blank brackets around here") (goto-char init-pos))))

(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun change-transparency (num)
  "This command changes the transparency of background.
Input a value within 50(transparent) to 100(opaque) as alpha value.
If you only input RET without inputting a number, the default is applied."
  (interactive "sInput alpha value within 20 to 100 (default 90): ")
  (cond ((string= num "")
         (set-frame-parameter nil 'alpha '(90 90)))
        ((< (string-to-number num) 50)
         (error "Too small alpha value"))
        (t (set-frame-parameter nil 'alpha (string-to-number num)))
    ))

;; window size
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-event (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(defun match-paren (arg)
  "jump to corresponding parenthesis"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((looking-back "\\s\)") (backward-list 1))
        (t ())))

(defun kill-word-at-point ()
      (interactive)
      (let* ((bounds (bounds-of-thing-at-point 'word))
             (start (car bounds))
             (end (cdr bounds)))
            (kill-region start end))
      (message "word killed"))

(defun kill-region-or-word ()
  "Kill the word at the point if transient-mark-mode is not nil and mark-active is nil.
When region is set, call `kill-region'."
  (interactive)
  (if (and transient-mark-mode (not mark-active))
    (kill-word-at-point)
    (kill-region (region-beginning) (region-end))))

(defun save-word-at-point ()
      (interactive)
      (let* ((bounds (bounds-of-thing-at-point 'word))
             (start (car bounds))
             (end (cdr bounds)))
            (kill-ring-save start end))
      (message "word saved"))

(defun save-region-or-word ()
  "save the word at the point if transient-mark-mode is not nil and mark-active is nil.
When region is set, call `kill-ring-save'."
  (interactive)
  (if (and transient-mark-mode (not mark-active))
    (save-word-at-point)
    (kill-ring-save (region-beginning) (region-end))))

(defun copy-line (&optional arg)
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-beginning-position (1+(or arg 1))))
  (message "line saved"))

(defun shorten-directory (dir max-length)
  "Show up to MAX-LENGTH characters of a directory name DIR."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Store current directory in clipboard
(defun my:copy-current-path()
  (interactive)
  (let ((fPath default-directory))
    (when fPath
      (message "stored path: %s" fPath)
      (kill-new (file-truename fPath)))))

(defun my:copy-current-filename()
  (interactive)
  (let ((fPath buffer-file-name))
    (when fPath
      (message "stored path: %s" fPath)
      (kill-new (file-truename fPath)))))

(defalias 'thisdir 'my:copy-current-path)
(defalias 'thisfile 'my:copy-current-filename)

(defun my:swap-faces (face1 face2)
  (let (temp-var)
    (cl-loop for (attr . desc) in face-attribute-name-alist do
          (setq temp-var (face-attribute face1 attr))
          (set-face-attribute face1 nil attr (face-attribute face2 attr))
          (set-face-attribute face2 nil attr temp-var))))

(defun revert-buffer-without-confirm ()
  (interactive)
  (if (not (buffer-modified-p))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mozc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for linux
(when (eq system-type 'gnu/linux)
  (use-package mozc
    :config
    (setq default-input-method "japanese-mozc")
    (global-set-key [(super space)] 'toggle-input-method)
    (define-key isearch-mode-map "\C-o" 'isearch-toggle-input-method))
  (use-package mozc-popup
    :config
    (setq mozc-candidate-style 'popup))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fontawesome
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fontawesome
  :defer t
  :config
  (if (equal system-type 'windows-nt) (set-fontset-font "fontset-default" '(#xf000 . #xf280) "FontAwesome-11"))
  )

(defun is-fontawesome-installed ()
  (member "FontAwesome" (font-family-list)))

(defun is-fontawesome-ready ()
  (and (is-fontawesome-installed)
       (package-installed-p 'fontawesome))
  )

(defun my:safe-awesomefont-icon (str &optional icon)
  "Return fontawesome icon if possible, otherwise return string str.
If the second argument icon is omitted or nil, this function just returns str.
The argument icon must be string."
  (if (and icon (is-fontawesome-ready))
      (fontawesome icon)
    str))

(defun my:safe-lighter-icon (str &optional icon)
  (concat " " (my:safe-awesomefont-icon str icon))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "yasnippet_config") ; should be loaded before auto-complete-mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load "acmode")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "company_config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'company
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-to-list (make-local-variable 'company-backends) 'company-elisp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-insert-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(auto-insert-mode)
(custom-set-variables
 '(auto-insert-directory (concat user-emacs-directory "lib/template/")))
;; Get default include path
;; shell-command for clang: "echo \"\" | clang++ -E -x c++ - -v"
;; shell-command for g++: "echo \"\" | g++ -v -x c++ -E -"
(defun my:get-include-dirs ()
  (let* ((command-result (shell-command-to-string "echo \"\" | clang++ -E -x c++ - -v"))
         (start-string "#include <...> search starts here:\n")
         (end-string "End of search list.\n")
         (start-pos (string-match start-string command-result))
         (end-pos (string-match end-string command-result))
         (include-string (substring command-result (+ start-pos (length start-string)) end-pos)))
    (split-string include-string)))
(defun my:include-dirs-with-I ()
  (mapcar (lambda (item) (concat "-I" item)) (my:get-include-dirs)))
;; Replace macro in template files
(defvar my:autoinsert-template-replace-alist
  '(("%file%" .
     (lambda()
       (file-name-nondirectory (buffer-file-name))))
    ;("%author%" . (lambda()(identity user-full-name)))
    ;("%email%"  . (lambda()(identity user-mail-address)))
    ("%filewithoutext%" .
     (lambda()
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ("%includePathsWithI%" . (lambda() (mapconcat #'identity (my:include-dirs-with-I) "\n")))
    ))
(defun my:template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        my:autoinsert-template-replace-alist)
  (goto-char (point-max))
  (message "done."))
;; alist for extensions and templates
(setq auto-insert-alist
      (nconc '(
               ;("\\.cpp$" . ["template.cpp" my:template])
               ;("\\.h$"   . ["template.h" my:template])
               ("\\.clang_complete" . ["template.clang_complete" my:template])
               ) auto-insert-alist))

(add-hook 'find-file-hooks 'auto-insert)

(defun my:insert-include-dirs-option ()
  (interactive)
  (insert (mapconcat #'identity (my:include-dirs-with-I) "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vc-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn off vc-mode
(custom-set-variables
 '(vc-handled-backends nil))
;; Remove hook
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Do isearch with selected region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scratch buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; Make "*scratch*" and put it into buffer-list
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my:clear-scratch()
  "kill-buffer for *scratch* just erase buffer contents"
  (if (string= "*scratch*" (buffer-name)) (progn (my-make-scratch 0) nil) t))

(add-hook 'kill-buffer-query-functions 'my:clear-scratch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line and row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show line number in mode line
(setq line-number-mode t)       ; show line number in mode line
(use-package linum
  :defer t
  ;; :init (global-linum-mode)
  :config
  (progn
    ;;(setq linum-format "%3d ")
    (setq linum-format 'dynamic)
    ;; (setq linum-delay t)
    ;; (defadvice linum-schedule (around my-linum-schedule () activate)
    ;;   (run-with-idle-timer 0.2 nil #'linum-update-current))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package server
  :config
  (progn (setq server-auth-dir "~/.emacs.d/server")
         (server-start)))
;; (unless (server-running-p)
;;   ;; (server-force-delete)
;;   (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show the windows in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Split window verticaly (the function name confuses me a lot!)
(setq ediff-split-window-function 'split-window-horizontally)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  ;; :defer t
  :commands dired
  :init
  (progn (put 'dired-find-alternate-file 'disabled nil)
         ;; Directory first
         (setq ls-lisp-dirs-first t)
         )
  :config
  (progn (setq dired-dwim-target t)
         (if (equal system-type 'windows-nt)
             (setq dired-listing-switches "-alh")
           (setq dired-listing-switches "-alh --group-directories-first"))
         (bind-keys :map dired-mode-map
                    ((kbd "RET") . dired-find-alternate-file)
                    ("a" . dired-find-file)
                    ("j" . dired-next-line)
                    ("k" . dired-previous-line)
                    ("h" . dired-up-directory)
                    ("l" . ignore)
                    ("i" . ignore)
                    ("s" . dired-rotate-sort)
                    ("o" . dired-display-file))
         (use-package wdired
           ;; :defer t
           ;; :commands wdired-change-to-dired-mode
           :init (bind-keys :map dired-mode-map ("i" . wdired-change-to-wdired-mode)))
         (use-package popwin
           :defer t
           :config
           ;; dired: open file with popwin
           (defun my:find-file-popwin-dired ()
             (interactive)
             (popwin:find-file (dired-get-file-for-visit)))
           (define-key dired-mode-map "l" 'my:find-file-popwin-dired)
           )
         (use-package dired-ranger
           :config
           (bind-keys :map dired-mode-map
                      :prefix "c"
                      :prefix-map dired-ranger-map
                      :prefix-docstring "Map for ranger operations."
                      ("c" . dired-ranger-copy)
                      ("p" . dired-ranger-paste)
                      ("m" . dired-ranger-move)))
         (use-package dired-filter
           :config
           (dired-filter-mode))
         ;; Settings for windows
         (if (equal system-type 'windows-nt)
             (progn
               ;; Key bindings for windows
               (if (equal system-type 'windows-nt)
                   (bind-keys :map dired-mode-map
                              ("z" . uenox-dired-winstart)))
               (autoload 'netdir "mmemo-drive" "My computer" t)
               (autoload 'netcomp "mmemo-drive" "My network" t)
               (defadvice dired-up-directory
                   (around list-drive activate)
                 (if (string-match "^[a-zA-Z]:[/]*$" default-directory)
                     (netdir)
                   ad-do-it))))
         ))

;; Don't make new buffer when visit new directory
(defvar my-dired-before-buffer nil)
(defadvice dired-up-directory
    (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
    (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

;; Run associated program (for windows)
(defun uenox-dired-winstart ()
  "Type '[uenox-dired-winstart]': win-start the current line's file."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename)))
        (w32-shell-execute "open" fname)
        (message "win-started %s" fname))))

;; Expand sort function of dired
(defvar dired-sort-order '("" "t" "S" "X")
  "Toggle -t (time) -X (extension) -S (size) none (alphabetical)")
(defvar dired-sort-order-position 0)

(defun dired-rotate-sort ()
  "Rotate dired toggle sorting order by `dired-sort-order'"
  (interactive)
  (setq dired-sort-order-position
        (% (1+ dired-sort-order-position) (length dired-sort-order)))
  (setq dired-actual-switches
        (concat dired-listing-switches (elt dired-sort-order
                                            dired-sort-order-position)))
  (dired-sort-other dired-actual-switches))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color"
;;   "Set `ansi-color-for-comint-mode' to t." t)
;; (add-hook 'shell-mode-hook
;;           '(lambda ()
;;              (setq ansi-color-names-vector
;;                    ["#000000"           ; black
;;                     "#ff6565"           ; red
;;                     "#93d44f"           ; green
;;                     "#eab93d"           ; yellow
;;                     "#204a87"           ; blue
;;                     "#ce5c00"           ; magenta
;;                     "#89b6e2"           ; cyan
;;                     "#ffffff"]          ; white
;;                    )
;;              (ansi-color-for-comint-mode-on)))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which function mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-func
  :config
  (setq which-func-modes '(python-mode))
  (which-function-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:python-mode-setup()
  (setq tab-width 4
        python-indent-offset 4
        evil-shift-width 4)
  (use-package highlight-indent-guides
    :config
    (highlight-indent-guides-mode 1))
  )

(add-hook 'python-mode-hook 'my:python-mode-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:c-c++-mode-setup()
  (setq evil-shift-width 2
        tab-width 2
        )
  )
(add-hook 'c-mode-hook 'my:c-c++-mode-setup)
(add-hook 'c++-mode-hook 'my:c-c++-mode-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doc-view mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doc-view
  :defer t
  :config
  (progn (bind-keys :map doc-view-mode-map
                    ("j" . doc-view-next-line-or-next-page)
                    ("k" . doc-view-previous-line-or-previous-page)
                    ("h" . image-backward-hscroll)
                    ("l" . image-forward-hscroll))))
(add-hook 'doc-view-mode-hook 'auto-revert-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-theme-directory (concat user-emacs-directory "lib/themes/"))
(defcustom my-default-theme 'solarized-dark "default theme")
;; set safe themes
(custom-set-variables
 '(custom-safe-themes
   '(
     "3c9d994e18db86ae397d077b6324bfdc445ecc7dc81bb9d528cd9bba08c1dac1"
     "367e859d84bdd85bf9ab7edfee035a7cce5d3db0f75ffaf85e7753da84e4920c"
     "729b17c0c54263c489d13ac0ce89ef17fcedf9d36f920c635c22a4b33a6ca59d"
     "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33"
     default)
  )
)

(defun change-theme(theme &optional no-confirm no-enable)
  "Disable all enabled themes, and load Custom theme named THEME."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                 (mapcar 'symbol-name
                     (custom-available-themes))))
    nil nil))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme theme)
)

(use-package solarized-theme
  :defer t
  :init
  (setq solarized-high-contrast-mode-line nil)
  (custom-set-variables
   ;; Don't scale font
   '(solarized-use-variable-pitch nil)
   '(solarized-height-minus-1 1.0)
   '(solarized-height-plus-1 1.0)
   '(solarized-height-plus-2 1.0)
   '(solarized-height-plus-3 1.0)
   '(solarized-height-plus-4 1.0)
   )
  (setq x-underline-at-descent-line t)
  (defun my:swap-faces-solarized ()
    (when (and (featurep 'powerline) (eq (car custom-enabled-themes) 'solarized-dark))
      (my:swap-faces 'powerline-active1 'powerline-inactive1)
      (my:swap-faces 'powerline-active2 'powerline-inactive2)
      (powerline-reset)))
  (defadvice load-theme (after my:ad-swap-faces-solarized activate)
    (my:swap-faces-solarized))
  )

(defun my:load-default-theme ()
  (load-theme my-default-theme t t)
  (enable-theme my-default-theme)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired-async
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'dired (use-package dired-async))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-save-buffers-enhanced
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-save-buffers-enhanced
  :config
  (setq auto-save-buffers-enhanced-include-regexps '(".+")) ; save all files automatically
  ;; Black list
  (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$" "\\.ipynb"
                                                     "^/ssh:" "/sudo:" "^/scp:" "^/rsync:"
                                                     "^/sshx:" "^/scpx:"))
  ;; Inhibit message
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  ;; *scratch* buffer
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer (concat user-emacs-directory "scratch"))
  (auto-save-buffers-enhanced t)
  (bind-key "C-x a s" 'auto-save-buffers-enhanced-toggle-activity)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pos-tip
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package pos-tip
  ;; :defer t
  :config
  ;; Default color
  (progn (setq pos-tip-foreground-color "white")
         (setq pos-tip-background-color "steelblue")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smartrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'smartrep)
;; (custom-set-variables '(smartrep-mode-line-active-bg nil))
(use-package smartrep
  ;; :defer t
  :config
  (custom-set-variables '(smartrep-mode-line-active-bg nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key-chord
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package key-chord
  :config
  (key-chord-define-global "jk" 'view-mode) ;toggle view-mode
  (setq key-chord-two-keys-delay 0.06)
  (key-chord-mode 1)
  ;; Prevent input-method-function from changing its value to nil without asking
  (defadvice toggle-input-method (around toggle-input-method-around activate)
    (let ((input-method-function-save input-method-function))
      ad-do-it
      (setq input-method-function input-method-function-save))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :config
  (custom-set-variables '(which-key-lighter ""))
  (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use aspell
(when (equal system-type 'windows-nt)
  (setq-default ispell-program-name "C:\\opt\\Aspell\\bin\\aspell"))
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
;; 日本語対応
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; User dictionary
(setq ispell-personal-dictionary "~/en.pws")
(bind-key "A-$" 'ispell-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (progn (setq flyspell-use-meta-tab nil))
  :config
  (diminish 'flyspell-mode (my:safe-lighter-icon "Fly" "check"))
  (bind-keys :map flyspell-mode-map
                    ("C-;" . nil)
                    ("C-." . nil)
                    ("C-," . nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popwin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popwin
  :config
  (popwin-mode 1)
  ;; Automatically determine window size and position of popwin
  (defun popwin-auto-set-popup-window-position-and-size ()
    (let ((w (frame-width))
          (h (frame-height))
          (flag 0))
      (if (and (< 140 w) (< h w)) (setq flag 1))
      (cond ((= flag 1) (custom-set-variables '(popwin:popup-window-position 'right)
                                              '(popwin:popup-window-width 70)))
            ((= flag 0) (custom-set-variables '(popwin:popup-window-position 'bottom)))
            (t (print "Error")))))
  (defadvice  popwin:display-buffer-1 (before ad-popwin-auto-window-position-and-sizse activate)
    (popwin-auto-set-popup-window-position-and-size))
  (defadvice  popwin:popup-buffer (before ad-popwin-auto-window-position-and-sizse activate)
    (popwin-auto-set-popup-window-position-and-size)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; direx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package direx
  :bind ("C-x C-d" . direx:jump-to-directory-other-window)
  :config
  (if (is-fontawesome-ready)
      (use-package fontawesome
        :config
        (defface my:direx-folder
          '((t (:family "FontAwesome"
                        :height 0.8
                        :inherit dired-directory)))
          "direx face for folder icon")
        (defface my:direx-file
          '((t (:family "FontAwesome"
                        :height 0.8
                        :inherit default)))
          "direx face for file icon")
        (setq direx:leaf-icon (concat (propertize (fontawesome "file-o")
                                                  'face 'my:direx-file)
                                      " ")
              direx:open-icon (concat (propertize (fontawesome "folder-open")
                                                  'face 'my:direx-folder)
                                      " ")
              direx:closed-icon (concat (propertize (fontawesome "folder")
                                                    'face 'my:direx-folder)
                                        " ")
              )
        )
    )
  (push '(direx:direx-mode :position left :width 50 :dedicated t)
        popwin:special-display-config)
  )

(defun my:direx-mode-hook()
  (visual-line-mode -1)
  (toggle-truncate-lines 1))

(add-hook 'direx:direx-mode-hook 'my:direx-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "elscreen-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; view-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package view
  :defer t
  :init
  (setq view-read-only t)
  :config
  (diminish 'view-mode "")
  )

(use-package viewer
  :commands view-mode-by-default-setup
  :config
  ;; Regular expression of file extension enable view mode
  (setq view-mode-by-default-regexp "\\.\\(dat\\|log\\)")
  (add-hook 'find-file-hook 'view-mode-by-default-setup)
  ;; ;; Don't disable view-mode
  ;; (viewer-stay-in-setup)
  )

;; If evil package is installed, view-mode starts in normal state
(defun my:view-mode-setup ()
  (if (featurep 'evil) (evil-normal-state)))

(add-hook 'view-mode-hook 'my:view-mode-setup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (load "yatex-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUCTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "my-auctex-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "my-helm-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-jump-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-jump-mode
  :defer t
  :init
  (use-package key-chord
    :config
    (key-chord-define-global "jw" 'ace-jump-word-mode)
    (key-chord-define-global "jc" 'ace-jump-char-mode)
    (key-chord-define-global "jl" 'ace-jump-line-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-search-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ace-isearch
  :defer t
  :init
  (custom-set-variables
   ;; Do not use helm-swoop automatically.
   '(ace-isearch-use-function-from-isearch nil)
   '(ace-isearch-lighter ""))
  (defadvice isearch-forward (before my:advice-turn-on-global-ace-isearch-mode activate)
    (unless (featurep 'ace-isearch) (global-ace-isearch-mode 1))
    )
  (defadvice isearch-backward (before my:advice-turn-on-global-ace-isearch-mode activate)
    (unless (featurep 'ace-isearch) (global-ace-isearch-mode 1))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "\C-c o l") 'org-store-link)
(define-key global-map (kbd "\C-c o a") 'org-agenda)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars t)
(setq org-directory "~/org/")
;; File name of org-default-notes-file
(setq org-default-notes-file (concat org-directory "schedule.org"))
;; (setq org-startup-truncated nil)
;; (setq org-log-done t)

;; Enable orgtbl-mode by default
(use-package org-table
  :commands orgtbl-mode
  :config
  (diminish 'orgtbl-mode (my:safe-lighter-icon "OrgTbl" "table")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; region bindings mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package region-bindings-mode
  :config
  (region-bindings-mode-enable))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "multiple-cursors-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expand region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :bind (("C-M-," . er/contract-region)
         ("C-M-." . er/expand-region)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-symbol
  :commands highlight-symbol-at-point
  :init
  (custom-set-variables '(highlight-symbol-foreground-color "white") ; highlight-symbol-foreground-color
                        '(highlight-symbol-colors '("DarkOrange2" "DodgerBlue2" "DeepPink2")))
  (use-package key-chord
    :config
    (key-chord-define-global "kl" 'highlight-symbol-at-point)
    )
  :bind (("C-c h" . highlight-symbol-at-point)
         ("C-c C-h" . highlight-symbol-remove-all))
  :config
  (diminish 'highlight-symbol-mode "")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight linum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'linum
  (use-package hlinum
    :config (hlinum-activate)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volatile highlight
;; Highlight pasted region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package volatile-highlights
  :config
  (diminish 'volatile-highlights-mode  "")
  (volatile-highlights-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar myCursorColor_ime "Springgreen4")
(defvar myCursorColor_SR "Yellow4")
(defvar myCursorColor_view "#0088cc")
(defvar myCursorColor_mc "VioletRed")

(defun my:refresh-cursor ()
  (interactive)
  (when (get-buffer-window (current-buffer))
    (cond
     ((and (featurep 'smartrep) (< 0 (length smartrep-mode-line-string)))
      (set-cursor-color myCursorColor_SR) (setq cursor-type 'box))
     ((and (featurep 'multiple-cursors) multiple-cursors-mode (if (featurep 'evil) (or (evil-insert-state-p) (evil-emacs-state-p)) t))
      (set-cursor-color myCursorColor_mc) (setq cursor-type 'box))
     ((and (my:get-ime) (if (featurep 'evil) (or (evil-insert-state-p) (evil-emacs-state-p)) t))
      (set-cursor-color myCursorColor_ime) (setq cursor-type '(bar . 3)))
     ((and view-mode  (if (featurep 'evil) (or (evil-insert-state-p) (evil-emacs-state-p)) t))
      (set-cursor-color myCursorColor_view) (setq cursor-type 'hollow))
     ((and (featurep 'evil) (or (evil-normal-state-p) (evil-visual-state-p))) (evil-refresh-cursor))
     (t (set-cursor-color myCursorColor) (setq cursor-type '(bar . 3))))))

(defmacro my:macro-after-ad-refresh-cursor (f)
 `(defadvice ,f (after my:after-ad-refresh-cursor-color activate)
    (my:refresh-cursor)))

(my:macro-after-ad-refresh-cursor select-window)
(my:macro-after-ad-refresh-cursor view-mode)
(my:macro-after-ad-refresh-cursor delete-window)
(my:macro-after-ad-refresh-cursor kill-buffer)
(my:macro-after-ad-refresh-cursor toggle-input-method)
;; (my:macro-after-ad-refresh-cursor keyboard-quit)
;; (my:macro-after-ad-refresh-cursor force-mode-line-update)
(with-eval-after-load 'smartrep
  (my:macro-after-ad-refresh-cursor smartrep-do-fun)
  ;; (my:macro-after-ad-refresh-cursor smartrep-map-internal) ; doesn't work
  )
(with-eval-after-load 'evil
  (my:macro-after-ad-refresh-cursor evil-insert-state))
(add-hook 'window-configuration-change-hook 'my:refresh-cursor)
;; (add-hook 'focus-in-hook 'my:refresh-cursor)
(when (eq system-type 'windows-nt)
  (add-hook 'w32-ime-on-hook
            (function (lambda () (set-cursor-color myCursorColor_ime) (setq cursor-type '(bar . 3)))))
  (add-hook 'w32-ime-off-hook
            (function (lambda () (set-cursor-color myCursorColor) (setq cursor-type '(bar . 3)))))
  )
(when (equal system-type 'darwin)
  (add-hook 'mac-selected-keyboard-input-source-change-hook 'my:refresh-cursor))

(with-eval-after-load 'multiple-cursors
  (add-hook 'multiple-cursors-mode-enabled-hook (function (lambda () (set-cursor-color myCursorColor_mc) (setq cursor-type 'box))))
  (add-hook 'multiple-cursors-mode-disabled-hook 'my:refresh-cursor)
  ;; (add-hook 'multiple-cursors-mode-hook 'my:refresh-cursor)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-mode
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :defer t
  :config
  (custom-set-variables '(rainbow-delimiters-max-face-count 7))
  )

;; "#EF845C"
;; "#F9C270"
;; "#FFF67F"
;; "#69BD83"
;; "#54C3F1"
;; "#796BAF"
;; "#BA79B1"
;; "#DD6673"

;; "#F39800"
;; "#F3E100"
;; "#86B81B"
;; "#00958D"
;; "#0097DB"
;; "#0062AC"
;; "#8A017C"
;; "#D60077"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smooth scroll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smooth-scroll
  :config
  (diminish 'smooth-scroll-mode "")
  (custom-set-variables '(smooth-scroll/vscroll-step-size 3))
  (smooth-scroll-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anzu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package anzu
  :init
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000)
   '(anzu-minimum-input-length 3)
   '(anzu-use-migemo nil))
  (defadvice isearch-mode (before my:advice-turn-on-anzu-mode activate)
    (global-anzu-mode 1))
  :bind (("C-c r" . anzu-query-replace)
         ;; ("C-c R" . anzu-query-replace-regexp)
         )
  :config
  (global-anzu-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; visual-regexp-steroids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package visual-regexp-steroids
  :init (setq vr/engine 'pcre2el)
  :bind (("C-c R" . vr/query-replace)
         ("C-c m" . vr/mc-mark))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :config
  (diminish 'undo-tree-mode "")
  (global-undo-tree-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "evil-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package open-junk-file :defer t
  :init
  (defvar my:junk-dir "~/junk/")
  (setq open-junk-file-format (concat my:junk-dir "%Y/%m/%d-%H%M%S."))
  (use-package helm-ag :defer t
    :init
    (defun my:junk-search ()
      (interactive)
      (unless (file-exists-p my:junk-dir) (error "Junk files not found"))
      (helm-do-ag my:junk-dir)))
  :commands open-junk-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sublimity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package sublimity
  :defer t
  :config
  (use-package sublimity-map
    :config
    (sublimity-map-set-delay nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; google c style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package google-c-style
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quickrun (slow to load)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quickrun
  :init
  (push '("*quickrun*") popwin:special-display-config)
  :bind ("<f5>" . quickrun)
  :config
  ;; (bind-key "<f5>" 'quickrun)
  (custom-set-variables '(quickrun-timeout-seconds 300))
  ;; Associate .plt file to gnuplot
  (add-to-list 'quickrun-file-alist '("\\.plt\\'" . "gnuplot"))
  (quickrun-add-command "gnuplot"
                        '((:command . "gnuplot")
                          (:exec . ("%c -e \"set term post eps enh color dashlength 1.5 font 20; set output 'temp_output.eps'\" %s"
                                    "sumatrapdf -reuse-instance temp_output.eps"))))
  (quickrun-add-command "python"
                        '((:command . "python")
                          (:compile-only . "pyflakes %s")
                          (:description . "Run Python script")
                          (:remove . ("%s")))
                        :default "python")
  (when (eq system-type 'windows-nt)
    (defun my:quickrun-hook()
      (set-buffer-process-coding-system 'sjis 'utf-8))
    (add-hook 'quickrun/mode-hook 'my:quickrun-hook))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; irony mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package irony
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (custom-set-variables
   '(irony-server-install-prefix "~/.emacs.d/irony/")
   '(irony-server-build-dir "~/.emacs.d/irony/build/")
   '(irony-user-dir "~/.emacs.d/irony/"))
  (defun my-irony-mode-setup ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (defun my:company-irony-setup ()
    (add-to-list (make-local-variable 'company-backends) '(company-irony :with company-yasnippet))
    (use-package company-irony-c-headers
      :config
      (add-to-list (make-local-variable 'company-backends) 'company-irony-c-headers)))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'my-irony-mode-setup)
  (add-hook 'irony-mode-hook 'my:company-irony-setup)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function-args
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package function-args
  :defer t
  :init
  (defun my:function-args-mode-setup ()
    (function-args-mode 1))
  (add-hook 'c++-mode-hook 'my:function-args-mode-setup)
  (add-hook 'c-mode-hook 'my:function-args-mode-setup)
  :config
  (bind-keys :map function-args-mode-map
             ("M-i" . nil)
             ("M-o" . nil)
             ("M-n" . nil)
             ("M-h" . nil)
             ("M-u" . nil)
             ("M-j" . nil)
             ("C-M-j" . nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake mode (cmake-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cmake-mode :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck :defer t
  :config
  (use-package flycheck-pos-tip
    :config
    (flycheck-pos-tip-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jedi (python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jedi-core
  :defer t
  :init
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (defun my:company-jedi-setup ()
    (use-package company-jedi
    :config
    (add-to-list (make-local-variable 'company-backends)
                 '(company-jedi :with company-yasnippet company-dabbrev-code))))
  (add-hook 'python-mode-hook 'my:company-jedi-setup)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cython-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cython-mode
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)
         ("\\.pxi\\'" . cython-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EIN (Emacs Ipython Notebook)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ein
  :defer t
  :init
  (defun my:ein-setup ()
    (define-key ein:notebook-mode-map (kbd "<S-return>") 'ein:worksheet-execute-cell-and-insert-below))
  (add-hook 'ein:notebook-mode-hook 'my:ein-setup)
  :config
  (custom-set-variables '(ein:use-auto-complete t)
                        '(ein:use-smartrep t))
  (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Check existence shell
;; (defun skt:shell ()
;;   (or (executable-find "zsh")
;;       (executable-find "bash")
;;       ;; (executable-find "f_zsh") ;; In case of Emacs + Cygwin
;;       ;; (executable-find "f_bash") ;; In case of Emacs + Cygwin
;;       (executable-find "cmdproxy")
;;       (error "can't find 'shell' command in PATH!!")))

;; (setq shell-file-name (skt:shell))
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.text\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode))
  :init
  (defun markdown-custom ()
    "markdown-mode-hook"
    ;; Read source from stdin (nil) or a file (t)
    (setq markdown-command-needs-filename nil))
  (add-hook 'markdown-mode-hook 'markdown-custom)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manage-minor-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package manage-minor-mode
  :defer t
  :init
  (setq manage-minor-mode-bals-exclude-list
      '((global (global-font-lock-mode font-lock-mode delete-selection-mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indent-guide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package indent-guide
  :defer t
  :init
  (setq indent-guide-recursive t)
  (setq indent-guide-threshold -1)
  (setq indent-guide-delay nil)
  (custom-set-faces '(indent-guide-face ((t (:inherit font-lock-comment-delimiter-face
                                             :italic nil
                                             :bold nil)))))
  :config
  ;; (with-eval-after-load 'company
  ;;   (add-hook 'company-mode-hook
  ;;             (lambda ()
  ;;               (add-hook 'company-completion-started-hook
  ;;                         (lambda (&optional arg) (indent-guide-mode -1)) nil t)
  ;;               (add-hook 'company-completion-cancelled-hook
  ;;                         (lambda (&optional arg) (indent-guide-mode 1)) nil t)
  ;;               (add-hook 'company-completion-finished-hook
  ;;                         (lambda (&optional arg) (indent-guide-mode 1)) nil t))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; highlight indent guides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package highlight-indent-guides
  :defer t
  :config
  (defun my:set-indent-guide-face ()
    (let* ((base-color (face-attribute 'default :background))
           (lighten-color1 (color-lighten-name base-color 8))
           (lighten-color2 (color-lighten-name base-color 4))
           (darken-color1 (color-darken-name base-color 10))
           (darken-color2 (color-darken-name base-color 4)))
      (custom-set-faces
       `(highlight-indent-guides-odd-face (
                                           (((class color) (background dark)) (:background ,lighten-color1))
                                           (((class color) (background light)) (:background ,darken-color1))))
       `(highlight-indent-guides-even-face (
                                            (((class color) (background dark)) (:background ,lighten-color2))
                                            (((class color) (background light)) (:background ,darken-color2)))))))
  (add-hook 'highlight-indent-guides-mode-hook 'my:set-indent-guide-face)
  (defadvice load-theme (after my:ad-set-indent-guide-face activate)
    (my:set-indent-guide-face))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-newline
  :bind (("RET" . smart-newline)
         ("<S-return>" . newline)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter
  :defer t
  :init
  (defvar git-gutter-mode-map
    (make-sparse-keymap))
  :config
  (diminish 'git-gutter-mode (my:safe-lighter-icon "GG" "code-fork"))
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  (use-package smartrep
    :config
    (smartrep-define-key git-gutter-mode-map
        "C-x" '(("p" . 'git-gutter:previous-hunk)
                ("n" . 'git-gutter:next-hunk))))
  (with-eval-after-load 'evil
    (add-hook 'git-gutter-mode-on-hook
              (lambda ()
                (define-key evil-normal-state-map "[c" 'git-gutter:previous-hunk)
                (define-key evil-normal-state-map "]c" 'git-gutter:next-hunk)))
    (add-hook 'git-gutter-mode-off-hook
              (lambda ()
                (define-key evil-normal-state-map "[c" nil)
                (define-key evil-normal-state-map "]c" nil)))
    ;; (evil-define-key 'normal git-gutter-mode-map "]c" #'git-gutter:next-hunk) ;; doesn't work
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-ps1-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: git-ps1-mode is incompatible with tramp mode
(use-package git-ps1-mode
  :config
  (when (equal system-type 'windows-nt)
    (setq git-ps1-mode-ps1-file "C:\\msys64\\usr\\share\\git\\completion\\git-prompt.sh"))
  (git-ps1-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multicolumn mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multicolumn
  :config
  (multicolumn-global-mode 1)
  (define-key multicolumn-map (kbd "C-x 4 4")
    'multicolumn-delete-other-windows-and-split-with-follow-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package switch-window
  :init
  (setq switch-window-shortcut-style 'qwerty)
  :bind ("C-q o" . switch-window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swap-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package swap-buffers
  :bind ("C-q s" . swap-buffers))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beacon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
  :config
  (diminish 'beacon-mode (my:safe-lighter-icon "*" "lightbulb-o"))
  (beacon-mode 1)
  (custom-set-variables '(beacon-blink-when-focused t)
                        '(beacon-color (face-attribute 'font-lock-warning-face :foreground nil t)))
  (bind-key "M-/" 'beacon-blink)
  (with-eval-after-load 'company
    (add-hook 'company-mode-hook
              (lambda ()
                (add-hook 'company-completion-started-hook
                          (lambda (&optional arg) (beacon-mode -1)) nil t)
                (add-hook 'company-completion-cancelled-hook
                          (lambda (&optional arg) (beacon-mode 1)) nil t)
                (add-hook 'company-completion-finished-hook
                          (lambda (&optional arg) (beacon-mode 1)) nil t))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zoom-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zoom-window
  :bind ("C-q z" . zoom-window-zoom)
  :config
  (setq zoom-window-use-elscreen t)
  (setq zoom-window-mode-line-color "DarkGreen")
  (zoom-window-setup)
  ;; Disable split window
  (defun my:zoom-window-disable-splitting-window ()
    (let ((enabled (zoom-window--enable-p)))
      (if enabled (error "Zoom window is active. Disable zoom window before split window or make new buffer")))
    )
  (defmacro my:before-ad-zoom-window-disable-splitting-window (f)
    `(defadvice ,f (before ad-zoom-window-disable-splitting-window activate)
       (my:zoom-window-disable-splitting-window)))
  ;; Add advice
  (my:before-ad-zoom-window-disable-splitting-window split-window-right)
  (my:before-ad-zoom-window-disable-splitting-window split-window-below)
  (my:before-ad-zoom-window-disable-splitting-window find-file)
  (my:before-ad-zoom-window-disable-splitting-window kill-buffer)
  (with-eval-after-load 'evil
    (my:before-ad-zoom-window-disable-splitting-window evil-window-split)
    (my:before-ad-zoom-window-disable-splitting-window evil-window-vsplit)
    (my:before-ad-zoom-window-disable-splitting-window evil-window-new)
    (my:before-ad-zoom-window-disable-splitting-window evil-window-vnew))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "powerline_config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my note utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package my-note :defer t
  :commands (mynote:make mynote:search)
  :bind (("C-c C-f" . mynote:make)
         ("C-c C-s" . mynote:search)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zen mind mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zen-mind
  :commands Zen:go-deeper
  :init
  (setq Zen:mode-line-format-level2
        (list
         "%e"
         'mode-line-front-space
         'mode-line-mule-info
         'mode-line-client
         'mode-line-modified
         'mode-line-remote
         'mode-line-frame-identification
         '(:eval (shorten-directory default-directory 35))
         'mode-line-buffer-identification
         " "
         '"%p (%3l:%3c)"
         '(elscreen-display-screen-number
           (" " elscreen-mode-line-string))
         " "
         'evil-mode-line-tag
         'smartrep-mode-line-string
         '(vc-mode vc-mode)
         " "
         'mode-line-modes
         'mode-line-misc-info
         'mode-line-end-spaces)
        )
  (setq Zen:mode-line-format-level3 Zen:mode-line-format-level2)
  :config
  ;; Restore fringe width when popup window appears
  (defadvice popwin:display-buffer-1 (before ad-restore-text-area-temporary activate)
    (Zen:restore-text-area))
  ;; Refresh text area size after popup window is closed
  (defadvice popwin:close-popup-window (after ad-return-to-Zen activate)
    (Zen:refresh))
  ;; Set Zen:theme-level1 to current theme
  (advice-add 'Zen:initialization :after '(lambda() (setq Zen:theme-level1 (car custom-enabled-themes))))
  ;; If elscreen tabs are displayed, hide them.
  (defvar my:elscreen-display-tab-p elscreen-display-tab)
  (defadvice Zen:initialization (before ad-hide-elscreen-tab activate)
    (set (make-local-variable 'elscreen-display-tab) nil))
  (defadvice Zen:go-back (before ad-show-elscreen-tab activate)
    (kill-local-variable 'elscreen-display-tab))
  ;; Update screen tab when Zen-mode state changes
  (advice-add 'Zen:go-deeper :after '(lambda() (elscreen-notify-screen-modification 'force)))
  (with-eval-after-load 'evil
    (before-ad-quit-Zen-mind evil-window-split)
    (before-ad-quit-Zen-mind evil-window-vsplit)
    (before-ad-quit-Zen-mind evil-window-new)
    (before-ad-quit-Zen-mind evil-window-vnew))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RexTeX mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package reftex
  :defer t
  :config
  (diminish 'reftex-mode (my:safe-lighter-icon "Ref" "bookmark")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abbrev mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package abbrev
  :defer t
  :config
  (diminish 'abbrev-mode ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flymake
  :defer t
  :config
  (diminish 'flymake-mode " Fm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; outline minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package outline
  :defer t
  :config
  (diminish 'outline-minor-mode (my:safe-lighter-icon "Outl" "th-list")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lice (license and header template)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lice :defer t
  :init
  (setq lice:custom-template-directory
        (expand-file-name "lib/lice" user-emacs-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meigen bot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package meigen-bot
  :config
  (setq meigen-file-path "~/.emacs.d/lib/wisdom/words_of_wisdom.txt")
  (setq my:meigen-already-displayed-p nil)
  (defun my:display-meigen ()
    (when (not my:meigen-already-displayed-p)
      (echo-meigen-to-minibuffer)
      (setq my:meigen-already-displayed-p t)
      (setq meigen-reset-timer (run-at-time "12 hour" nil
                                            (lambda ()
                                              (setq my:meigen-already-displayed-p nil)
                                              (cancel-timer meigen-reset-timer))))))
  (run-with-idle-timer 5 t 'my:display-meigen))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc minor mode lighter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package simple
  :defer t
  :config
  (diminish 'visual-line-mode ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc setkey (set key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-q C-r") 'window-resizer)
(global-set-key (kbd "C-%") 'match-paren)
;; C-w cut word under cursor unless region is selected
(global-set-key (kbd "C-w") 'kill-region-or-word)
;; M-w copy word under cursor unless region is selected
(global-set-key (kbd "M-w") 'save-region-or-word)
;; Copy whole line
(global-set-key (kbd "M-k") 'copy-line)
;; Copy current directory to kill ring
(global-set-key (kbd "C-c 0") 'my:copy-current-path)
;; Revert buffer without confirm
(global-set-key (kbd "M-r") 'revert-buffer-without-confirm)
;; Open lines
(global-unset-key (kbd "C-x o"))
;; Window move and scroll
(use-package smartrep
  :config
  (smartrep-define-key
      global-map "C-x" '(("o" . 'open-line)))
  (smartrep-define-key
      global-map "C-q" '(
                         ;; Other window operations
                         ("n" . (lambda () (scroll-other-window 1)))
                         ("p" . (lambda () (scroll-other-window-down 1)))
                         ("d" . (lambda () (scroll-other-window (/ (window-height) 2))))
                         ("u" . (lambda () (scroll-other-window-down (/ (window-height) 2))))
                         ("SPC" . 'scroll-other-window)
                         ("DEL" . (lambda () (scroll-other-window '-)))
                         ("g" . (lambda () (beginning-of-buffer-other-window 0)))
                         ("G" . (lambda () (end-of-buffer-other-window 0)))
                         ;; Move to other window
                         ("h" . 'windmove-left)
                         ("j" . 'windmove-down)
                         ("k" . 'windmove-up)
                         ("l" . 'windmove-right))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load theme
;;; This is preferred to be put at the last of init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (featurep 'elscreen-persist)
    (defadvice elscreen-persist-restore (after my:ad-modify-theme activate)
      (my:load-default-theme)
      (my:swap-faces-solarized))
  (my:load-default-theme))


;; end of file
