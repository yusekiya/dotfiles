;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting mainly for built-in feature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager
;; Must be loaded before the other packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; location of packages
;; installed packages are located at default directory
(setq package-user-dir "~/.emacs.d/elpa")
;; installed packages are located at user-specified directory
;; (setq package-user-dir (concat user-emacs-directory "elpa"))
;; (fset 'package-desc-vers 'package--ac-desc-version)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package
;; Although use-package is not built-in feature, it is better
;; to require the package here because the package helps us
;; to describe all the package configuration in an easy way.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Path to file including common package list (one package per line)")
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
    (f-write-text (mapconcat 'symbol-name local_package_list "\n") 'utf-8 file_name))
  )

(defun my:save_package_list()
  "Output installed package list to `my:package_list_file'"
  (interactive)
  (let ((local_package_list)
        (common_package_list)
        (add_list)
        (remove_list))
    ;; Get packages as symbol list
    (setq local_package_list (my:get_local_package_list))
    (setq common_package_list (my:pull_package_list))
    (setq add_list (-difference local_package_list common_package_list))
    (setq remove_list (-difference common_package_list local_package_list))
    (when (and (or add_list remove_list)
               (yes-or-no-p (format "Add: %s\nRemove: %s\nUpdate common package list? "
                               (mapconcat 'symbol-name add_list ", ")
                               (mapconcat 'symbol-name remove_list ", "))))
      ;; Output local_package_list to file
      (f-write-text (mapconcat 'symbol-name local_package_list "\n") 'utf-8 my:package_list_file)
      ;; Output change log
      ;; (my:save_package_diff_log add_list remove_list)
      (my:save_package_log local_package_list)
      (message (concat "Common package list file updated! Check change log in " my:package_list_change_log_dir)))))

(defun my:pull_package_list()
  "Import installed package list from `my:package_list_file', and return the list"
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
  "Delete package. Input PKG is given as a symbol (not string)"
  (let ((pkg_desc (car (assoc-default pkg package-alist))))
    (package-delete pkg_desc)
    ))

(defun my:synchronize_packages()
  "Synchronize packages with the packages listed in `my:package_list_file'"
  (let* ((local_package (my:get_local_package_list))
        (common_package (my:pull_package_list))
        (to_be_installed (-difference common_package local_package))
        (to_be_deleted (-difference local_package common_package))
        (flag_maybe_installed nil)
        (flag_maybe_deleted nil)
        (flag_update nil))
    (when to_be_installed
      (when (yes-or-no-p
             (if (= (length to_be_installed) 1)
                 (format "Package Sync: Install package `%s'? " (symbol-name (car to_be_installed)))
               (format "Package Sync: Install these %d packages (%s)? "
                       (length to_be_installed)
                       (mapconcat 'symbol-name to_be_installed ", ")))
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
             (if (= (length to_be_deleted) 1)
                 (format "Package Sync: Delete package `%s'? " (symbol-name (car to_be_deleted)))
               (format "Package Sync: Delete these %d packages (%s)? "
                       (length to_be_deleted)
                       (mapconcat 'symbol-name to_be_deleted ", "))))
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

;; Update common package file,
;; when packages are installed or deleted through through package-menu-execute
(advice-add 'package-menu-execute :after 'my:save_package_list)
;; and when installed through package-install-from-buffer
;; (c.f. package-install-file calls package-install-from-buffer)
(advice-add 'package-install-from-buffer :after 'my:save_package_list)


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
;; Highlight
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'hl-line)
(use-package hl-line)
(defface hlline-face
  '(;; (((class color) (background dark))
    ;;  (:background "black"
    ;;   :underline nil))
    ;; (((class color) (background light))
    ;;  (:background "gray90"
    ;;   :underline nil))
    (t ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode 1);enable


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
         (bind-keys :map dired-mode-map
                    ((kbd "RET") . dired-find-alternate-file)
                    ("a" . dired-find-file)
                    ("j" . dired-next-line)
                    ("k" . dired-previous-line)
                    ("h" . dired-up-directory)
                    ("l" . ignore)
                    ("s" . dired-rotate-sort))
         (use-package wdired
           ;; :defer t
           ;; :commands wdired-change-to-dired-mode
           :init (bind-keys :map dired-mode-map ("r" . wdired-change-to-wdired-mode)))
         (use-package dired-ranger
           :config
           (bind-keys :map dired-mode-map
                      :prefix "c"
                      :prefix-map dired-ranger-map
                      :prefix-docstring "Map for ranger operations."
                      ("c" . dired-ranger-copy)
                      ("p" . dired-ranger-paste)
                      ("m" . dired-ranger-move)))
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
;; Python mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:python-mode-setup()
  (setq tab-width 4
        python-indent-offset 4
        evil-shift-width 4)
  ;; (highlight-indentation-mode 1)
  (use-package indent-guide
    :config
    (indent-guide-mode 1))
  ;; (use-package yasnippet
  ;;   :config
  ;;   (yas-minor-mode 1)
  ;;   (yas-reload-all))
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
(defcustom my-default-theme 'material "default theme")
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

(defun my:load-default-theme ()
  (load-theme my-default-theme t t)
  (enable-theme my-default-theme)
  )

(add-hook 'after-init-hook 'my:load-default-theme)


(provide 'builtins)
;; end of file
