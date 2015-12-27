;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.java\\'" 'utf-8)              ; Java
(modify-coding-system-alist 'file "\\.py\\'" 'utf-8)                ; Python
(modify-coding-system-alist 'file "\\.c\\'" 'utf-8)                ; c
(modify-coding-system-alist 'file "\\.cpp\\'" 'utf-8)                ; c++

(when (eq system-type 'windows-nt)
  (setq default-file-name-coding-system 'shift_jis))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IME setting for japanese environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for windows
(when (eq system-type 'windows-nt)
  (set-keyboard-coding-system 'japanese-shift-jis)
  ;; (set-keyboard-coding-system 'utf-8)
  (setq default-input-method "W32-IME")
  (w32-ime-initialize)
  (setq ime-enable-document-feed nil)
  ;; Support incremental search in Japanese
  (defun w32-isearch-update ()
    (interactive)
    (isearch-update))
  (define-key isearch-mode-map [compend] 'w32-isearch-update)
  (define-key isearch-mode-map "\C-o" 'isearch-toggle-input-method)
  (add-hook 'isearch-mode-hook
            (lambda () (setq w32-ime-composition-window (minibuffer-window))))
  (add-hook 'isearch-mode-end-hook
            (lambda () (setq w32-ime-composition-window nil)))
  ;; Disable IME on yes/no query
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
  )

;; dabbrev for japanese
(setq case-replace nil)             ;dabbrev exact (upper or lower)case
(setq dabbrev-case-fold-search nil) ;dabbrev exact case
                                        ;(load "dabbrev-ja")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; region color
(global-font-lock-mode t)  ; enalbe global-font-lock-moe
(transient-mark-mode t)    ; enable region color

(defvar dark_BG "#2d3743")
(defvar dark_FG "gray90")
(defvar myCursorColor "VioletRed")
;; set other default-frame-alist
(if window-system
    (progn
      ;; Foreground color
      (add-to-list 'default-frame-alist `(foreground-color . ,dark_FG))
      ;; Background color
      (add-to-list 'default-frame-alist `(background-color . ,dark_BG))
      ;; Cursor color
      (add-to-list 'default-frame-alist `(cursor-color . ,myCursorColor))
      (add-to-list 'default-frame-alist '(cursor-type  . '(bar . 3)))
      ;; Mouse pointer color
      (add-to-list 'default-frame-alist '(mouse-color . "white"))
      ;; Symbol sizse in fringe
      (add-to-list 'default-frame-alist '(left-fringe . 14))
      (add-to-list 'default-frame-alist '(right-fringe . 14))))

;; Default transparency
(add-to-list 'default-frame-alist '(alpha . 90))

;; Inhibit memubar, scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Cursor
(setq blink-cursor-delay 1.0)
(setq blink-cursor-interval 0.75)
(blink-cursor-mode 1)
(setq-default cursor-in-non-selected-windows nil)

;; Trancation lines
(setq truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; Emphasize corresponding parentheses
(show-paren-mode t)

;; Don't blink matching parenthesis
(setq blink-matching-paren nil)

;; Show end of buffer in fringe
(setq-default indicate-buffer-boundaries 'left)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behavior configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide password in shell-mode
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; tab width
(setq-default tab-width 4)
(setq tab-stop-list '(4 8 12))
;; Use whitespace instead of tab
(setq-default indent-tabs-mode nil)

;; Inhibit startup screen
(setq inhibit-startup-message t)
;; Emacs starts on buffer menu
(buffer-menu)

;; tmp file saving directory
(setq auto-save-default nil)
;; (setq auto-save-list-file-prefix "~/.emacs.d/backup")

;; backup.file~ location
(setq make-backup-files nil)
;; (setq backup-directory-alist
;;       (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
;;             backup-directory-alist))

;; Remove attributes from text in kill-ring
(defadvice kill-new (around my-kill-ring-disable-text-property activate)
  (let ((new (ad-get-arg 0)))
    (set-text-properties 0 (length new) nil new)
    ad-do-it))

;; Don't omit argument for elisp (?)
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

;; Resurrect cursor point
(require 'saveplace)
(setq-default save-place t)

;; Automatically reload updated file
(global-auto-revert-mode 1)
;; ;; Reload file under version control system
;; (setq auto-revert-check-vc-info t)

;; auto-insert LAST-MODIFIED-DATE
(if (not (memq 'time-stamp write-file-hooks))
    (setq write-file-hooks
          (cons 'time-stamp write-file-hooks)))
(setq time-stamp-line-limit 40)
(setq time-stamp-format "%3b %02d %:y")
(setq system-time-locale "C")

;; Beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Diff
(setq diff-switches "-u")

;; Enable CUA mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
;; Set key
(define-key global-map (kbd "C-x SPC") 'cua-set-rectangle-mark)

(setq dabbrev-ignored-buffer-names '("*Messages*"))

;; Scrolling
(setq scroll-conservatively 35
      scroll-margin 3
      ;; scroll-step 1
      )
(setq comint-scroll-show-maximum-output t) ;; for shell-mode

;; Visual line mode
(global-visual-line-mode 1)
(custom-set-variables '(visual-line-fringe-indicators '(t t)))

;; Enable of overwrite and deletion of selected region
(delete-selection-mode 1)

;; Maximum number of items for recentf
(setq recentf-max-saved-items 200)

;; Make connection speed with external program faster
(setq w32-pipe-read-delay 0)

;; Don't indent automatically
(electric-indent-mode -1)

;; Enable narrowing
;;(put 'narrow-to-page 'disabled nil)
;;(put 'narrow-to-region 'disabled nil)

;; Load newer file of .el or .elc
(setq load-prefer-newer t)

;; Do NOT show message in scratch buffer
(setq initial-scratch-message nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto mode
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set key and alias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\C-h" 'backward-delete-char) ; C-h as BackSpace
(global-set-key "\M-?" 'help-for-help)        ; M-? as help
;;(global-set-key "\C-q" 'quoted-insert)      ; C-q original
(define-key isearch-mode-map "\C-h" 'isearch-delete-char) ;; enable \C-h in isearch mode
;; Make C-q to a prefix key
(define-key global-map "\C-q" (make-sparse-keymap))
(global-set-key "\C-q\C-q" 'quoted-insert) ; assign quoted-insert to C-q C-q

;; Kill emacs with M-x exit
(defalias 'exit 'save-buffers-kill-emacs)
(global-unset-key (kbd "C-x C-c"))

;; Reload configuration
(defalias 'reload-emacs '(lambda() (interactive) (load-file "~/.emacs")))

;; Change encoding
(defalias 'change-encoding 'set-buffer-file-coding-system)
;; Change encoding and reload buffer
(defalias 'reload-with-different-encoding 'revert-buffer-with-coding-system)

;; Zoom and zoom-out
(defalias 'zoom-in '(lambda() "`text-scale-adjust' can reset face to global default." (interactive) (text-scale-increase +1)))
(defalias 'zoom-out '(lambda() "`text-scale-adjust' can reset face to global default." (interactive) (text-scale-decrease +1)))

;; Toggle input method
(global-set-key "\C-o" 'toggle-input-method)  ; original C-o was open-line
(global-set-key (kbd "\C-x o") 'open-line) ; assing C-x o to open-line

;; Repeated strike on ESC to escape
(global-set-key (kbd "M-ESC ESC") 'keyboard-quit)

;; Make goto-line accessible
(global-set-key (kbd "M-g") 'goto-line) ; some keybinds go off

;; Cursor move in window
(global-set-key (kbd "C-M-h") (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-M-m") (lambda () (interactive) (move-to-window-line nil)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (move-to-window-line -1)))

;; C-M-d to kill-visual-line
(global-set-key (kbd "C-M-d") 'kill-visual-line)
;; Kill whole line with C-k
(global-set-key (kbd "C-k") 'kill-whole-line)

;; Window move
(global-set-key "\C-ql" 'windmove-right)
(global-set-key "\C-qh" 'windmove-left)
(global-set-key "\C-qj" 'windmove-down)
(global-set-key "\C-qk" 'windmove-up)
;; Change prefix-key for window operation from C-x to C-q
(global-set-key (kbd "C-q o") 'other-window)
(global-set-key (kbd "C-q 0") 'delete-window)
(global-set-key (kbd "C-q 1") 'delete-other-windows)
(global-set-key (kbd "C-q 2") 'split-window-below)
(global-set-key (kbd "C-q 3") 'split-window-right)
(define-key global-map (kbd "C-q 4") ctl-x-4-map)
(define-key global-map (kbd "C-q 5") ctl-x-5-map)

;; Cycle spacing
(global-set-key (kbd "S-SPC") 'cycle-spacing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load configuration for packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "package_config")


;; end of file
