;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting mainly for non-built-in feature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun change-transparency (num)
  "This command changes the transparency of background.
Input a number within 20 to 100 as the percent of transparency.
If you only input RET without inputting a number, the default is applied."
  (interactive "sPercent of transparency within 20 to 100 (default 90): ")
  (if (string= num "")
      (set-frame-parameter nil 'alpha '(90 40))
    (set-frame-parameter nil 'alpha (string-to-number num))))

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
(defun my:copy-current-path ()                                                         
  (interactive)
  (let ((fPath default-directory))
    (when fPath
      (message "stored path: %s" fPath)
      (kill-new (file-truename fPath)))))


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
  (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$" "\\.ipynb"))
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
;; smartrep seems to conflict with powerline
;; Must be loaded before powerline
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
;; Spell checker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use aspell
(when (equal system-type 'windows-nt)
  (setq-default ispell-program-name "C:\\opt\\Aspell\\bin\\aspell"))
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
;; 日本語対応
(eval-after-load "ispell"
 '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
;; User dictionary
(setq ispell-personal-dictionary (concat my:user-dictionary-directory "en.pws"))

;; flyspell
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (progn (setq flyspell-use-meta-tab nil))
  :config
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
  ;; dired: open file with popwin
  (defun my:find-file-popwin-dired ()
    (interactive)
    (popwin:find-file (dired-get-file-for-visit)))
  (with-eval-after-load 'dired (define-key dired-mode-map "o" 'my:find-file-popwin-dired))
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
          ))
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
  :diminish ""
  :init
  (setq view-read-only t)  
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
(load "yatex-config")


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
;; yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "yasnippet_config") ; should be loaded before auto-complete-mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "acmode")


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
  :diminish ""
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
  :diminish ""
  :config
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

(advice-add 'keyboard-quit :before #'my:refresh-cursor)

(defmacro my:macro-after-ad-refresh-cursor (f)
 `(defadvice ,f (after my:after-ad-refresh-cursor-color activate)
    (my:refresh-cursor)))

(my:macro-after-ad-refresh-cursor select-window)
(my:macro-after-ad-refresh-cursor view-mode)
(my:macro-after-ad-refresh-cursor delete-window)
(my:macro-after-ad-refresh-cursor kill-buffer)
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
(add-hook 'w32-ime-on-hook
          (function (lambda () (set-cursor-color myCursorColor_ime) (setq cursor-type '(bar . 3)))))
(add-hook 'w32-ime-off-hook
          (function (lambda () (set-cursor-color myCursorColor) (setq cursor-type '(bar . 3)))))

(with-eval-after-load 'multiple-cursors
  (add-hook 'multiple-cursors-mode-enabled-hook (function (lambda () (set-cursor-color myCursorColor_mc) (setq cursor-type 'box))))
  (add-hook 'multiple-cursors-mode-disabled-hook 'my:refresh-cursor)
  ;; (add-hook 'multiple-cursors-mode-hook 'my:refresh-cursor)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-mode
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :defer t
  ;; :init
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
  :diminish ""
  :config
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
;; undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "evil-config")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-junk-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package open-junk-file
  :init
  (setq open-junk-file-format (concat my:note-dir "%Y/%m/%d-%H%M%S."))
  :bind ("C-c C-f" . open-junk-file))


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
  :config  
  (defun my-irony-mode-setup ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (require 'ac-irony)
  (defun my-ac-irony-setup ()
    ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
    ;; *may* persist after an expansion.
    (yas-minor-mode 1)
    (auto-complete-mode 1)
    (add-to-list 'ac-sources 'ac-source-irony)
    (define-key irony-mode-map (kbd "M-RET") 'ac-complete-irony-async))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'my-irony-mode-setup)
  (add-hook 'irony-mode-hook 'my-ac-irony-setup))


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
(use-package cmake-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jedi (python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package jedi
  :defer t
  :init
  (defun my:jedi-init ()
    (jedi:setup)
    (define-key python-mode-map (kbd "M-/") 'jedi:complete)
    (define-key jedi-mode-map (kbd "<C-tab>") nil)
    ;; (yas-minor-mode-on)
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    )
  (add-hook 'python-mode-hook 'my:jedi-init)
  ;; (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cython-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cython-mode
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)
         ("\\.pxi\\'" . cython-mode))
  )


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
;; Check existence shell
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      ;; (executable-find "f_zsh") ;; In case of Emacs + Cygwin
      ;; (executable-find "f_bash") ;; In case of Emacs + Cygwin
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode (("\\.text\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode))
  :init
  (defun markdown-custom ()
    "markdown-mode-hook"
    ;; Read source from stdin (nil) or a file (t)
    (setq markdown-command-needs-filename nil))
  (add-hook 'markdown-mode-hook 'markdown-custom))


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
                                             :bold nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-newline
  :bind (("RET" . smart-newline)
         ("<S-return>" . newline)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode line lighter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar mode-line-cleaner-alist
  '( ;; Lighter must starts with space
    (yas-minor-mode . "")
    (helm-mode . "")
    (smooth-scroll-mode . "")
    (volatile-highlights-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    ;; Major modes
    ;; (lisp-interaction-mode . "Li")
    ;; (python-mode . "Py")
    ;; (ruby-mode   . "Rb")
    ;; (emacs-lisp-mode . "El")
    ;; (markdown-mode . "Md")
))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)


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
  (beacon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zoom-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zoom-window
  :bind ("C-q z" . zoom-window-zoom)
  ;; :init
  ;; (defvar my:zoom-window--orig-mode-line nil)
  ;; (defvar my:zoom-window--mode-line-format " Zoom ")
  ;; (defun my:zoom-window--save-orig-mode-line ()
  ;;   (setq my:zoom-window--orig-mode-line mode-line-format))
  ;; (defun my:zoom-window--restore-orig-mode-line ()
  ;;   (setq mode-line-format my:zoom-window--orig-mode-line))
  ;; (defun my:zoom-window--update-mode-line ()
  ;;   (let ((enabled (zoom-window--enable-p)))
  ;;     (if enabled
  ;;         ;; when zoom-window is enabled
  ;;         (progn (my:zoom-window--save-orig-mode-line)
  ;;                (setq mode-line-format (cons my:zoom-window--mode-line-format mode-line-format)))
  ;;         ;; when zoom-window is disabled
  ;;         (my:zoom-window--restore-orig-mode-line))))
  ;; (advice-add 'zoom-window-zoom :after #'my:zoom-window--update-mode-line) 
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
;; git-gutter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package git-gutter
  :diminish (git-gutter-mode "GG")
  :defer t
  :init
  (defvar git-gutter-mode-map
    (make-sparse-keymap))
  :config
  (use-package smartrep
    :config
    (smartrep-define-key git-gutter-mode-map
        "C-x" '(("p" . 'git-gutter:previous-hunk)
                ("n" . 'git-gutter:next-hunk))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; search my notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-ag
  :init
  (defun my:search-notes ()
  (interactive)
  (unless (boundp 'my:note-dir) (error "my:note-dir not found"))
  (helm-ag my:note-dir))
  :defer t)


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

;; Open lines
(global-unset-key (kbd "C-x o"))
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


(provide 'site-lisp)
;; end of file
