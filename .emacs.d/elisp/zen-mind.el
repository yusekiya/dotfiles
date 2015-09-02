;;; zen.el --- provide zen-mind-mode                      -*- lexical-binding: t; -*-
;;; coding: utf-8
(require 'manage-minor-mode)

(defvar-local Zen:state 0
  "Current level of Zen (default: 0)")

(defvar Zen:ultimate-level 5 ;; the number of Zen state
  "Do NOT customize this variable!")

(defvar Zen:text-area-width 120
  "Width of the text area represented as the number of colums in Zen-mind.")

(defvar Zen:default-mode-line-format default-mode-line-format)
(defvar Zen:mode-line-format-level1 Zen:default-mode-line-format)
(defvar Zen:mode-line-format-level2
  (list
   "%e"
   'mode-line-front-space
   'mode-line-mule-info
   'mode-line-client
   'mode-line-modified
   'mode-line-remote
   'mode-line-frame-identification
   'mode-line-buffer-identification
   " "
   '"%p (%3l:%3c) "
   '(vc-mode vc-mode)
   " "
   'mode-line-modes
   'mode-line-misc-info
   'mode-line-end-spaces)
  )
(defvar Zen:mode-line-format-level3 Zen:mode-line-format-level2)
(defvar Zen:mode-line-format-level4
  (list
   "%e"
   'mode-line-front-space
   'mode-line-mule-info
   'mode-line-client
   'mode-line-modified
   'mode-line-remote
   'mode-line-frame-identification
   'mode-line-buffer-identification
   " "
   '"%p (%3l:%3c)"
   ))
(defvar Zen:mode-line-format-level5 '(""))

(defvar Zen:default-theme (car custom-enabled-themes))
(defvar Zen:theme-level1 Zen:default-theme)
(defvar Zen:theme-level2 'zenburn)
(defvar Zen:theme-level3 'zenburn)
(defvar Zen:theme-level4 'zenburn)
(defvar Zen:theme-level5 'zenburn)


(defun Zen:initialization ()
  "Initialize variables for Zen-mind."
  (delete-other-windows)
  (setq Zen:state 0)
  (setq Zen:default-mode-line-format mode-line-format)
  (setq Zen:default-theme (car custom-enabled-themes))
  (set-frame-parameter nil 'fullscreen 'fullscreen)
  (Zen:shrink-text-area))

(defun Zen:current-theme ()
  (cond ((= Zen:state 1) Zen:theme-level1)
        ((= Zen:state 2) Zen:theme-level2)
        ((= Zen:state 3) Zen:theme-level3)
        ((= Zen:state 4) Zen:theme-level4)
        ((= Zen:state 5) Zen:theme-level5)
        (t Zen:default-theme)))

(defun Zen:compare-theme (theme)
  (string= theme (Zen:current-theme)))

(defun Zen:update-theme(theme)
  "Change current theme to THEME.
If THEME is nil, just disable all enabled themes."
  (unless (Zen:compare-theme theme)
    (mapc 'disable-theme custom-enabled-themes)
    (when theme
      (load-theme theme))))

;; to be obsoleted
(defun Zen:convert-column-width-to-pixel-width (width)
  "Returns pixel width of WIDTH described as the number of columns.
WIDTH is a integer of a float number. The return is a integer."
  (let ((window-width (window-width))
        (window-width-in-pixel (window-pixel-width)))
    (round (* width (/ (float window-width-in-pixel) window-width)))))

(defun Zen:shrink-text-area ()
  "Set text area width to `Zen:text-area-width'.
If it exceeds current window width, then do nothing."
  (let ((margin-width 0)
        (current-window-width (window-width)))
    (when (< Zen:text-area-width current-window-width)
      (setq margin-width (/ (- current-window-width Zen:text-area-width) 2))
      (set-window-margins nil margin-width margin-width))))

(defun Zen:restore-text-area ()
  "Set text area witdth to default unconditionally."
  (set-window-margins nil left-margin-width right-margin-width))

(defun Zen:refresh-text-area-width ()
  "Refresh text area width according to current level."
  (when (> Zen:state 0)
    (Zen:restore-text-area)
    (Zen:shrink-text-area)))

(defun Zen:go-back ()
  "Restore settings to default."
  (interactive)
  (when (> Zen:state 0)
    (widen)
    (set-frame-parameter nil 'fullscreen nil)
    (Zen:restore-text-area)
    (Zen:update-theme Zen:default-theme)
    (font-lock-mode)
    (read-only-mode -1)
    (manage-minor-mode-restore-from-bals)
    (setq mode-line-format Zen:default-mode-line-format)
    (setq Zen:state 0)))

(defalias 'Zen:revert 'Zen:go-back
  "Restore settings to default.")

(defun Zen:step-out-from-level-0 ()
  "Go to Zen state 1."
  (setq mode-line-format Zen:mode-line-format-level1)
  (setq Zen:state 1))

(defun Zen:step-out-from-level-1 ()
  "Go to Zen state 2."
  (setq mode-line-format Zen:mode-line-format-level2)
  (setq Zen:state 2))

(defun Zen:step-out-from-level-2 ()
  "Go to Zen state 3."
  (setq mode-line-format Zen:mode-line-format-level3)
  (manage-minor-mode-bals)
  (setq Zen:state 3))

(defun Zen:step-out-from-level-3 ()
  "Go to Zen state 4."
  (setq mode-line-format Zen:mode-line-format-level4)
  (font-lock-mode -1)
  (setq Zen:state 4))

(defun Zen:step-out-from-level-4 ()
  "Go to Zen state 5."
  (setq mode-line-format Zen:mode-line-format-level5)
  (cua-set-mark)
  (narrow-to-region (point) (point))
  (read-only-mode 1)
  (setq Zen:state 5)
  (message "Use Zen:go-back to revert"))

(defun Zen:step-out-from-current-level (level)
  "Ge to Zen state LEVEL+1"
  (cond ((= level 0) (Zen:update-theme Zen:theme-level1) (Zen:step-out-from-level-0))
        ((= level 1) (Zen:update-theme Zen:theme-level2) (Zen:step-out-from-level-1))
        ((= level 2) (Zen:update-theme Zen:theme-level3) (Zen:step-out-from-level-2))
        ((= level 3) (Zen:update-theme Zen:theme-level4) (Zen:step-out-from-level-3))
        ((= level 4) (Zen:update-theme Zen:theme-level5) (Zen:step-out-from-level-4))))

;; This function must be improved
(defun Zen:goto-* (level)
  "Go to Zen state LEVEL."
  (when (> level Zen:state)
    (if (= level 0) (Zen:initialization))
    (if (> level 0) (Zen:step-out-from-level-0))
    (if (> level 1) (Zen:step-out-from-level-1))
    (if (> level 2) (Zen:step-out-from-level-2))
    (if (> level 3) (Zen:step-out-from-level-3))
    (if (> level 4) (Zen:step-out-from-level-4))
    (if (> level Zen:ultimate-level) (message "You have achieved the deepest level."))
    (cond ((= Zen:state 1) (Zen:update-theme Zen:theme-level1))
          ((= Zen:state 2) (Zen:update-theme Zen:theme-level2))
          ((= Zen:state 3) (Zen:update-theme Zen:theme-level3))
          ((= Zen:state 4) (Zen:update-theme Zen:theme-level4))
          ((= Zen:state 5) (Zen:update-theme Zen:theme-level5)))))

(defun Zen:go-deeper ()
  "Increase level of Zen."
  (interactive)
  (if (= Zen:state 0) (Zen:initialization))
  (if (< Zen:state Zen:ultimate-level)
      (Zen:step-out-from-current-level Zen:state) (message "You have achieved the deepest level.")))

(defun Zen:refresh ()
  "Reload setting for current Zen state."
  (interactive)
  (if (> Zen:state 0) (Zen:refresh-text-area-width)))

(add-hook 'kill-buffer-hook 'Zen:go-back)
(defmacro before-ad-quit-Zen-mind (f)
  `(defadvice ,f (before ad-quit-Zen-mind activate)
     (Zen:go-back)))
(progn
  (before-ad-quit-Zen-mind split-window-right)
  (before-ad-quit-Zen-mind split-window-below)
  (before-ad-quit-Zen-mind find-file))

(provide 'zen-mind)
