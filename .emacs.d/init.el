;;; -*- mode: Emacs-Lisp; syntax: elisp -*-
;;     Time-stamp: <Aug 05 2016>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings depending on devices
;; This file must be in %HOME% (= (getenv "HOME")) directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting for path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory where files for emacs are located
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
;; elisp load-path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "config/"))
(add-to-list 'load-path (concat user-emacs-directory "config/packages/"))
;; PATH environment
(setq exec-path (parse-colon-path (getenv "PATH")))


;; ;; measure start-up time
;; (require 'initchart)
;; ;; Measure the execution time of a specified function for every call.
;; ;; Optionally, you might give a parameter name of the function you specified to
;; ;; record what value is passed to the function.
;; ;; initchart-visualize-init-sequence visualizes init time chart.
;; (initchart-record-execution-time-of load file)
;; (initchart-record-execution-time-of require feature)

;; window size
(setq default-frame-alist
      (append (list
               ;; size and position
               '(width . 145)  ; the number of characters
               '(height . 38) ; the number of lines
               '(top . 0)    ; y coordiate of frame
               '(left . 0))   ; x coordiate of frame
               default-frame-alist))

(setq display-size (list (display-pixel-width) (display-pixel-height)))

(cond ((string-match "MacBook\\.local$" system-name) (setq font-height 160))
      ((string-match "MacBook-Pro\\.local$" system-name) (setq font-height 140))
      ((equal display-size '(2560 1440)) (setq font-height 140))
      ((equal display-size '(1440 900)) (setq font-height 140))
      (t (setq font-height 120)))

;; fonta
(set-face-attribute 'default nil
                    :family "Ricty Discord"
                    :height font-height)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (cons "Ricty Discord" "iso10646-1"))


;; line spacing
(setq-default line-spacing 2)

;; threshold for garbage collection
(setq gc-cons-threshold 100000000)   ; 100 MB

;; set the security level of the Diffie-Hellman key exchange to default
(setq gnutls-min-prime-bits nil)

;; threshold width for split window
(setq split-width-threshold 140)

;; Load newer file of .el or .elc
(setq load-prefer-newer t)

;; Distinguish version
(when (>= emacs-major-version 26)
  (load "core_config")
  (load "package_config"))

;; custom variables
(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p "~/.emacs.d/custom.el") (load custom-file))

;;start on home directory
(cd "~")


;; end of file
