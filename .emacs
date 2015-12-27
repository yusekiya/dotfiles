;;; -*- mode: Emacs-Lisp; syntax: elisp -*-
;;     Time-stamp: <Dec 22 2015>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings depending on devices
;; This file must be in %HOME% (= (getenv "HOME")) directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setenv "PATH" (concat (getenv "PATH") ";~/bin/"))
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


;; font
(set-face-attribute 'default nil
                    :family "Ricty Discord"
                    :height 120)
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
(setq gc-cons-threshold 1048576)   ; 1 MB

;; set the security level of the Diffie-Hellman key exchange to default
(setq gnutls-min-prime-bits nil)

;; threshold width for split window
(setq split-width-threshold 140)

;; Load newer file of .el or .elc
(setq load-prefer-newer t)

;; Distinguish version
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 4)) (load "core_config"))

;;start on home directory
(cd "~")


;; end of file
