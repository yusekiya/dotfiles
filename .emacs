;;; -*- mode: Emacs-Lisp; syntax: elisp -*-
;;     Time-stamp: <Oct 24 2015>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings depending on devices
;; This file must be in %HOME% (= (getenv "HOME")) directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting for path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote directory for settings and elisps
(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))
;; Local directory for settings and elisps
(defconst my:user-dictionary-directory (expand-file-name "~/"))
;; elisp load-path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "config/packages/"))
;; PATH environment
(setenv "PATH" (concat (getenv "PATH") ";~/bin/"))
(setq exec-path (parse-colon-path (getenv "PATH")))
;; Note directory (open-junk-file)
(defvar my:note-dir "~/junk/")


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

;; (set-face-attribute 'default nil
;;                     :family "Myrica M"
;;                     :height 120)
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   (cons "Myrica M" "iso10646-1"))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0212
;;                   (cons "Myrica M" "iso10646-1"))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'katakana-jisx0201
;;                   (cons "Myrica M" "iso10646-1"))

;; line spacing
(setq-default line-spacing 2)

;; threshold for garbage collection
(setq gc-cons-threshold 134217728)

;; set the security level of the Diffie-Hellman key exchange to default
(setq gnutls-min-prime-bits nil)

(custom-set-variables
 ;; if the width of current window is greater than this value,
 ;; split-window-sensibly can split the window horizontally.
 '(split-width-threshold 140))

;; distinguishing Emacsen
(if (and (>= emacs-major-version 24) (>= emacs-minor-version 4)) (load ".ntemacs24"))

;;start on home directory
(cd "~")


;; end of file
