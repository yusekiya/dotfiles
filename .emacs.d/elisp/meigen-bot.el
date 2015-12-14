;;; meigen-bot.el --- Echo meigen randomly in Emacs mini-buffer

;; Author: fnwiya
;; URL: https://github.com/fnwiya/meigen-bot
;; Package-Requires: ()
;; Keywords: meigen mini-buffer
;; Version: 0.1

;; Copyright (c) 2015 fnwiya
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This program echo meigen in your minibuffer.

;;; Usage:

;; (add-to-list 'load-path path-to-meigen-bot)
;; (require 'meigen-bot)
;; (setq meigen-file-path path-to-your-meigen-file)
;; (setq meigen-separator "\n")
;; (add-hook 'emacs-startup-hook
;;         (lambda ()
;;           (echo-meigen-to-minibuffer)
;;           ))


;;; Code:

(defcustom meigen-file-path "~/meigen.txt"
  "File path to your meigen file."
  :group 'meigen-bot)

(defcustom meigen-separator "\n"
  "Separator for each meigen(default is '\n')."
  :group 'meigen-bot)


(defun get-sentence-randomly()
  (let* ((random-messages
          (split-string
           (with-temp-buffer
             (insert-file-contents meigen-file-path)
             (buffer-substring-no-properties (point-min) (point-max)))
           meigen-separator))
         (len (length random-messages)))
    (car (nthcdr (random len) random-messages)))
  )

(defun echo-meigen-to-minibuffer()
  (interactive)
  (message (get-sentence-randomly))
  )

(provide 'meigen-bot)
;;; meigen-bot.el ends here
