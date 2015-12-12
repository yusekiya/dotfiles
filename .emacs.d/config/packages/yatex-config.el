;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yatex
  :mode (("\\.tex$" . yatex-mode)
         ("\\.ltx$" . yatex-mode)
         ("\\.cls$" . yatex-mode)
         ("\\.sty$" . yatex-mode)
         ("\\.clo$" . yatex-mode)
         ("\\.bbl$" . yatex-mode))
  :config
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-inhibit-prefix-letter t)
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-environment-indent 4)
  (setq YaTeX-sectioning-indent 4)
  (setq YaTeX-use-LaTeX2e t)
  (setq YaTeX-use-AMS-LaTeX t)
  ;; commands for windows
  (when (eq system-type 'windows-nt)
    (setq YaTeX-dvi2-command-ext-alist
          '(("SumatraPDF\\|TeXworks\\|evince\\|okular\\|firefox\\|chrome\\|AcroRd32\\|pdfopen" . ".pdf")))
    (setq tex-command "ptex2pdf -u -l -e -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1\"")
    (setq bibtex-command "latexmk -e \"$latex=q/uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/makeindex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
    (setq makeindex-command "latexmk -e \"$latex=q/uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/makeindex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
    (setq dvi2-command "rundll32 shell32,ShellExec_RunDLL SumatraPDF -reuse-instance")
    (setq dviprint-command-format "powershell -Command \"& {$r = Write-Output %s;$p = [System.String]::Concat('\"\"\"',[System.IO.Path]::GetFileNameWithoutExtension($r),'.pdf','\"\"\"');Start-Process pdfopen -ArgumentList ('--rxi','--file',$p)}\""))
  ;; Reference level
  ;; 1:?, 2:section, 3:subsection, 4:subsubsection 
  (setq YaTeX::ref-labeling-section-level 4)
  ;; Don't close parenthesis automatically
  (setq YaTeX-close-paren-always 'never)
  ;; Add math-sign
  (setq
   YaTeX-math-sign-alist-private
   '(
     ("q"         "Q"          	"(Q)")
     ("z"         "Z"          	"ZZ")
     ("t"		"text"		"text")
     ("qu"        "quad"         	"__")
     ("qq"        "qquad"         "____")
     ("ls"        "varlimsup"     "___\nlim")
     ("li"        "varliminf"     "lim\n---")
     ("il"        "varinjlim"     "lim\n-->")
     ("pl"        "varprojlim"    "lim\n<--")
     ("st"        "text{ s.t. }" "s.t.")
     ("bigop"     "bigoplus"      "_\n(+)~")
     ("bigot"     "bigotimes"     "_\n(x)\n ~")
     ))
  ;; Color
  (setq YaTeX-use-font-lock t)
  ;; Use popwin for some buffers
  (use-package popwin
    :config
    (defadvice YaTeX-showup-buffer (around popwin-yatex:YaTeX-showup-buffer (buffer &optional func select) activate)
      (popwin:display-buffer-1 buffer
                               :default-config-keywords `(:noselect ,(not select))
                               :if-config-not-found (lambda (buffer) ad-do-it)))
    (push '("*YaTeX-typesetting*") popwin:special-display-config)
    (push '("*dvi-preview*") popwin:special-display-config)
    (push '("*Label completions*") popwin:special-display-config)
    (push '("*RefTeX Select*") popwin:special-display-config))
  ;; Turn on auto complete
  ;; (use-package auto-complete
  ;;   :config
  ;;   (add-to-list 'ac-modes 'yatex-mode)
  ;;   (auto-complete-mode 1))
  ;; ;; Turn on yasnippet mode
  ;; (use-package yasnippet
  ;;   :config
  ;;   (yas-minor-mode-on))
  (use-package key-chord
    :config
    (key-chord-define YaTeX-mode-map "ds" 'insert-backslash)
    (key-chord-define YaTeX-mode-map "jv" 'insert-subscript)
    (key-chord-define YaTeX-mode-map "jr" 'insert-superscript)
    (key-chord-define YaTeX-mode-map "jf" 'insert-sub-and-sup))
  (bind-keys :map YaTeX-mode-map
             ("C-," . my:goto-blank-brackets-backward)
             ("C-." . my:goto-blank-brackets-forward)
             ("C-j" . YaTeX-intelligent-newline))
  (use-package font-latex)
  (use-package preview)
  (use-package flyspell)
)

;; ac-latex
;; (defun my:setting-ac-for-latex ()
;;   (require 'auto-complete-latex)
;;   (setq ac-l-dict-directory (concat user-emacs-directory "lib/acmode/ac-l-dict/"))
;;   ;; (add-to-list 'ac-l-sources 'ac-source-yasnippet)
;;   (setq ac-l-sources (append '(ac-source-yasnippet ac-source-words-in-same-mode-buffers) ac-l-sources))
;;   (ac-l-setup)
;;   )
;; (add-hook 'yatex-mode-hook 'my:setting-ac-for-latex)

;; RefTeX mode
(defun my:RefTeX-setup-for-yatex ()
  (turn-on-reftex)
  (setq reftex-label-alist '(AMSTeX))
  (custom-set-variables '(reftex-toc-split-windows-horizontally t))
)
(add-hook 'yatex-mode-hook 'my:RefTeX-setup-for-yatex)

;; Outline mode
(defun latex-outline-level ()
  (interactive)
  (let ((str nil))
	(looking-at outline-regexp)
	(setq str (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
	(cond
	 ((string-match "documentclass" str) 1)
	 ((string-match "documentstyle" str) 1)
	 ((string-match "part" str) 2)
	 ((string-match "chapter" str) 3)
	 ((string-match "appendix" str) 3)
	 ((string-match "subsubsection" str) 6)
	 ((string-match "subsection" str) 5)
	 ((string-match "section" str) 4)
	 (t (+ 6 (length str)))
	 )))

(defun my:yatex-outline-mode-setup ()
  (setq outline-level 'latex-outline-level)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp
        (concat "[ \t]*" (regexp-quote "\\")
                "\\(appendix\\|documentstyle\\|documentclass\\|part\\|chapter\\|section\\|"
                "subsection\\|subsubsection\\|paragraph\\|subparagraph\\)"
                "\\*?[ \t]*[[{]"))
  ;; (concat "[ \t]*\\\\\\(documentstyle\\|documentclass\\|"
  ;;         "part\\|chapter\\|appendix\\|section\\|subsection\\|subsubsection\\)"
  ;;         "\\*?[ \t]*[[{]"))
  (outline-minor-mode t))

(add-hook 'yatex-mode-hook 'my:yatex-outline-mode-setup)

(defun my:yatex-mode-setup ()
  (setq auto-fill-function nil)
  ;; setup font-latex
  (if (featurep 'font-latex) (font-latex-setup))
  ;; modify syntax
  (modify-syntax-entry ?% "<" (syntax-table))
  (modify-syntax-entry 10 ">" (syntax-table))
  (if (featurep 'flyspell) (flyspell-mode 1))
  )
(add-hook 'yatex-mode-hook 'my:yatex-mode-setup)


;; -------------------------------------------------------------------
;; Definition of functions for YaTeX
;; search for sumatrapdf
(defun sumatrapdf-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "C:/w32tex/NDde/Binary/fwdsumatrapdf")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat pf " " ctf " " ln))
    (process-query-on-exit-flag
      (start-process-shell-command "fwdsumatrapdf" nil cmd args))))

;;; search for dviout
(defun dviout-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (df)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "C:/w32tex/dviout/dviout")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq df (concat (car (split-string mtf "\\.")) ".dvi"))
    (setq args (concat " -1 " df " # " ln " " ctf))
    (process-query-on-exit-flag
      (start-process-shell-command "dviout" nil cmd args))))

;; Just insert "\"
(defun insert-backslash ()
  (interactive)
  (insert "\\")
)

;;; subscript
(defun insert-subscript ()
  (interactive)
  (insert "_{}")
  (backward-char 1)
)

;;; superscript
(defun insert-superscript ()
  (interactive)
  (insert "^{}")
  (backward-char 1)
)

;;; subscript and superscript
(defun insert-sub-and-sup ()
  (interactive)
  (insert "_{}^{}")
  (backward-char 4)
  )


(defun my-insert-ket (size)
  (cond ((string= size "") (insert "| {} \\rangle"))
        ((string= size "l") (insert "\\bigl| {} \\bigr>"))
        ((string= size "L") (insert "\\Bigl| {} \\Bigr>"))
        ((string= size "h") (insert "\\biggl| {} \\biggr>"))
        ((string= size "H") (insert "\\Biggl| {} \\Biggr>"))
        ((string= size "r") (insert "\\left| {} \\right>"))
        (t (insert "| {} \\rangle"))
        )
  (my:goto-blank-brackets-backward)
)

;; end of file
