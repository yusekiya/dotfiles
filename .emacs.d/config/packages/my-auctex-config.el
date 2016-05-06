(when (equal system-type 'windows-nt)
  (with-eval-after-load "latex"
    (setq TeX-engine-alist '((pdfuptex "pdfupTeX"
                                       "ptex2pdf -u -e -ot \"-kanji=utf8 -no-guess-input-enc %S %(mode)\""
                                       "ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc %S %(mode)\""
                                       "euptex")))
    (setq japanese-TeX-engine-default 'pdfuptex)
                                        ;(setq japanese-TeX-engine-default 'luatex)
                                        ;(setq japanese-TeX-engine-default 'xetex)
    (setq TeX-view-program-list '(("SumatraPDF"
                                   "rundll32 shell32,ShellExec_RunDLL SumatraPDF -reuse-instance %s.pdf")))
    (setq TeX-view-program-selection '((output-dvi "SumatraPDF")
                                       (output-pdf "SumatraPDF")))
    (setq japanese-LaTeX-default-style "jsarticle")
                                        ;(setq japanese-LaTeX-default-style "ltjsarticle")
    (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
      (delq (assoc command TeX-command-list) TeX-command-list))
    )
  (setq preview-image-type 'dvipng)
  ;; (setq preview-gs-command "gs")
  ;; (setq preview-image-type 'png)
  (custom-set-variables
   '(preview-LaTeX-command (quote ("platex \"\\nonstopmode\\nofiles\\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\\AtBeginDocument{\\ifx\\ifPreview\\undefined" preview-default-preamble "\\fi}\"%' \"\\input\" %t")))
   )
  (defun my:preview-scale-from-face ()
    `(lambda nil
       (/ ,(* (/ (preview-inherited-face-attribute
                  'preview-reference-face
                  :height 'default) 10.0) 1.1)
          (preview-document-pt))))
  (setq-default preview-scale-function #'my:preview-scale-from-face)
  (custom-set-variables
   '(preview-default-option-list
     (quote
      ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))))
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (add-to-list 'TeX-command-list
                                     '("Latexmk"
                                       "latexmk %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-upLaTeX-pdfdvi"
                                       "latexmk -e \"$latex=q/uplatex %%O -kanji=utf8 -no-guess-input-enc %S %(mode) %%S/\" -e \"$bibtex=q/upbibtex %%O %%B/\" -e \"$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/\" -e \"$makeindex=q/upmendex %%O -o %%D %%S/\" -e \"$dvipdf=q/dvipdfmx %%O -o %%D %%S/\" -norc -gg -pdfdvi %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX-pdfdvi"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-upLaTeX-pdfps"
                                       "latexmk -e \"$latex=q/uplatex %%O -kanji=utf8 -no-guess-input-enc %S %(mode) %%S/\" -e \"$bibtex=q/upbibtex %%O %%B/\" -e \"$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/\" -e \"$makeindex=q/upmendex %%O -o %%D %%S/\" -e \"$dvips=q/dvips %%O -z -f %%S | convbkmk -u > %%D/\" -e \"$ps2pdf=q/ps2pdf.exe %%O %%S %%D/\" -norc -gg -pdfps %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX-pdfps"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-pdfLaTeX"
                                       "latexmk -e \"$pdflatex=q/pdflatex %%O %S %(mode) %%S/\" -e \"$bibtex=q/bibtex %%O %%B/\" -e \"$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/\" -e \"$makeindex=q/makeindex %%O -o %%D %%S/\" -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-LuaLaTeX"
                                       "latexmk -e \"$pdflatex=q/lualatex %%O %S %(mode) %%S/\" -e \"$bibtex=q/upbibtex %%O %%B/\" -e \"$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/\" -e \"$makeindex=q/upmendex %%O -o %%D %%S/\" -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-LuaJITLaTeX"
                                       "latexmk -e \"$pdflatex=q/luajitlatex %%O %S %(mode) %%S/\" -e \"$bibtex=q/upbibtex %%O %%B/\" -e \"$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/\" -e \"$makeindex=q/upmendex %%O -o %%D %%S/\" -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaJITLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-XeLaTeX"
                                       "latexmk -e \"$pdflatex=q/xelatex %%O %S %(mode) %%S/\" -e \"$bibtex=q/upbibtex %%O %%B/\" -e \"$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/\" -e \"$makeindex=q/upmendex %%O -o %%D %%S/\" -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-XeLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("SumatraPDF"
                                       "rundll32 shell32,ShellExec_RunDLL SumatraPDF -reuse-instance %s.pdf"
                                       TeX-run-discard-or-function t t :help "Forward search with SumatraPDF"))
                        (add-to-list 'TeX-command-list
                                     '("fwdsumatrapdf"
                                       "fwdsumatrapdf %s.pdf \"%b\" %n"
                                       TeX-run-discard-or-function t t :help "Forward search with SumatraPDF"))
                        (add-to-list 'TeX-command-list
                                     '("TeXworks"
                                       "synctex view -i \"%n:0:%b\" -o %s.pdf -x \"texworks --position=%%{page+1} %%{output}\""
                                       TeX-run-discard-or-function t t :help "Run TeXworks"))
                        (add-to-list 'TeX-command-list
                                     '("TeXstudio"
                                       "synctex view -i \"%n:0:%b\" -o %s.pdf -x \"texstudio --pdf-viewer-only --page %%{page+1} %%{output}\""
                                       TeX-run-discard-or-function t t :help "Run TeXstudio"))
                        (add-to-list 'TeX-command-list
                                     '("Firefox"
                                       "powershell -Command \"& {$r = Write-Output %o;$o = [System.String]::Concat('\"\"\"',[System.IO.Path]::GetFileNameWithoutExtension($r),'.pdf','\"\"\"');Start-Process firefox -ArgumentList ('-new-window',$o)}\""
                                       TeX-run-discard-or-function t t :help "Run Mozilla Firefox"))
                        (add-to-list 'TeX-command-list
                                     '("Chrome"
                                       "powershell -Command \"& {$r = Write-Output %s.pdf;$o = [System.String]::Concat('\"\"\"',[System.IO.Path]::GetFullPath($r),'\"\"\"');Start-Process chrome -ArgumentList ('--new-window',$o)}\""
                                       TeX-run-discard-or-function t t :help "Run Chrome PDF Viewer"))
                        (add-to-list 'TeX-command-list
                                     '("pdfopen"
                                       "tasklist /fi \"IMAGENAME eq AcroRd32.exe\" /nh | findstr \"AcroRd32.exe\" > nul && pdfopen --r15 --file %s.pdf && pdfclose --r15 --file %s.pdf & synctex view -i \"%n:0:%b\" -o %s.pdf -x \"pdfopen --r15 --file %%{output} --page %%{page+1}\""
                                       TeX-run-discard-or-function t t :help "Run Adobe Acrobat Reader DC")))))
  )

(when (equal system-type 'darwin)
  (with-eval-after-load "latex"
    (setq TeX-engine-alist '((pdfuptex "pdfupTeX"
                                       "/Library/TeX/texbin/ptex2pdf -u -e -ot \"-kanji=utf8 -no-guess-input-enc %S %(mode)\""
                                       "/Library/TeX/texbin/ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc %S %(mode)\""
                                       "/Library/TeX/texbin/euptex")))
    (setq japanese-TeX-engine-default 'pdfuptex)
    (setq TeX-view-program-selection '((output-dvi "displayline")
                                       (output-pdf "displayline")))
    (setq japanese-LaTeX-default-style "jsarticle")
    (dolist (command '("pTeX" "pLaTeX" "pBibTeX" "jTeX" "jLaTeX" "jBibTeX" "Mendex"))
      (delq (assoc command TeX-command-list) TeX-command-list))
    )
  (setq preview-image-type 'dvipng)
  (defun my:preview-scale-from-face ()
    `(lambda nil
       (/ ,(* (/ (preview-inherited-face-attribute
                  'preview-reference-face
                  :height 'default) 10.0) 1.1)
          (preview-document-pt))))
  (setq-default preview-scale-function #'my:preview-scale-from-face)
  (custom-set-variables
   '(preview-default-option-list
     (quote
      ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))))
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook
            (function (lambda ()
                        (add-to-list 'TeX-command-list
                                     '("Latexmk"
                                       "/Library/TeX/texbin/latexmk %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-upLaTeX-pdfdvi"
                                       "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvipdf=q/dvipdfmx %%O -o %%D %%S/' -norc -gg -pdfdvi %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX-pdfdvi"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-upLaTeX-pdfps"
                                       "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -e '$dvips=q/dvips %%O -z -f %%S | convbkmk -u > %%D/' -e '$ps2pdf=q/ps2pdf %%O %%S %%D/' -norc -gg -pdfps %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-upLaTeX-pdfps"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-pdfLaTeX"
                                       "/Library/TeX/texbin/latexmk -e '$pdflatex=q/pdflatex %%O %S %(mode) %%S/' -e '$bibtex=q/bibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/makeindex %%O -o %%D %%S/' -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-pdfLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-LuaLaTeX"
                                       "/Library/TeX/texbin/latexmk -e '$pdflatex=q/lualatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-LuaJITLaTeX"
                                       "/Library/TeX/texbin/latexmk -e '$pdflatex=q/luajitlatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-LuaJITLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("Latexmk-XeLaTeX"
                                       "/Library/TeX/texbin/latexmk -e '$pdflatex=q/xelatex %%O %S %(mode) %%S/' -e '$bibtex=q/upbibtex %%O %%B/' -e '$biber=q/biber %%O --bblencoding=utf8 -u -U --output_safechars %%B/' -e '$makeindex=q/upmendex %%O -o %%D %%S/' -norc -gg -pdf %t"
                                       TeX-run-TeX nil (latex-mode) :help "Run Latexmk-XeLaTeX"))
                        (add-to-list 'TeX-command-list
                                     '("displayline"
                                       "/Applications/Skim.app/Contents/SharedSupport/displayline %n %s.pdf \"%b\""
                                       TeX-run-discard-or-function t t :help "Forward search with Skim"))
                        (add-to-list 'TeX-command-list
                                     '("Skim"
                                       "/usr/bin/open -a Skim.app %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Skim"))
                        (add-to-list 'TeX-command-list
                                     '("Preview"
                                       "/usr/bin/open -a Preview.app %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Preview"))
                        (add-to-list 'TeX-command-list
                                     '("TeXShop"
                                       "/usr/bin/open -a TeXShop.app %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run TeXShop"))
                        (add-to-list 'TeX-command-list
                                     '("TeXworks"
                                       "/Library/TeX/texbin/synctex view -i \"%n:0:%b\" -o %s.pdf -x \"/Applications/TeXworks.app/Contents/MacOS/TeXworks --position=%%{page+1} %%{output}\""
                                       TeX-run-discard-or-function t t :help "Run TeXworks"))
                        (add-to-list 'TeX-command-list
                                     '("TeXstudio"
                                       "/Library/TeX/texbin/synctex view -i \"%n:0:%b\" -o %s.pdf -x \"/Applications/texstudio.app/Contents/MacOS/texstudio --pdf-viewer-only --page %%{page+1} %%{output}\""
                                       TeX-run-discard-or-function t t :help "Run TeXstudio"))
                        (add-to-list 'TeX-command-list
                                     '("Firefox"
                                       "/usr/bin/open -a Firefox.app %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Mozilla Firefox"))
                        (add-to-list 'TeX-command-list
                                     '("acroread"
                                       "/usr/bin/open -a \"Adobe Acrobat Reader DC.app\" %s.pdf"
                                       TeX-run-discard-or-function t t :help "Run Adobe Acrobat Reader DC")))))
  )

;; Settings independent of OS
(with-eval-after-load "latex"
  (setq font-latex-fontify-script t)
  (custom-set-faces '(font-latex-superscript-face ((t :height 1.0)))
                    '(font-latex-subscript-face ((t :height 1.0))))
  (setq font-latex-script-display '((raise -0.2) raise 0.2))
  (custom-set-variables '(font-latex-fontify-sectioning 'color))
  (setq-default TeX-master nil)
  (bind-keys :map LaTeX-mode-map
             ("C-," . my:goto-blank-brackets-backward)
             ("C-." . my:goto-blank-brackets-forward))
  
  (use-package key-chord
    :config
    (key-chord-define LaTeX-mode-map "ds" 'insert-backslash)
    (key-chord-define LaTeX-mode-map "jv" 'insert-subscript)
    (key-chord-define LaTeX-mode-map "jr" 'insert-superscript)
    (key-chord-define LaTeX-mode-map "jf" 'insert-sub-and-sup))
  (use-package latex-utils
    :config
    (bind-key "C-c c" 'latex-utils:change-parentheses))
  )


;; LaTeX setup
(defun my:latex-setup ()
  (use-package smart-newline
    :config
    (bind-keys :map LaTeX-mode-map
               ("RET" . smart-newline)))
  (use-package flyspell
    :config
    (flyspell-mode 1))
  ;; Regard backslash as usual word
  (modify-syntax-entry ?\\ "w")
  (with-eval-after-load 'company
    (use-package company-auctex
      :config
      (add-to-list (make-local-variable 'company-backends) 'company-yasnippet)
      (company-auctex-init))))

(add-hook 'LaTeX-mode-hook 'my:latex-setup)


;; Yasnippet
(defun my:yasnippet-setup-for-auctex ()
  (when (featurep 'yasnippet)
    (set (make-local-variable 'yas-key-syntaxes) '("w" "w_" "w()" yas-try-key-from-whitespace))))

(add-hook 'LaTeX-mode-hook 'my:yasnippet-setup-for-auctex)


;; RefTeX mode
(defun my:RefTeX-setup-for-auctex ()
  (turn-on-reftex)
  (setq reftex-label-alist '(AMSTeX))
  (setq reftex-plug-into-AUCTeX t)
  (custom-set-variables '(reftex-toc-split-windows-horizontally t))
)
(add-hook 'LaTeX-mode-hook 'my:RefTeX-setup-for-auctex)


;; kinsoku.el
(setq kinsoku-limit 10)


;; My functions
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

(setq latexmkrc-template-alist `(("EN" . ,(expand-file-name
                                           "lib/template/template_en.latexmkrc"
                                           user-emacs-directory))
                                 ("JP (Unicode)" . ,(expand-file-name
                                                     "lib/template/template_jp_unicode.latexmkrc"
                                                     user-emacs-directory))
                                 ("JP (Non unicode)" . ,(expand-file-name
                                                         "lib/template/template_jp_nonunicode.latexmkrc"
                                                         user-emacs-directory))))

(defun my:create_latexmkrc (arg)
  (interactive
   (list
    (completing-read "Choose one: " '("EN" "JP (Unicode)" "JP (Non unicode)"))))
  (let ((file-name (cdr (assoc arg latexmkrc-template-alist))))
    (copy-file file-name (expand-file-name ".latexmkrc" default-directory))))
