;;; custom.el ---                                    -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-isearch-lighter "")
 '(ace-isearch-use-function-from-isearch nil)
 '(anzu-deactivate-region t)
 '(anzu-minimum-input-length 3)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(anzu-use-migemo nil)
 '(auto-insert-directory (concat user-emacs-directory "lib/template/"))
 '(beacon-blink-when-focused t)
 '(beacon-color
   (face-attribute
    (quote font-lock-warning-face)
    :foreground nil t))
 '(custom-safe-themes
   (quote
    ("3c9d994e18db86ae397d077b6324bfdc445ecc7dc81bb9d528cd9bba08c1dac1" "367e859d84bdd85bf9ab7edfee035a7cce5d3db0f75ffaf85e7753da84e4920c" "729b17c0c54263c489d13ac0ce89ef17fcedf9d36f920c635c22a4b33a6ca59d" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" default)))
 '(elscreen-display-tab t)
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(helm-command-prefix-key "C-;")
 '(highlight-symbol-colors (quote ("DarkOrange2" "DodgerBlue2" "DeepPink2")))
 '(highlight-symbol-foreground-color "white")
 '(irony-server-build-dir "~/.emacs.d/irony/build/")
 '(irony-server-install-prefix "~/.emacs.d/irony/")
 '(irony-user-dir "~/.emacs.d/irony/")
 '(package-selected-packages
   (quote
    (zoom-window zenburn-theme yatex with-editor which-key volatile-highlights visual-regexp-steroids viewer use-package swoop switch-window swap-buffers sublimity sublime-themes spacemacs-theme solarized-theme smooth-scroll smartrep smart-newline restart-emacs region-bindings-mode rainbow-mode rainbow-delimiters quickrun powershell powerline popwin paper-theme ov open-junk-file multiple-cursors multicolumn multi-term mozc-popup monokai-theme material-theme markdown-mode manage-minor-mode load-theme-buffer-local lice key-chord indent-guide hlinum highlight-symbol highlight-indentation highlight-indent-guides helm-gtags helm-flyspell helm-flymake helm-flycheck helm-ag goto-last-change google-c-style git-ps1-mode git-gutter function-args fringe-helper fontawesome flycheck-pos-tip flatui-theme farmhouse-theme expand-region exec-path-from-shell evil-visualstar evil-surround evil-numbers evil-nerd-commenter evil-matchit evil-jumper evil-indent-textobject evil-indent-plus evil-exchange evil-args evil-anzu elscreen-persist ein direx dired-ranger dired-filter cython-mode company-statistics company-quickhelp company-jedi company-irony-c-headers company-irony company-auctex color-theme-sanityinc-tomorrow cmake-mode buttercup beacon basic-theme auto-save-buffers-enhanced auto-complete-c-headers auto-complete-auctex aurora-theme all ace-isearch ac-math)))
 '(powerline-default-separator (quote angle))
 '(powerline-height (+ 2 (my:powerline-get-good-height)))
 '(preview-default-option-list
   (quote
    ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels")))
 '(smartrep-mode-line-active-bg nil)
 '(smooth-scroll/vscroll-step-size 3)
 '(solarized-height-minus-1 1.0)
 '(solarized-height-plus-1 1.0)
 '(solarized-height-plus-2 1.0)
 '(solarized-height-plus-3 1.0)
 '(solarized-height-plus-4 1.0)
 '(solarized-use-variable-pitch nil)
 '(vc-handled-backends nil)
 '(visual-line-fringe-indicators (quote (t t)))
 '(which-key-lighter "")
 '(yas-trigger-key "TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-current-screen-face ((t (:inherit default :weight bold))))
 '(indent-guide-face ((t (:inherit font-lock-comment-delimiter-face :italic nil :bold nil)))))
