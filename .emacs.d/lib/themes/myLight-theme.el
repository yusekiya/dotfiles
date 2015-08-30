(deftheme myLight
  "My light theme")

(custom-theme-set-faces
 'myLight
 '(cursor ((t (:foreground "LightSlateGrey"))))
 '(default ((t (:foreground "black"
                :background "gray93"))))
 '(mode-line ((t (:foreground "gray0"
                  :background "PaleGreen3"))))
 '(mode-line-inactive ((t (:foreground "white"
                           :background "gray0"))))
 ;; '(mode-line-inactive ((t (:foreground "gray60"
 ;;                           :background "gray15"))))
 '(region ((t (:background "LightSteelBlue1"))))
 '(font-lock-string-face ((t (:foreground "yellow4"))))
 '(font-lock-keyword-face ((t (:foreground "BlueViolet"))))
 '(font-lock-constant-face ((t (:foreground "DodgerBlue4"))))
 ;'(font-lock-constant-face ((t (:foreground "#3B5998"))))
 '(font-lock-type-face ((t (:foreground "VioletRed1"))))
 '(font-lock-comment-face ((t (:foreground "gray45"))))
 '(font-lock-function-name-face ((t (:foreground "#008080"))))
 '(font-lock-builtin-face ((t (:foreground "DarkGreen"))))
 '(font-lock-warning-face ((t (:foreground "white"
                               :background "#ee2c2c"
                               :bold t))))
 '(highlight ((t (:foreground "white"
                  :background "DodgerBlue4"))))
 '(comint-highlight-prompt ((t (:inherit highlight))))
 '(minibuffer-prompt ((t (:foreground "DodgerBlue4"))))
 '(fringe ((t (:inherit default
               :foreground "gray25"
               :background "gray70"))))
 ;; '(show-paren-match ((t (:foreground "firebrick1"
 ;;                         :background "gray93"
 ;;                         :bold t))))
 '(show-paren-match ((t (:foreground "gray93"
                         :background "YellowGreen"
                         :bold t))))
 '(hlline-face ((t (:background "white"
                    :underline nil))))
 '(linum ((t (:background "gray70"
              :foreground "gray25"
              :bold nil))))
 '(linum-highlight-face ((t (:background "gray70"
                             :foreground "firebrick1"
                             :bold t))))
 '(ace-jump-face-foreground ((t (:background "royal blue"
                                 :foreground "white"))))
 '(elscreen-tab-background-face ((t (:foreground "gray60"
                                     :background "gray70"))))
 '(elscreen-tab-current-screen-face ((t (:background "gray93"
                                         :foreground "firebrick1"
                                         :bold t))))
 '(elscreen-tab-other-screen-face ((t (:foreground "white"
                                       :background "gray40"))))
 '(helm-selection ((t (:foreground "black"
                       :background "PaleGreen1"
                       :underline nil))))
 '(mc/cursor-face ((t (:inverse-video nil
                       :background "VioletRed"
                       :foreground "white"))))
 '(anzu-mode-line ((t (:background "white"
                       :foreground "DodgerBlue4"
                       :bold t))))
 '(latex-caption-face ((t (:foreground "gray45"))))
 '(latex-footnote-face ((t (:foreground "yellow4"))))
 '(my-latex-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(my-latex-constant-face ((t (:inherit font-lock-constant-face))))
 '(font-latex-math-face ((t (:foreground "sienna3"))))
 '(font-latex-string-face ((t (:foreground "yellow4"))))
 '(font-latex-sectioning-0-face ((t (:foreground "VioletRed"))))
 '(font-latex-sectioning-1-face ((t (:foreground "VioletRed"))))
 '(font-latex-sectioning-2-face ((t (:foreground "VioletRed"))))
 '(font-latex-sectioning-3-face ((t (:foreground "VioletRed"))))
 '(font-latex-sectioning-4-face ((t (:foreground "VioletRed"))))
 '(font-latex-sectioning-5-face ((t (:foreground "VioletRed"))))
 '(font-latex-superscript-face ((t (:height 0.9))))
 '(font-latex-subscript-face ((t (:height 0.9))))
 '(font-latex-warning-face ((t (:foreground "firebrick1"))))
 '(evil-ex-substitute-replacement ((t (:foreground "red3"))))
 '(evil-ex-info ((t (:foreground "red3"))))
 '(powerline-active1 ((t
                       (:background "PaleGreen3" :foreground "gray0")
                       )))
 '(powerline-inactive1 ((t
                         (:background "gray0" :foreground "white")
                         )))
 '(powerline-active2 ((t
                       (:background "grey40" :foreground "white")
                       )))
 '(powerline-inactive2 ((t
                         (:background "grey40" :foreground "gray80")
                         )))
 )

(provide-theme 'myLight)
