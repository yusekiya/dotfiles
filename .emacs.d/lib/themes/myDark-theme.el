(deftheme myDark
  "My dark theme")

;; test (in Dropbox)
;; #008080 #2c5115 #3B5998

(custom-theme-set-faces
 'myDark
 '(cursor ((t (:foreground "LightSlateGrey"))))
 '(default ((t (:foreground "gray90"
                :background "#272822"))))
 '(mode-line ((t (:foreground "white"
                  :background "LightSlateGrey"))))
 ;'(mode-line-inactive ((t (:foreground "gray60"
 ;                          :background "gray15"))))
 '(mode-line-inactive ((t (:foreground "white"
                           :background "gray0"))))
 '(region ((t (:background "DodgerBlue4"))))
 '(font-lock-string-face ((t (:foreground "chocolate1"))))
 '(font-lock-keyword-face ((t (:foreground "SteelBlue3"))))
 '(font-lock-constant-face ((t (:foreground "turquoise3"))))
 '(font-lock-type-face ((t (:foreground "VioletRed1"))))
 '(font-lock-comment-face ((t (:foreground "gray60"))))
 '(font-lock-function-name-face ((t (:foreground "YellowGreen"))))
 '(font-lock-builtin-face ((t (:foreground "SeaGreen3"))))
 '(font-lock-warning-face ((t (:foreground "white"
                               :background "#ee2c2c"
                               :bold t))))
 '(highlight ((t (:foreground "white"
                  :background "DodgerBlue4"))))
 '(comint-highlight-prompt ((t (:inherit highlight))))
 '(minibuffer-prompt ((t (:foreground "DeepSkyBlue"))))
 '(fringe ((t (:foreground "white"
               :background "gray10"
               :height 120))))
 ;; '(show-paren-match ((t (:foreground "firebrick1"
 ;;                         :background "#2d3743"
 ;;                         :bold t))))
 '(show-paren-match ((t (:foreground "white"
                         :background "#008080"
                         :bold t))))
 '(hlline-face ((t (:background "black"
                    :underline nil))))
 '(linum ((t (:background "gray10"
              :foreground "gray70"
              :height 120
              :bold nil))))
 '(linum-highlight-face ((t (:background "gray10"
                             :foreground "YellowGreen"
                             :height 120
                             :bold t))))
 '(ace-jump-face-foreground ((t (:background "royal blue"
                                 :foreground "white"))))
 '(elscreen-tab-background-face ((t (:foreground "gray60"
                                     :background "gray10"))))
 '(elscreen-tab-current-screen-face ((t (:inherit default
                                         ;; :background "#272822"
                                         :foreground "YellowGreen"
                                         :bold t))))
 '(elscreen-tab-other-screen-face ((t (:inherit mode-line
                                       :foreground "white"
                                       ;; :background "gray40"
                                       ))))
 '(helm-selection ((t (:foreground "white"
                       :background "forest green"
                       :underline nil))))
 '(mc/cursor-face ((t (:inverse-video nil
                       :background "VioletRed"
                       :foreground "white"))))
 '(anzu-mode-line ((t (:background "white"
                       :foreground "DodgerBlue4"
                       :bold t))))
 '(latex-caption-face ((t (:foreground "LightGray"))))
 '(latex-footnote-face ((t (:foreground "chocolate1"))))
 '(my-latex-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(my-latex-constant-face ((t (:inherit font-lock-constant-face))))
 '(font-latex-math-face ((t (:foreground "burlywood1"))))
 '(font-latex-string-face ((t (:foreground "chocolate1"))))
 '(font-latex-sectioning-0-face ((t (:foreground "VioletRed1"))))
 '(font-latex-sectioning-1-face ((t (:foreground "VioletRed1"))))
 '(font-latex-sectioning-2-face ((t (:foreground "VioletRed1"))))
 '(font-latex-sectioning-3-face ((t (:foreground "VioletRed1"))))
 '(font-latex-sectioning-4-face ((t (:foreground "VioletRed1"))))
 '(font-latex-sectioning-5-face ((t (:foreground "VioletRed1"))))
 '(font-latex-superscript-face ((t (:height 0.9))))
 '(font-latex-subscript-face ((t (:height 0.9))))
 '(font-latex-warning-face ((t (:foreground "firebrick1"))))
 '(evil-ex-substitute-replacement ((t (:foreground "yellow2"))))
 '(evil-ex-info ((t (:foreground "firebrick1"))))
 '(popup-tip-face ((t (:foreground "white"
                       :background "steel blue"))))
 '(rainbow-delimiters-unmatched-face
   ((((background light)) (:foreground "#88090B"))
    (((background dark)) (:foreground "#D7000F" :background "white"))))
 ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "#F39800"))))
 ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "#F3E100"))))
 ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "#86B81B"))))
 ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "#00958D"))))
 ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "#0097DB"))))
 ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "#0062AC"))))
 ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "#8A017C"))))
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "#CF7250"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "#54C3F1"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "#F9C270"))))
 '(rainbow-delimiters-depth-4-face ((((background dark)) (:foreground "#796BAF"))))
 '(rainbow-delimiters-depth-5-face ((((background dark)) (:foreground "#FFF67F"))))
 '(rainbow-delimiters-depth-6-face ((((background dark)) (:foreground "#BA79B1"))))
 '(rainbow-delimiters-depth-7-face ((((background dark)) (:foreground "#69BD83"))))
 '(powerline-active1 ((t
                       (:background "LightSlateGrey" :foreground "white")
                       )))
 '(powerline-inactive1 ((t
                         (:background "grey0" :foreground "white")
                         )))
 '(powerline-active2 ((t
                       (:background "grey40" :foreground "white")
                       )))
 '(powerline-inactive2 ((t
                         (:background "grey40" :foreground "gray80")
                         )))
)

(provide-theme 'myDark)
