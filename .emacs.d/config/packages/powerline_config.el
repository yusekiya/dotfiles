;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------------------------------------------------
;; face: | 7,3,5,4 > 1 > 6 > 1 > 2 < 1 < 5 |
;;--------------------------------------------------------------------------------
(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions))
  (require 'cl))
(require 'cl-lib)
(require 'powerline)

(defun my:powerline-get-good-height ()
  (let ((original-height (frame-char-height)))
    (if (cl-evenp original-height)
        ;; if height of character is even
        original-height
      ;; if height of character is odd
      (+ original-height 1)
      )))

(defun get-buffer-file-eol-type ()
  (case (coding-system-eol-type buffer-file-coding-system)
    (0 "LF")
    (1 "CRLF")
    (2 "CR")
    (otherwise "??")))

(defun get-buffer-coding-type-without-eol-type ()
  (cl-labels
      ((remove-os-info (string)
                       (replace-regexp-in-string "-\\(dos\\|unix\\|mac\\)$" "" string)))
    (lexical-let
        ((string
          (replace-regexp-in-string "-with-signature" "(bom)"
                                    (remove-os-info  (symbol-name buffer-file-coding-system)))))
      (if (string-match-p "(bom)" string)
          (downcase string)
        (upcase string)))))

(defface powerline-active3
  '((t (:background "Springgreen4"
        :foreground "white")))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-inactive3
  '((t (:background "grey0"
        :foreground "gray60")))
  "Powerline face 3."
  :group 'powerline)

(defface powerline-active4
  '((t (:background "VioletRed"
        :foreground "white")))
  "Powerline face 4."
  :group 'powerline)

(defface powerline-inactive4
  '((t (:background "grey0"
        :foreground "gray60")))
  "Powerline face 4."
  :group 'powerline)

(defface powerline-active5
  '((t (:background "#0088cc"
        :foreground "white")))
  "Powerline face 5."
  :group 'powerline)

(defface powerline-inactive5
  '((t (:background "grey0"
        :foreground "gray60")))
  "Powerline face 5."
  :group 'powerline)

(defface powerline-active7
  '((t (:background "Yellow4"
        :foreground "white"
       )))
  "Powerline face 7."
  :group 'powerline)

(defface powerline-inactive7
  '((t (:background "grey0"
        :foreground "gray60")))
  "Powerline face 7."
  :group 'powerline)

(defface powerline-zoom-window
  '((t (:background "DarkGreen"
        :foreground "white")))
  "Powerline face for zoom-window"
  :group 'powerline)

(defpowerline powerline-zoom-window-mode
  "Zoom ")

(defpowerline powerline-ime-mode
  (cond
   ((and (featurep 'smartrep) (< 0 (length smartrep-mode-line-string))) "<SR>")
   ((my:get-ime) "[あ] ")
   (view-mode "View ")
   ((evil-normal-state-p) "Normal ")
   ((evil-operator-state-p) "Operator ")
   ((evil-visual-state-p) "Visual ")
   ((evil-insert-state-p) "[Aa] ")
   ((evil-emacs-state-p) "Emacs ")
   (t "Unknown ")))

(defpowerline powerline-coding-type
  (concat (get-buffer-coding-type-without-eol-type) "[" (get-buffer-file-eol-type) "]"))

(defpowerline powerline-buffer-status
  (concat (if buffer-read-only "r-" "rw")
          ":"
          (if (buffer-modified-p) "*" "-")))

(use-package git-ps1-mode
  :diminish git-ps1-mode
  :config
  (setq git-ps1-mode-lighter-text-format "[%s]"))

(defpowerline powerline-git-status
  (if (featurep 'git-ps1-mode) git-ps1-mode-lighter-text ""))

(custom-set-variables '(powerline-height (+ 2 (my:powerline-get-good-height))))

(defadvice load-theme (after my:ad-disable-box-mode-line activate)
  (set-face-attribute 'mode-line nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil))
 
;;;
;;; `powerline-angle-left' and `powerline-angle-right'
;;; cf. https://gist.github.com/kenoss/b2d2daf38a956644fb04
(eval-when-compile
  (defun pl/wrap-defun* (name dir width let-vars body &optional face-i)
    "A generalization of `pl/wrap-defun'. If FACE-I is non-nil, it is used instead of interpolated color."
    (let* ((src-face (if (eq dir 'left) 'face1 'face2))
           (dst-face (if (eq dir 'left) 'face2 'face1)))
      `(defun ,(intern (format "powerline-%s-%s" name (symbol-name dir)))
         (face1 &optional face2 height)
         (setq face2 (or face2 face1))
         (when window-system
           (unless height
             (setq height (pl/separator-height)))
           (let* ,(append `((color1 (when ,src-face
                                      (pl/hex-color (face-attribute ,src-face :background))))
                            (color2 (when ,dst-face
                                      (pl/hex-color (face-attribute ,dst-face :background))))
                            (colori (if ',face-i
                                        (pl/hex-color (face-attribute ',face-i :background))
                                      (when (and color1 color2) (pl/interpolate color1 color2))))
                            (color1 (or color1 "None"))
                            (color2 (or color2 "None"))
                            (colori (or colori "None")))
                          let-vars)
             (create-image ,(append `(concat (format "/* XPM */ static char * %s_%s[] = { \"%s %s 3 1\", \"0 c %s\", \"1 c %s\", \"2 c %s\","
                                                     ,(replace-regexp-in-string "-" "_" name)
                                                     (symbol-name ',dir)
                                                     ,width
                                                     height
                                                     color1
                                                     color2
                                                     colori))
                                    body
                                    '("};"))
                           'xpm t
                           :ascent 'center
                           :face (when (and face1 face2)
                                   ,dst-face)))))))
  )
 
(defmacro pl/angle (dir)
  "Generate an angle XPM function for DIR."
  (let ((row-modifier (if (eq dir 'left) 'identity 'reverse)))
    (pl/wrap-defun* "angle" dir 'middle-width
                    '((border-width 10)
                      (width (1- (/ height 2)))
                      (middle-width (+ border-width (1- (ceiling height 2)))))
                    `((cl-loop for i from 0 to width
                               concat (pl/pattern-to-string
                                       (,row-modifier (pl/row-pattern i middle-width border-width))))
                      ;; (when (cl-oddp height)
                      ;;   (pl/pattern-to-string (,row-modifier (append (make-list middle-width 0)
                      ;;                                                (make-list border-width 2)))))
                      (when (cl-oddp height)
                        (pl/pattern-to-string (,row-modifier (append (make-list (- middle-width border-width) 0)
                                                                     (make-list border-width 2)))))
                      (cl-loop for i from width downto 0
                               concat (pl/pattern-to-string
                                       (,row-modifier (pl/row-pattern i middle-width border-width)))))
                    'default)))
 
(pl/memoize (pl/angle left))
(pl/memoize (pl/angle right))
(pl/reset-cache)


(defun powerline-my-theme ()
  "Setup a mode-line with major and minor modes centered. The separator shape is angle."
  (interactive)
  (custom-set-variables '(powerline-default-separator 'angle))
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 (if active 'powerline-active3 'powerline-inactive3))
                          (face4 (if active 'powerline-active4 'powerline-inactive4))
                          (face5 (if active 'powerline-active5 'powerline-inactive5))
                          (face6 (if active 'mode-line 'mode-line-inactive))
                          (face7 (if active 'powerline-active7 'powerline-inactive7))
                          (face8 (if active 'powerline-zoom-window 'mode-line-inactive))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          ;; powerline-default-separator
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           ;; powerline-default-separator
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list
                                ;; ;; state indicator
                                (cond
                                 ((and (featurep 'smartrep) (< 0 (length smartrep-mode-line-string)))
                                  (powerline-ime-mode face7 'l))
                                 ((my:get-ime)
                                  (powerline-ime-mode face3 'l))
                                 (view-mode (powerline-ime-mode face5 'l))
                                 ((evil-normal-state-p) (powerline-ime-mode face5 'l))
                                 ((evil-operator-state-p) (powerline-ime-mode face5 'l))
                                 ((evil-visual-state-p) (powerline-ime-mode face5 'l))
                                 (t (powerline-ime-mode face4 'l)))
                                ;; separator
                                (if (not (and (featurep 'zoom-window) (zoom-window--enable-p)))
                                    ;; when zoom-window is not enabled or zoom-window is not installed
                                    (cond
                                      ((and (featurep 'smartrep) (< 0 (length smartrep-mode-line-string)))
                                        (funcall separator-left face7 face1))
                                      ((my:get-ime)
                                        (funcall separator-left face3 face1))
                                      (view-mode (funcall separator-left face5 face1))
                                      ((evil-normal-state-p) (funcall separator-left face5 face1))
                                      ((evil-operator-state-p) (funcall separator-left face5 face1))
                                      ((evil-visual-state-p) (funcall separator-left face5 face1))
                                      (t (funcall separator-left face4 face1)))
                                    ;; when zoom-window is enabled
                                      (cond
                                        ((and (featurep 'smartrep) (< 0 (length smartrep-mode-line-string)))
                                            (funcall separator-left face7 face8))
                                        ((my:get-ime)
                                         (funcall separator-left face3 face8))
                                        (view-mode (funcall separator-left face5 face8))
                                        ((evil-normal-state-p) (funcall separator-left face5 face8))
                                        ((evil-operator-state-p) (funcall separator-left face5 face8))
                                        ((evil-visual-state-p) (funcall separator-left face5 face8))
                                        (t (funcall separator-left face4 face8)))
                                    )
                                        (when (and (featurep 'zoom-window) (zoom-window--enable-p))
                                          (powerline-zoom-window-mode face8 'l))
                                        (when (and (featurep 'zoom-window) (zoom-window--enable-p))
                                          (funcall separator-left face8 face1))
                                ;; ;; coding type and buffer status
                                (powerline-coding-type face1 'l)
                                (powerline-buffer-status face1 'l)
                                ;; (powerline-buffer-size nil 'l)
                                ;; ;; separator
                                (funcall separator-left face1 face1)
                                ;; ;; buffer id
                                (powerline-buffer-id face1 'l)
                                (powerline-git-status face1 'l)
                                ;; ;; separator
                                (funcall separator-left face1 face1)
                                ;; ;; line number
                                (powerline-raw "%4l" face1 'l)
                                (powerline-raw " :" face1)
                                (powerline-raw "%2c" face1 'l)
                                ;; (powerline-narrow face1 'l)
                                (powerline-raw "(%3p)" face1 'l)
                                ))
                          (rhs (list
                                (when which-func-mode
                                  (powerline-raw which-func-current face1 'r))
                                ;; (powerline-raw global-mode-string face1 'r)
                                (funcall separator-right face1 face1)
                                ;; ;; major mode
                                (powerline-major-mode face1 'r)
                                ;; (powerline-raw " ")
                                ))
                          (center (list
                                   (powerline-raw " " face1)
                                   (funcall separator-left face1 face2)
                                   (when (boundp 'erc-modified-channels-object)
                                     (powerline-raw erc-modified-channels-object face2 'l))
                                   (powerline-process face2)
                                   ;; (powerline-raw " :" face2)
                                   (powerline-minor-modes face2 'l)
                                   (powerline-raw " " face2)
                                   (funcall separator-right face2 face1)))
                          )
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs))
                     )))))

;; set theme
(powerline-my-theme)

;; テーマを変更したときにモードラインの表示が崩れる問題を修正
;; 同じ関数の呼び出しを高速化する memorize (キャッシュを作成する関数) が原因のよう
;; powerline-reset を実行しキャッシュを削除
;; modify mode line color when theme is changed
;; (advice-add 'disable-theme :after 'powerline-reset)
;; (advice-add 'load-theme :after 'powerline-reset)
(defadvice disable-theme (after my:advice-refresh-mode-line-after-disable-theme activate)
  (powerline-reset))
(defadvice load-theme (after my:advice-refresh-mode-line-after-load-theme activate)
  (powerline-reset))

;; pos-tip が表示されるとモードラインがinactiveになったまま戻らない問題を修正
(advice-add 'pos-tip-hide :after 'powerline-set-selected-window)
(advice-add 'x-hide-tip :after 'powerline-set-selected-window)
