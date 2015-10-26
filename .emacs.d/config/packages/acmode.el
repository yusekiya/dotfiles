;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-complete
  :config
  (diminish 'auto-complete-mode "")
  (global-auto-complete-mode 1)
  (bind-key "M-SPC" 'auto-complete)
  (add-to-list 'load-path (concat user-emacs-directory "lib/acmode/lib/fuzzy"))
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "lib/acmode/dict"))
  (ac-set-trigger-key "TAB")
  (setq ac-dwim t) ; DWIM (Do What I Mean)
  (setq ac-menu-height 15)
  ;; Start auto-complete without trigger key
  ;; (setq ac-auto-start 3) ; the number of characters above which auto complete starts
  ;; (setq ac-delay 0.3) ; time delay
  ;; Start auto-complete with trigger key
  (setq ac-auto-start nil)
  (setq ac-use-fuzzy t) ; enable fuzzy search
  (setq ac-use-menu-map t)
  ;; Font
  ;; (set-face-font 'ac-candidate-face "MS Gothic 11")
  ;; (set-face-font 'ac-selection-face "MS Gothic 11")
  (setq popup-use-optimized-column-computation nil)
  (use-package auto-complete-config
    :config (ac-config-default))  
  (setq-default ac-sources '(ac-source-filename ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (bind-keys :map ac-completing-map
             ;; ("M-/" . ac-stop)
             )
  (bind-keys :map ac-menu-map
             ("C-n" . ac-next)
             ("C-p" . ac-previous)
             ("<tab>" . ac-complete)
             ("C-SPC" . ac-expand)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete-c-headers 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:ac-c-header-init ()
  (use-package auto-complete-c-headers
    :config
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (setq achead:include-directories (append (my:get-include-dirs) achead:include-directories))))

(add-hook 'c-mode-hook 'my:ac-c-header-init)
(add-hook 'c++-mode-hook 'my:ac-c-header-init)


;; end of file
