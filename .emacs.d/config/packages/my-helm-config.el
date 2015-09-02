;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-config
  :init
  (custom-set-variables '(helm-command-prefix-key "C-;"))
  :config
  (bind-keys :map helm-command-map
             ("a" . helm-ag)
             ("o" . helm-occur)
             ("y" . yas-insert-snippet)
             ("m" . helm-all-mark-rings))
)

(use-package helm
  ;; :init
  ;; :bind (;("M-x" . helm-M-x)
  ;;        ("M-y" . helm-show-kill-ring)
  ;;        ("C-x b" . helm-mini)
  ;;        ("M-/" . helm-dabbrev))
  :config
  (setq
   helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-quick-update t ; do not display invisible candidates
   helm-idle-delay 0.01 ; be idle for this many seconds, before updating in delayed sources.
   helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
   helm-split-window-default-side 'other ;; open helm buffer in another window
   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-candidate-number-limit 200 ; limit the number of displayed canidates
   helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
   ;; helm-command
   helm-M-x-requires-pattern 0     ; show all candidates when set to 0
   )
  (bind-keys ("M-x" . helm-M-x)
             ("M-y" . helm-show-kill-ring)
             ("C-x b" . helm-mini)
             ("M-/" . helm-dabbrev))
  (bind-keys :map helm-map
             ("C-o" . nil)
             ("TAB" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action)
             ("C-h" . delete-backward-char))
   )

(use-package helm-files
  :bind ("C-x C-f" . helm-find-files)
  :config
  (setq
   helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   helm-boring-file-regexp-list
   '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
   helm-ff-file-name-history-use-recentf t
   ;; helm-buffers   
   helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                          ; useful in helm-mini that lists buffers
   ;; ido
   ido-use-virtual-buffers t      ; Needed in helm-buffers-list
   )
  (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                            '(picture-mode artist-mode)))
  (bind-keys :map helm-find-files-map
             ("C-h" . delete-backward-char)
             ("C-i" . helm-execute-persistent-action))
   )

(use-package helm-eshell
  :defer t)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

(use-package helm-grep
  :defer t
  :config
  (bind-keys :map helm-grep-mode-map
             ("RET" . helm-grep-mode-jump-other-window)
             ("n" . helm-grep-mode-jump-other-window-forward)
             ("p" . helm-grep-mode-jump-other-window-backward)))

(use-package helm-ag
  :defer t)

(use-package helm-swoop
  :bind
  (("M-o" . helm-swoop)
   ("M-O" . helm-swoop-back-to-last-point)
   ("C-c M-o" . helm-multi-swoop)
   ;; ("C-c M-O" . helm-multi-swoop-all)
   )
  :config
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)
  ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)
  ;; If nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color t)
  (bind-keys :map isearch-mode-map
             ("M-o" . helm-swoop-from-isearch))
  (bind-keys :map helm-swoop-map
             ("M-o" . helm-multi-swoop-all-from-helm-swoop)
             ;; ("M-i" . helm-swoop-from-evil-search)
             )
  )

(use-package helm-gtags
  :defer t
  :init
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'objc-mode-hook 'helm-gtags-mode)
  (add-hook 'python-mode-hook 'helm-gtags-mode)
  :config
  (global-unset-key "\C-t")
  (custom-set-variables
   '(helm-gtags-path-style 'relative)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update t)
   '(helm-gtags-prefix-key "C-t"))
  (bind-keys :map helm-gtags-mode-map
             ("M-." . helm-gtags-find-tag)
             ("M-," . helm-gtags-pop-stack)
             ("C-t r" . helm-gtags-find-rtag)
             ("C-t s" . helm-gtags-find-symbol)
             ("C-t p" . helm-gtags-parse-file)
             ))

;;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; start helm-mode
(use-package helm-mode
  :diminish helm-mode
  :config
  (helm-mode 1))
