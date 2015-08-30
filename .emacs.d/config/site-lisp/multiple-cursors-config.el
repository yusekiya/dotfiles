;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :bind (("<C-M-return>" . mc/edit-lines)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init
  (use-package region-bindings-mode
    :config
    (bind-keys :map region-bindings-mode-map
               ("e" . mc/edit-lines)
               ("p" . mc/mark-previous-like-this)
               ("n" . mc/mark-next-like-this)
               ("m" . mc/mark-more-like-this-extended)
               ("u" . mc/unmark-next-like-this)
               ("U" . mc/unmark-previous-like-this)
               ("s" . mc/skip-to-next-like-this)
               ("S" . mc/skip-to-previous-like-this)
               ("\C-ci" . mc/insert-numbers)
               ("\C-co" . mc/sort-regions)
               ("\C-cO" . mc/reverse-regions)
               ("\C-c*" . mc/mark-all-like-this)
               ("\C-cd" . mc/mark-all-like-this-dwim)))
  ;; Path to .mc-lists
  (setq mc/list-file (concat user-emacs-directory ".mc-lists.el"))
  )

;; ;; set key
;; (global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
;; ;; using region bidings mode
;; (define-key region-bindings-mode-map "e" 'mc/edit-lines)
;; (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
;; (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
;; (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)
;; (define-key region-bindings-mode-map "u" 'mc/unmark-next-like-this)
;; (define-key region-bindings-mode-map "U" 'mc/unmark-previous-like-this)
;; (define-key region-bindings-mode-map "s" 'mc/skip-to-next-like-this)
;; (define-key region-bindings-mode-map "S" 'mc/skip-to-previous-like-this)
;; (define-key region-bindings-mode-map "\C-ci" 'mc/insert-numbers)
;; (define-key region-bindings-mode-map "\C-co" 'mc/sort-regions)
;; (define-key region-bindings-mode-map "\C-cO" 'mc/reverse-regions)
;; (define-key region-bindings-mode-map "\C-c*" 'mc/mark-all-like-this)
;; (define-key region-bindings-mode-map "\C-cd" 'mc/mark-all-like-this-dwim)

;; (global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
