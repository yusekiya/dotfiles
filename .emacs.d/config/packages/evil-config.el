;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; options for Evil
;; must be written before (require 'evil)
;; Assign scroll-up C-u
(setq evil-want-C-u-scroll t)
;; cursor style
(setq evil-insert-state-cursor '("VioletRed" (bar . 4)))
(setq evil-normal-state-cursor '("#0088cc" box))
(setq evil-emacs-state-cursor '("VioletRed" (bar . 4)))
(setq evil-cross-lines t
      evil-search-module 'evil-search
      evil-ex-search-vim-style-regexp t)
(setq-default evil-shift-width 4)

;; load evil
(use-package evil
  :config
  (evil-mode 1)
  ;; Setting for insert state
  (setcdr evil-insert-state-map nil)      ; Use emacs key bindings in insert state
  (define-key evil-insert-state-map [escape] 'evil-force-normal-state) ; Go back to normal state with ESC
  ;; Keep evil from changing cursor style in insert state
  (defadvice evil-refresh-cursor (around evil-refresh-cursor-unless-insert-mode activate)
    (if (not (or (evil-insert-state-p) (evil-emacs-state-p))) ad-do-it (my:refresh-cursor)))
  (defun evil-swap-key (map key1 key2)
    "Swap KEY1 and KEY2 in MAP."
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk")
  ;; Set key
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$")) ; remap Y to y$ which copies text to the end-of-line
  (define-key evil-normal-state-map (kbd "gh") (kbd "^"))
  (define-key evil-normal-state-map (kbd "gl") (kbd "$"))
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-visual-state-map (kbd ";") 'evil-ex)
  (define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)
  (define-key evil-visual-state-map (kbd ":") 'evil-repeat-find-char)
  (define-key evil-normal-state-map (kbd "<SPC>v") (kbd "0v$h"))
  (define-key evil-normal-state-map (kbd "<SPC>y") (kbd "0v$hy"))
  (define-key evil-normal-state-map (kbd "<SPC>d") (kbd "0v$hd"))
  (define-key evil-normal-state-map (kbd "<SPC>i") (kbd "gg=G C-o zz"))
  (use-package key-chord
    :config
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))
  )

;; evil-surround mode
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; evil-jumper
(use-package evil-jumper
  :config
  ;; (turn-on-evil-jumper-mode)
  (global-evil-jumper-mode))

;; evil-exchange
(use-package evil-exchange
  :config
  ;; change default key bindings (if you want)
  ;; (setq evil-exchange-key (kbd "cx")) ; doesn't work
  ;; (setq evil-exchange-cancel-key (kbd "cX")) ; doesn't work
  (evil-exchange-install))

;; evil-matchit
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1)
  )

;; evil-args
(use-package evil-args
  :init
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  ;; bind evil-jump-out-args
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))

;; evil-nerd-commenter
(use-package evil-nerd-commenter
  :init
  (with-eval-after-load "evil"
    (define-key evil-normal-state-map " c " 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map " c " 'evilnc-comment-or-uncomment-lines)
    (define-key evil-normal-state-map " cl" 'evilnc-quick-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map " ll" 'evilnc-quick-comment-or-uncomment-to-the-line)
    (define-key evil-normal-state-map " cc" 'evilnc-copy-and-comment-lines)
    (define-key evil-normal-state-map " cp" 'evilnc-comment-or-uncomment-paragraphs)
    (define-key evil-normal-state-map " cr" 'comment-or-uncomment-region)
    (define-key evil-normal-state-map " cv" 'evilnc-toggle-invert-comment-line-by-line)
    (eval-after-load 'evil-nerd-commenter-operator
      '(progn
         (define-key evil-normal-state-map "  " 'evilnc-comment-operator)
         (define-key evil-visual-state-map "  " 'evilnc-comment-operator)))
    ))

;; evil-anzu
(use-package evil-anzu)

;; evil-indent-textobject
(use-package evil-indent-textobject)

;; evil-visualstar
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; evil-numbers
(use-package evil-numbers
  :defer t
  :init
  (define-key evil-normal-state-map "+" #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map "-" #'evil-numbers/dec-at-pt))

(use-package dired
  :defer t
  :config
  (evil-define-key 'normal dired-mode-map (kbd "RET") #'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map (kbd "a") #'dired-find-file)
  (evil-define-key 'normal dired-mode-map (kbd "z") #'uenox-dired-winstart)
  (evil-define-key 'normal dired-mode-map (kbd "j") #'dired-next-line)
  (evil-define-key 'normal dired-mode-map (kbd "k") #'dired-previous-line)
  (evil-define-key 'normal dired-mode-map (kbd "h") #'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") #'ignore)
  (evil-define-key 'normal dired-mode-map (kbd "s") #'dired-rotate-sort)
  (evil-define-key 'normal dired-mode-map (kbd "/") #'dired-mark-files-regexp)
  )

;; wdired with Evil
(use-package wdired
  :defer t
  :config
  (evil-define-key 'normal dired-mode-map (kbd "r") #'wdired-change-to-wdired-mode))

;; expand-region with evil
(use-package expand-region
  :defer t
  :init
  (define-key evil-visual-state-map "v" #'er/expand-region)
  (define-key evil-visual-state-map "\C-v" #'er/contract-region))

;; Define key bind for multiple cursors
(use-package region-bindings-mode
  :defer t
  :config
  (evil-define-key 'visual region-bindings-mode-map (kbd "p")
    #'mc/mark-previous-like-this)
  (evil-define-key 'visual region-bindings-mode-map (kbd "n")
    #'mc/mark-next-like-this)
  (evil-define-key 'visual region-bindings-mode-map (kbd "m")
    #'mc/mark-more-like-this-extended)
  (evil-define-key 'visual region-bindings-mode-map (kbd "s")
    #'mc/skip-to-next-like-this)
  (evil-define-key 'visual region-bindings-mode-map (kbd "S")
    #'mc/skip-to-previous-like-this)
  (evil-define-key 'visual region-bindings-mode-map "\C-ci" 'mc/insert-numbers)
  (evil-define-key 'visual region-bindings-mode-map "\C-co" 'mc/sort-regions)
  (evil-define-key 'visual region-bindings-mode-map "\C-cO" 'mc/reverse-regions)
  (evil-define-key 'visual region-bindings-mode-map "\C-c*" 'mc/mark-all-like-this)
  (evil-define-key 'visual region-bindings-mode-map "\C-cd" 'mc/mark-all-like-this-dwim))

;; Define key bind for direx mode
(with-eval-after-load 'direx
  (evil-define-key 'normal direx:direx-mode-map (kbd "j") 'direx:next-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "k") 'direx:previous-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "J") 'direx:next-sibling-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "K") 'direx:previous-sibling-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "RET") 'direx:maybe-find-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "TAB") 'direx:toggle-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "^") 'direx:up-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "u") 'direx:up-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "o") 'direx:find-item)
  (evil-define-key 'normal direx:direx-mode-map (kbd "E") 'direx:expand-item-recursively)
  (evil-define-key 'normal direx:direx-mode-map (kbd "g") 'direx:refresh-whole-tree))

(with-eval-after-load "helm-gtags"
  (global-unset-key "\C-t")
  (custom-set-variables
   '(helm-gtags-prefix-key "C-t"))
  (evil-define-key 'normal helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
  (evil-define-key 'normal helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (evil-define-key 'normal helm-gtags-mode-map (kbd "C-t r") 'helm-gtags-find-rtag)
  (evil-define-key 'normal helm-gtags-mode-map (kbd "C-t s") 'helm-gtags-find-symbol)
  (evil-define-key 'normal helm-gtags-mode-map (kbd "C-t p") 'helm-gtags-parse-file)
  )

(with-eval-after-load 'undo-tree
  (add-to-list 'evil-normal-state-modes 'undo-tree-visualizer-mode)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-undo)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-redo)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "h") 'undo-tree-visualize-switch-branch-left)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "t") 'undo-tree-visualizer-toggle-timestamps)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "d") 'undo-tree-visualizer-toggle-diff)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "s") 'undo-tree-visualizer-selection-mode)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "q") 'undo-tree-visualizer-quit)
  (evil-define-key 'normal undo-tree-visualizer-mode-map (kbd "<escape>") 'undo-tree-visualizer-abort)
  )
