;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :init
  ;; Load user defined snippets
  (setq yas-snippet-dirs (list (concat user-emacs-directory "lib/snippets")))
  :config
  (diminish 'yas-minor-mode "")
  ;; ;; Don't expand snippets after symbol constituents like underscore (_)
  ;; (setq yas-key-syntaxes (delete "w" yas-key-syntaxes))
  ;; ;; Don't expand snippets after dot
  ;; (setq yas-key-syntaxes (delete "w_" yas-key-syntaxes))

  ;; Load community-developed snippets
  (use-package yasnippet-snippets)
  (yas-global-mode 1)
  (custom-set-variables '(yas-trigger-key "TAB"))
  (setq yas-triggers-in-field t) ; for nested forms
  (bind-keys :map yas-minor-mode-map
             ("C-x i i" . yas-insert-snippet)
             ("C-x i n" . yas-new-snippet)
             ("C-x i v" . yas-visit-snippet-file)
             )
  ;; Use popup menu for yas-choose-value
  (use-package popup)
  (setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))
  ;; Alias
  (defalias 'describe-snippets 'yas-describe-tables)
  (defalias 'show-snippets 'yas-describe-tables)
  (defalias 'list-snippets 'yas-describe-tables))

;; Use popup menu for yas-choose-value
;; (require 'popup)
(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))
