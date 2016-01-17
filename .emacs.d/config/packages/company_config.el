(use-package company
  :config
  (global-company-mode +1)
  (use-package company-statistics
    :config
    (company-statistics-mode))
  (diminish 'company-mode "")
  (setq company-minimum-prefix-length 3)
  (setq company-transformers '(company-sort-by-statistics company-sort-by-backend-importance))
  ;; ;; Disable auto start of completion
  ;; (custom-set-variables
  ;;  '(company-idle-delay nil))
  (bind-key "M-SPC" 'company-complete)
  (bind-key "A-SPC" 'company-complete)
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-filter-candidates)
             ("C-h" . backward-delete-char)
             ([tab] . company-complete-selection))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))
  (use-package company-quickhelp
    :config
    (company-quickhelp-mode +1))
  )
