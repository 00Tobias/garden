;; Enable vertico
(vertico-mode)

;; richer annotations with marginalia
(marginalia-mode)

;; Use corfu for a small in-buffer completion popup
(setq corfu-auto t)
(corfu-global-mode)

;; Kind-icon for icons in corfu
(require 'kind-icon) ;; :/
(setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;; Use orderless as my completion style
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

;; Use TAB to autocomplete
(setq tab-always-indent 'complete)

(provide 'init-completion)
