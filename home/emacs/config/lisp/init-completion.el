;;; init-completion.el --- Configuration for completion and minibuffer  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(savehist-mode)
(setq savehist-save-minibuffer-history t
      savehist-autosave-interval nil
      savehist-additional-variables '(register-alist
                                      kill-ring
                                      mark-ring global-mark-ring
                                      search-ring regexp-search-ring))

;; Use TAB to autocomplete
(setq tab-always-indent 'complete)

;; Use > Emacs 27.1 flex completion style
;; (setq completion-styles '(flex)
;;       completion-cycle-threshold 3)

;; orderless
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(setq suggest-key-bindings t
      completions-detailed t)

;; (require 'ido)
;; (ido-mode 1)
;; (setq ido-enable-flex-matching t
;;       ido-everywhere t
;;       ido-use-filename-at-point 'guess)
;; (fido-mode 1)

;; vertico
(vertico-mode)

;; marginalia
(marginalia-mode)

;; consult
(keymap-global-set "C-s"     'consult-line)
(keymap-global-set "C-x C-r" 'consult-recent-file)
(keymap-global-set "C-c \\"  'consult-register)
(keymap-global-set "C-c -"   'consult-register-load)
(keymap-global-set "C-c ="   'consult-register-store)
(keymap-global-set "C-x b"   'consult-buffer)
(keymap-global-set "C-x p b" 'consult-project-buffer)
(keymap-global-set "M-g o"   'consult-outline)

;; embark
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "M-." 'embark-dwim)

;; embark-consult
;; (add-hook 'embark-collect-mode 'consult-preview-at-point-mode)

;; corfu
(global-corfu-mode)

;; kind-icon
(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(provide 'init-completion)
;;; init-completion.el ends here
