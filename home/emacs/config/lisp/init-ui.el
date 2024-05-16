;;; init-ui.el --- Configurations for mode enhancements and UI  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq modus-themes-italic-constructs t
      modus-themes-common-palette-overrides
      '((fg-region unspecified)
        (fringe unspecified)
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))
(load-theme 'modus-vivendi t)

(add-to-list 'default-frame-alist '(font . "Sarasa Mono SC-10"))

(blink-cursor-mode 0)

(delete-selection-mode 1)

(column-number-mode 1)

(setq use-short-answers t)

(when (string= (system-name) "austrat")
  (pixel-scroll-mode 1)
  (pixel-scroll-precision-mode 1))

(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-S-z" 'undo-redo)

(setq case-replace nil)
(keymap-global-set "C-c r" 'replace-regexp)
(keymap-global-set "C-c q" 'query-replace-regexp)

(keymap-global-set "C-c s" 'save-buffer)

(defun highlight-todo ()
  "Highlight FIXME:, TODO: and NOTE:, case insensitively."
  (font-lock-add-keywords
   nil '(("\\<\\([fF][iI][xX][mM][eE]\\):"  1 'error prepend)
         ("\\<\\([tT][oO][dD][oO]\\):"      1 'warning prepend)
         ("\\<\\([nN][oO][tT][eE]\\):"      1 'font-lock-string-face prepend))))
(add-hook 'prog-mode-hook 'highlight-todo)

(setq flymake-mode-line-lighter "")

(add-hook 'prog-mode-hook 'hs-minor-mode 1)
(keymap-global-set "C-c b" 'hs-toggle-hiding)
(keymap-global-set "C-c h" 'hs-hide-all)
(keymap-global-set "C-c H" 'hs-show-all)

;;; package: diff-hl
(setq-default left-fringe-width 5)
(global-diff-hl-mode)

;;; package: vundo
(keymap-global-set "C-c u" 'vundo)

;;; package: transient-posframe
(setq transient-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
(transient-posframe-mode)

;;; package: flymake-popon
(require 'flymake-popon)
(with-eval-after-load 'flymake-popon (setcar (alist-get 'flymake-popon-mode minor-mode-alist) ""))
(setq flymake-popon-delay 0.5)
(flymake-popon-mode 1)

;;; package: eldoc-box
(require 'eldoc-box)
(with-eval-after-load 'eldoc-box (setcar (alist-get 'eldoc-box-hover-at-point-mode minor-mode-alist) ""))
(eldoc-box-hover-at-point-mode 1)

;;; package: nerd-icons
(setq nerd-icons-font-family "Iosevka Nerd Font Mono")

;;; package: nerd-icons-dired
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;;; package: nerd-icons-completion
(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

;;; package: nerd-icons-corfu
(with-eval-after-load 'corfu
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-ui)
;;; init-ui.el ends here
