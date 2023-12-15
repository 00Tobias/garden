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

(which-function-mode 1)

(setq use-short-answers t)

(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-S-z" 'undo-redo)

(defun highlight-todo ()
  "Highlight FIXME:, TODO: and NOTE:, case insensitively."
  (font-lock-add-keywords
   nil '(("\\<\\([fF][iI][xX][mM][eE]\\):"  1 'error prepend)
         ("\\<\\([tT][oO][dD][oO]\\):"      1 'warning prepend)
         ("\\<\\([nN][oO][tT][eE]\\):"      1 'font-lock-string-face prepend))))
(add-hook 'prog-mode-hook 'highlight-todo)

(setq flymake-mode-line-lighter "")

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

(provide 'init-ui)
;;; init-ui.el ends here
