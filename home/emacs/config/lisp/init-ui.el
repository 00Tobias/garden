;;; init-ui.el --- Configurations for mode enhancements and UI  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Set theme
(setq modus-themes-mode-line '(borderless)
      modus-themes-paren-match '(bold intense))
(load-theme 'modus-vivendi t)

;; Set font
(add-to-list 'default-frame-alist '(font . "Hack-10"))

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Delete selection when typing
(delete-selection-mode 1)

;; Enable line-numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Show column number on the modeline
(column-number-mode 1)

;; Vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 5)

;; If Emacs is version 29 or above, enable smooth scrolling
(if (>= emacs-major-version 29)
    (pixel-scroll-precision-mode))

;; way more useful
(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-S-z" 'undo-redo)

;; dired TODO: move this
(setq dired-dwim-target t)

;; paren-face
(global-paren-face-mode 't)

;; ace-window
(keymap-global-set "C-;" 'ace-window)
(setq aw-dispatch-always t)

;; hl-todo
(add-hook 'prog-mode-hook 'hl-todo-mode)
(setq hl-todo-highlight-punctuation ":"
    hl-todo-keyword-faces
    `(("TODO" warning bold)
      ("FIXME" error bold)
      ("HACK" font-lock-constant-face bold)
      ("REVIEW" font-lock-keyword-face bold)
      ("NOTE" success bold)
      ("DEPRECATED" font-lock-doc-face bold)
      ("BUG" error bold)
      ("XXX" font-lock-constant-face bold)))

;;diff-hl
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(setq-default left-fringe-width 5)
(global-diff-hl-mode)

;; which-key
(which-key-mode)
(diminish 'which-key-mode)

;; dirvish
;; (dirvish-override-dired-mode)

;; ;; frames-only-mode
;; (frames-only-mode 1)

;; ;; mini-frame
;; (setq mini-frame-standalone 't
;;       mini-frame-resize-min-height 10
;;       mini-frame-detach-on-hide nil)
;; (custom-set-variables
;;  '(mini-frame-show-parameters
;;    '((width . 0.5))))
;; (mini-frame-mode 1)

(provide 'init-ui)
;;; init-ui.el ends here
