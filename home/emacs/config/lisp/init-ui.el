;; Load theme
(load-theme 'modus-operandi t)

;; Set my default font
(add-to-list 'default-frame-alist '(font . "Hack"))

;; Enable line-numbers
(global-display-line-numbers-mode)

;; Vim-like scrolling
(setq scroll-step 1)
(setq scroll-margin 5)

;; Set keybinding for ace-window
(global-set-key (kbd "M-o") 'ace-window)

;; Customize modeline faces for moody
(let ((line (face-attribute 'mode-line :underline)))
  (set-face-attribute 'mode-line          nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :underline  line)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :background "#ffffff"))

;; Enable moody
(setq x-underline-at-descent-line t)
;; (setq moody-mode-line-height 19)pf
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(moody-replace-eldoc-minibuffer-message-function)

;; Add elements to modeline
(column-number-mode)

;; Highlight TODO: faces
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

;;; (popper-mode)

;; Add buffers to popper-mode
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "\\*scratch\\*"
        "\\*xref\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        help-mode
        compilation-mode

        ;; Shell modes
        "^\\*eshell.*\\*$" eshell-mode
        "^\\*shell.*\\*$"  shell-mode
        "^\\*term.*\\*$"   term-mode
        "^\\*vterm.*\\*$"  vterm-mode))

;; Set keybindings for popper-mode
(global-set-key (kbd "C-'") 'popper-toggle-latest)
(global-set-key (kbd "M-'") 'popper-cycle)
(global-set-key (kbd "C-M-'") 'popper-toggle-type)

;; Enable popper-mode
(popper-mode +1)

;; Enable echo-area hints for popper
(popper-echo-mode +1)

(provide 'init-ui)
