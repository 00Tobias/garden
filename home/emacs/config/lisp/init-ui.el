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

(blink-cursor-mode 0)

(delete-selection-mode 1)

(column-number-mode 1)

(setq use-short-answers t)

(when (string= (system-name) "austrat")
  (pixel-scroll-mode 1)
  (pixel-scroll-precision-mode 1)
  (setq touch-screen-precision-scroll t
        pixel-scroll-precision-interpolate-mice nil))

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

(add-hook 'prog-mode-hook 'hs-minor-mode 1)
(with-eval-after-load 'hideshow
  (setcar (alist-get 'hs-minor-mode minor-mode-alist) ""))
(keymap-global-set "C-c b" 'hs-toggle-hiding)
(keymap-global-set "C-c h" 'hs-hide-all)
(keymap-global-set "C-c H" 'hs-show-all)

(use-package diminish)

(use-package ultra-scroll
  :demand t
  :init (setq scroll-conservatively 3
              scroll-margin 0)
  :config (ultra-scroll-mode 1))

(use-package diff-hl
  :demand t
  :init (setq-default left-fringe-width 5)
  :config (global-diff-hl-mode 1))

(use-package vundo
  :bind ("C-c u" . vundo))

(use-package posframe)

(use-package transient-posframe
  :demand t
  :init (setq transient-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  :config (transient-posframe-mode 1))

(use-package flymake-posframe
  :after posframe
  :hook flymake-mode
  :init (setq flymake-posframe-max-width 80
              flymake-posframe-internal-border-width 1
              flymake-posframe-error-prefix "   "
              flymake-posframe-warning-prefix "   "
              flymake-posframe-note-prefix "   ")
  :custom-face
  (flymake-posframe-face ((t (:foreground
                              ,(modus-themes-get-color-value 'fg-main)
                              :background
                              ,(modus-themes-get-color-value 'bg-dim)))))
  (flymake-posframe-border-face ((t (:background
                                     ,(modus-themes-get-color-value 'bg-mode-line-active)))))
  (flymake-posframe-prefix-error-face ((t (:foreground
                                           ,(modus-themes-get-color-value 'err)
                                           :background
                                           ,(modus-themes-get-color-value 'bg-red-subtle)))))
  (flymake-posframe-prefix-warning-face ((t (:foreground
                                             ,(modus-themes-get-color-value 'warning)
                                             :background
                                             ,(modus-themes-get-color-value 'bg-yellow-subtle)))))
  (flymake-posframe-prefix-note-face ((t (:foreground
                                          ,(modus-themes-get-color-value 'green)
                                          :background
                                          ,(modus-themes-get-color-value 'bg-green-subtle))))))

(use-package eldoc-box
  :diminish eldoc-box-hover-at-point-mode
  :bind ("C-c d" . eldoc-box-help-at-point))

(use-package ligature
  :config
  ;; Ligatures for Iosevka from https://github.com/mickeynp/ligature.el/issues/8#issuecomment-703120788
  (ligature-set-ligatures
   'prog-mode
   '("<--" "<---" "<<-" "<-" "<->" "->" "->>" "-->" "--->"
     "<!--" "-<<" "-<" "-<-" "->-" ">-" ">>-" "<-->" "<--->"
     "<---->" "<==" "<===" "<<=" "<=" "<=>" "=>" "=>>" "==>"
     "===>" "<!---" "=<<" "=<" "=<=" "=>=" ">=" ">>=" "<==>"
     "<===>" "<====>" "<-------" "------->" "<======>" "<~~"
     "<~" "~>" "~~>" "\\/" "/\\" "==" "!=" "/=" "~=" "<>"
     "===" "!==" "=/=" "=!=" ":=" ":-" ":+" "<*" "<*>" "*>"
     "<|" "<|>" "|>" "+:" "-:" "=:" "::" ":::" "<." "<.>"
     ".>" "(*" "*)" ":>" "++" "+++" "|-" "-|"))
  (global-ligature-mode t))

(use-package nerd-icons-dired
  :hook dired-mode)

(use-package nerd-icons-completion
  :demand t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config (nerd-icons-completion-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-ui)
;;; init-ui.el ends here
