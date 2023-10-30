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

(set-face-attribute 'mode-line nil
                    :background "#1e1e1e"
                    :overline "white")
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (make-local-variable 'face-remapping-alist)
;;             (add-to-list 'face-remapping-alist '(default (:background "grey10")))))
;; (set-face-background 'minibuffer-prompt "grey10")
;; (with-current-buffer (get-buffer " *Echo Area 0*")
;;   (setq-local face-remapping-alist '((default (:background "#1e1e1e")))))

(add-to-list 'default-frame-alist '(font . "Sarasa Mono CL-12"))

(blink-cursor-mode 0)

(delete-selection-mode 1)

(column-number-mode 1)

(which-function-mode 1)

(setq scroll-step 1)
(setq scroll-margin 5)

(setq use-short-answers t)

(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-S-z" 'undo-redo)

(font-lock-add-keywords 'prog-mode '(("\\(FIXME\\|TODO\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'prog-mode '(("\\(NOTE\\|DONE\\):" 1 font-lock-string-face t)))

(setq flymake-mode-line-lighter "")

;;; package: diff-hl
(setq-default left-fringe-width 5)
(global-diff-hl-mode)

;;; package: vundo
(keymap-global-set "C-c u" 'vundo)

(provide 'init-ui)
;;; init-ui.el ends here
