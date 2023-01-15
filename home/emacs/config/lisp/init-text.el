;;; init-text.el --- Configuration for text modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Consider a period followed by a single space to be end of sentence
(setq sentence-end-double-space nil)

;; Enable Flyspell in text modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Command to toggle Flyspell
(keymap-global-set "C-c f" 'flyspell-toggle)

;; Automatically wrap lines at 80 columns in text modes
(add-hook 'text-mode-hook (lambda nil
                            (auto-fill-mode 1)
                            (set-fill-column 80)))

(provide 'init-text)
;;; init-text.el ends here
