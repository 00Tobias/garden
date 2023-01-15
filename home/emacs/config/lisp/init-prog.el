9;;; init-prog.el --- Configuration for programming modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Highlight trailing whitespace
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Use spaces for indentation.
(setq-default indent-tabs-mode nil)

;; Set width of indent
(setq-default tab-width 4)
(setq standard-indent 4)

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(electric-pair-mode 1)

;; lispy
(add-hook 'clojure-mode-hook    'lispy-mode)
(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'fennel-mode-hook     'lispy-mode)
(add-hook 'hy-mode-hook         'lispy-mode)
(add-hook 'lisp-mode-hook       'lispy-mode)
(add-hook 'racket-mode-hook     'lispy-mode)
(add-hook 'scheme-mode-hook     'lispy-mode)

;; yasnippet
(yas-global-mode 1)
(diminish 'yas-minor-mode)

;; aggressive-indent
;; (global-aggressive-indent-mode 1)

;; expand-region
(keymap-global-set "C-:" 'er/expand-region)

;; avy
;; Using 'consult-line' for my search interface, C-r is freed
(keymap-global-set "C-r" 'avy-goto-char)

;;; Langs

;; Clojure

;; Common lisp
(setq inferior-lisp-program "sbcl")

(provide 'init-prog)
;;; init-prog.el ends here
