;;; init-prog.el --- Configuration for programming modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq standard-indent 2)

(electric-pair-mode 1)

;; Let some trusted directories set risky local variables
(setq safe-local-variable-directories '("~/garden/" "~/git/guix"))
(dolist (subdir (seq-filter 'file-directory-p (directory-files (expand-file-name "~/projects/") t "\\`[^.]")))
  (add-to-list 'safe-local-variable-directories subdir))

;;; treesit
(setq treesit-extra-load-path '("~/.guix-home/profile/lib/tree-sitter"))
(dolist (mapping '((bash-mode . bash-ts-mode)
                   (c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   (css-mode . css-ts-mode)
                   (html-mode . html-ts-mode)
                   (js-mode . js-ts-mode)
                   (json-mode . json-ts-mode)
                   (python-mode . python-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(add-to-list 'auto-mode-alist '("\\.go$"  . go-ts-mode))
(add-to-list 'auto-mode-alist '("/go\\.mod\\'"  . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js$"  . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs$"  . rust-ts-mode))

;;; eglot
(require 'eglot)
(add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp")))
(setq eglot-events-buffer-size 0)

(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'clojure-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'haskell-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)

(use-package combobulate
  :hook (bash-ts-mode
         c-ts-mode
         c++-ts-mode
         css-ts-mode
         go-ts-mode
         html-ts-mode
         js-ts-mode
         json-ts-mode
         python-ts-mode
         typescript-ts-mode
         tsx-ts-mode
         ruby-ts-mode
         yaml-ts-mode))

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :hook ((prog-mode text-mode) . (lambda ()
                                   (setq-local
                                    completion-at-point-functions
                                    (cons #'tempel-expand completion-at-point-functions))))
  :config
  (unless (listp 'tempel-path)
    (setq tempel-path (list tempel-path)))
  (add-to-list 'tempel-path "~/git/guix/etc/snippets/tempel/*"))

(use-package aggressive-indent
  :demand t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'shell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package avy
  :demand t
  :bind (("C-c C-j" . avy-resume)
         ("C-r"     . avy-goto-char)
         ("C-."     . avy-goto-char)
         ("C-:"     . avy-goto-char-2)
         ("M-g g"   . avy-goto-line)
         ("M-g w"   . avy-goto-word-1)
         ("M-g e"   . avy-goto-word-0)))

(use-package expand-region
  :demand t
  :bind ("C-=" . er/expand-region))

(use-package paredit
  :after eldoc
  :diminish paredit-mode
  :hook ((paredit-mode . (lambda ()
                           (electric-indent-local-mode 0)
                           (electric-pair-local-mode 0)))
         ((emacs-lisp-mode
           eval-expression-minibuffer-setup
           ielm-mode
           lisp-mode
           lisp-interaction-mode
           scheme-mode
           clojure-mode
           fennel-mode)
          . paredit-mode))
  :config (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(use-package devdocs
  :bind ("C-c D" . devdocs-lookup))

;;; Language major modes

;;; Clojure

(use-package cider
  :init (setq cider-repl-display-help-banner nil
              cider-use-overlays t
              cider-use-tooltips nil
              cider-font-lock-reader-conditionals nil))

;;; Common lisp

(put 'define-configuration 'lisp-indent-function 'defun)

(setq inferior-lisp-program "sbcl")
(with-eval-after-load 'browse-url (add-to-list 'browse-url-handlers '("hyperspec" . eww-browse-url)))

(use-package sly
  :config
  (defun sly-eval-sexp-overlay ()
    (interactive)
    (let ((result (->> `(slynk:pprint-eval ,(sly-sexp-at-point))
                       sly-eval
                       s-trim
                       (s-replace "\n" ", "))))
      (eros--make-result-overlay result
        :where (point)
        :duration eros-eval-result-duration))))

(use-package sly-asdf)

(use-package clhs
  :if (file-exists-p "/gnu")
  :demand t
  :config (clhs-setup))

;;; Emacs lisp

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

(use-package eros
  :demand t
  :config (eros-mode 1))

;;; Haskell

(use-package haskell-mode
  :mode "\\.hs\\'")

;;; Scheme

(use-package geiser
  :init (setq geiser-default-implementation 'guile
              geiser-active-implementations '(guile)
              geiser-implementations-alist '(((regexp "\\.scm$") guile))
              geiser-mode-auto-p t
              geiser-repl-per-project-p t
              geiser-repl-query-on-kill-p nil))

(use-package geiser-guile
  :if (file-exists-p "/gnu")
  :after geiser
  :config (add-to-list 'geiser-guile-load-path "~/git/guix"))

;; Broken package: https://issues.guix.gnu.org/55013
(use-package guix
  :if (file-exists-p "/gnu")
  :init (setq guix-guile-program '("guix" "repl")))

;;; Fennel

(use-package fennel-mode
  :mode "\\.fnl\\'"
  :interpreter "fennel")

;;; Web

(use-package web-mode
  :mode "\\.html\\'"
  :init (setq web-mode-part-padding 2
              web-mode-code-indent-offset 4
              web-mode-css-indent-offset 4
              web-mode-indent-style 4))

(provide 'init-prog)
;;; init-prog.el ends here
