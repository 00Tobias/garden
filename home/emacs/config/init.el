;;; init.el --- The root of my Emacs config  -*- lexical-binding: t; -*-

;;; Commentary:
;; GNU's not Unix Editor Macros List Processing configuration files

;;; Code:

;; Use melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; automatically generate natively compiled files
(setq comp-deferred-compilation t)

;; AOT native compile packages
(setq package-native-compile t)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.cache/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
      backup-directory-alist '(("." . "~/.cache/emacs/backup/"))
      savehist-file "~/.cache/emacs/savehist")

;; Do not move the current file while creating backup
(setq backup-by-copying t)

;; Disable lockfiles
(setq create-lockfiles nil)

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Lastly, load files in ./lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-completion)
(require 'init-prog)
(require 'init-text)
(require 'init-eshell)
(require 'init-colemak)

(provide 'init)
;;; init.el ends here
