;; automatically generate natively compiled files
(setq comp-deferred-compilation t)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Start notification daemon
;; (ednc-mode)

;; Lastly, load files in ./lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-keybindings)
(require 'init-completion)
(require 'init-langs)
(require 'init-org)
