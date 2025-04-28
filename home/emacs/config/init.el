;;; init.el --- The root of my Emacs config  -*- lexical-binding: t; -*-

;;; Commentary:
;; GNU's not Unix Editor Macros List Processing configuration files

;;; Code:

;; Functions for settings keys have changed in Emacs 29, this creates dummy functions for Emacs versions below 29.
(when (version< emacs-version "29")
  (defun keymap-set (keymap key def)
    (define-key keymap (kbd key) def))
  (defun keymap-global-set (key def)
    (global-set-key (kbd key) def)))

(defun keymap-global-set-keys (&rest lst)
  "Define multiple global keymap keys using one function call."
  (declare (indent 1))
  (while lst
    (let ((key (pop lst))
          (cmd (pop lst)))
      (keymap-global-set key cmd))))

(defun keymap-set-keys (&rest lst)
  "Define multiple local keymap keys using one function call."
  (declare (indent 1))
  (let ((keymap (pop lst)))
    (while lst
      (let ((key (pop lst))
            (cmd (pop lst)))
        (keymap-set keymap key cmd)))))

(defun gsr ()
  "Reconfigure system profile"
  (interactive)
  (compile "sudo guix system reconfigure -c $(nproc) -L ~/garden ~/garden/system/hosts/(hostname).scm"))

(defun ghr ()
  "Reconfigure home profile"
  (interactive)
  (compile "guix home reconfigure -c $(nproc) -L ~/garden ~/garden/home/main.scm"))

(setq comp-deferred-compilation t)
(setq comp-async-report-warnings-errors nil)

(setq package-native-compile t)

(unless (file-exists-p "/gnu")
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(if (daemonp)
    (setq use-package-always-demand t)
  (setq use-package-always-defer t))

;; Write auto-saves and backups to separate directory.
(make-directory "~/.cache/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
      backup-directory-alist '(("." . "~/.cache/emacs/backup/"))
      savehist-file "~/.cache/emacs/savehist")

(setq project-list-file "~/.emacs.d/auto-save-list/projects"
      project-vc-extra-root-markers '(".project"))

(setq disabled-command-function nil)

(setq backup-by-copying t)

(setq create-lockfiles nil)

(global-auto-revert-mode t)

(setq require-final-newline t)

(setq large-file-warning-threshold nil)

(setq find-function-C-source-directory "~/git/emacs/src")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(global-subword-mode 1)

;; Lastly, load files in ./lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-ui)
(require 'init-completion)
(require 'init-prog)
(require 'init-text)
(require 'init-modes)
(require 'init-modal)
(when (daemonp)
  (require 'init-frames))

(provide 'init)
;;; init.el ends here
