;;; early-init.el --- Early init config  -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp)))

(setq load-prefer-newer t)

(set-face-background 'default "#000000")
(set-face-foreground 'default "#FFFFFF")

(setq default-frame-alist '((foreground-color . "#FFFFFF")
                            (background-color . "#000000")
                            (width . 0.8)
                            (height . 0.25)
                            (menu-bar-lines . nil)
                            (tool-bar-lines . nil)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)))

(setq inhibit-startup-screen t)

(setq initial-scratch-message "")

;;; early-init.el ends here
