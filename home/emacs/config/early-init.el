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

(setq-default pgtk-wait-for-event-timeout 0)

(setq default-frame-alist '((width . 0.8)
                            (height . 0.25)
                            (menu-bar-lines . nil)
                            (tool-bar-lines . nil)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)
                            (font . "Iosevka Nerd Font-11")))

(set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 100 :weight 'medium)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font" :height 100 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "AtkynsonNext Nerd Font Propo" :height 110 :weight 'medium)

(setq inhibit-startup-screen t)

(setq initial-scratch-message "")

;;; early-init.el ends here
