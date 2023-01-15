;;; early-init.el --- Early init config  -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq load-prefer-newer t)

;; Disable graphical elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent resizing the initial frame
(setq frame-inhibit-implied-resize t)

;; Remove startup screen
(setq inhibit-startup-screen t)

;;; early-init.el ends here
