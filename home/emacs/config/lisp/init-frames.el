;;; init-frames --- Configuration for a frame exclusive workflow  -*- lexical-binding: t; -*-

;;; Commentary:
;; Use my window manager as my window manager

;;; Code:

(setq minibuffer-follows-selected-frame nil)

;; Dedicated minibuffer frame

;; (setq minibuffer-auto-raise t
;;       inhibit-message t)

;; (push '(minibuffer . nil) default-frame-alist)
;; (push '(minibuffer . nil) initial-frame-alist)
;; (push '(name . "minibuffer") minibuffer-frame-alist)
;; (setq minibuffer-frame-alist (append '((name . "minibuffer")
;;                                        (width . 120)
;;                                        (height . 11))
;;                                      minibuffer-frame-alist))

;; ;; Shadow the signal function to stop errors from popping up the minibuffer
;; (defun signal (error-signal data)
;;   (message data))

;; (add-hook 'minibuffer-setup-hook 'make-frame-visible)
;; (add-hook 'vertico-mode-hook 'make-frame-visible)
;; (add-hook 'minibuffer-exit-hook 'make-frame-invisible)

;;; package: frames-only-mode
(with-eval-after-load 'frames-only-mode
  (add-to-list 'frames-only-mode-use-window-functions #'corfu-popupinfo--show)
  (add-to-list 'frames-only-mode-use-window-functions #'pcmpl-args-guess-display-width)
  (add-to-list 'frames-only-mode-use-window-functions #'embark-act))
(frames-only-mode 1)

(provide 'init-frames)
;;; init-frames.el ends here
