;;; init-frames --- Configuration for a frame exclusive workflow  -*- lexical-binding: t; -*-

;;; Commentary:
;; Use my window manager as my window manager

;;; Code:

;; Frame title as modeline replacement
(setq frame-title-format '("%e" mode-line-front-space
                           (:propertize
                            ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                            display (min-width (5.0)))
                           mode-line-frame-identification mode-line-buffer-identification "   "
                           mode-line-position (vc-mode vc-mode) "  " mode-line-modes mode-line-misc-info
                           mode-line-end-spaces))
(setq-default mode-line-format nil)

;; Dedicated minibuffer frame

(setq inhibit-message t)

(push '(minibuffer . nil) default-frame-alist)
(push '(minibuffer . nil) initial-frame-alist)
(setq minibuffer-frame-alist (append '((name . "minibuffer")
                                       (width . 150)
                                       (height . 11))
                                     minibuffer-frame-alist))

(defun setup-minibuffer-identifier (&rest ignored)
  (defconst minibuffer-identifier
    (let ((string (shell-command-to-string
                   "xdotool search --name '^minibuffer$'")))
      ;; Remove newline from output
      (substring string 0 (1- (length string)))))
  (remove-hook 'after-make-frame-functions #'setup-minibuffer-identifier))

(add-hook 'after-make-frame-functions #'setup-minibuffer-identifier)

(defun pull-minibuffer-frame (&rest ignored)
  (call-process-shell-command
   (concat "bspc node "
           minibuffer-identifier
           " -d $(bspc query -d -D focused) && bspc node -f "
           minibuffer-identifier)
   nil 0))

;; (advice-add 'read-from-minibuffer :before 'pull-minibuffer-frame)
;; (advice-add 'read-no-blanks-input :before 'pull-minibuffer-frame)
;; (advice-add 'read-string :before 'pull-minibuffer-frame)
;; (advice-add 'read-char :before 'pull-minibuffer-frame)
;; (advice-add 'read-event :before 'pull-minibuffer-frame)

(add-hook 'minibuffer-setup-hook 'pull-minibuffer-frame)

;; (add-hook 'minibuffer-setup-hook 'vertico--update)

(add-hook 'minibuffer-exit-hook (lambda ()
                                  (call-process-shell-command
                                   (concat "bspc node "
                                           minibuffer-identifier
                                           " -d X")
                                   nil 0)))

;;; package: frames-only-mode
(with-eval-after-load 'frames-only-mode
  (add-to-list 'frames-only-mode-use-window-functions #'corfu-popupinfo--show)
  (add-to-list 'frames-only-mode-use-window-functions #'pcmpl-args-guess-display-width)
  (add-to-list 'frames-only-mode-use-window-functions #'embark-act))
(frames-only-mode 1)

(provide 'init-frames)
;;; init-frames.el ends here
