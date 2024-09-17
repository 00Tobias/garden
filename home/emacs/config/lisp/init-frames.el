;;; init-frames --- Configuration for a frame exclusive workflow  -*- lexical-binding: t; -*-

;;; Commentary:
;; Use my window manager as my window manager

;;; Code:

;; Frame title as modeline replacement

(setq frame-title-format
      '("%e%z"
        (:eval (mode-line-eol-desc))
        mode-line-client
        "%*%+%@ %b  : :  <"
        ;; Unfortunately %l and %c is ignored for frame-title-format
        (:eval (number-to-string (line-number-at-pos)))
        ":"
        (:eval (number-to-string (current-column)))
        " / %p>  "
        mode-line-modes
        mode-line-misc-info
        (:eval (current-message))))
(setq-default mode-line-format nil)

;; Dedicated minibuffer frame

(push '(minibuffer . nil) default-frame-alist)
(push '(minibuffer . nil) initial-frame-alist)
(setq minibuffer-frame-alist
      (append '((name . "dedicated-minibuffer-frame")
                (width . 150)
                (height . 11))
              minibuffer-frame-alist))

(defun pull-minibuffer-frame (&rest ignored)
  (if (string= (system-name) "austrat")
      (call-process-shell-command
       "SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock swaymsg [title='^dedicated-minibuffer-frame$'] move workspace current"
       nil 0)
    (call-process-shell-command
     "i3-msg [title='^dedicated-minibuffer-frame$'] move workspace current"
     nil 0)))

(defun push-minibuffer-frame (&rest ignored)
  (if (string= (system-name) "austrat")
      (call-process-shell-command
       "SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock swaymsg [title='^dedicated-minibuffer-frame$'] move workspace 10"
       nil 0)
    (call-process-shell-command
     "i3-msg [title='^dedicated-minibuffer-frame$'] move workspace 10"
     nil 0)))

(add-hook 'minibuffer-setup-hook 'pull-minibuffer-frame)
(add-hook 'minibuffer-exit-hook 'push-minibuffer-frame)
(advice-add 'read-key-sequence :before 'pull-minibuffer-frame)
(advice-add 'read-key-sequence :after 'push-minibuffer-frame)

;;; package: frames-only-mode
(with-eval-after-load 'frames-only-mode
  (add-to-list 'frames-only-mode-use-window-functions #'corfu-popupinfo--show)
  (add-to-list 'frames-only-mode-use-window-functions #'pcmpl-args-guess-display-width)
  (add-to-list 'frames-only-mode-use-window-functions #'embark-act))
(frames-only-mode 1)

(provide 'init-frames)
;;; init-frames.el ends here
