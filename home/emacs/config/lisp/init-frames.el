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
(setq minibuffer-frame-alist
      (append '((name . "minibuffer")
                (title . "minibuffer")
                (width . 150)
                (height . 11))
              minibuffer-frame-alist))

(defun pull-minibuffer-frame (&rest ignored)
  (call-process-shell-command
   "i3-msg [title='minibuffer'] move workspace current"
   nil 0))

(add-hook 'minibuffer-setup-hook 'pull-minibuffer-frame)

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (call-process-shell-command
             "i3-msg [title='minibuffer'] move workspace 10"
             nil 0)))

;;; package: frames-only-mode
(with-eval-after-load 'frames-only-mode
  (add-to-list 'frames-only-mode-use-window-functions #'corfu-popupinfo--show)
  (add-to-list 'frames-only-mode-use-window-functions #'pcmpl-args-guess-display-width)
  (add-to-list 'frames-only-mode-use-window-functions #'embark-act))
(frames-only-mode 1)

(provide 'init-frames)
;;; init-frames.el ends here
