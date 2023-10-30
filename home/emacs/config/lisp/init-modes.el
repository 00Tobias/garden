;;; init-modes --- Configuration for various major modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; dired

(setq dired-dwim-target t
      dired-mouse-drag-files t
      dired-listing-switches "-lhA --group-directories-first")

(with-eval-after-load 'dired (keymap-set dired-mode-map "b" 'dired-up-directory))

(defun dired-open-externally (&optional arg)
  "Open marked or current file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (embark-open-externally (dired-get-filename))
   arg))
(with-eval-after-load 'dired (keymap-set dired-mode-map "E" 'dired-open-externally))

;;; eww

(setq shr-width 80
      eww-search-prefix "https://html.duckduckgo.com/html/?q="
      ;; browse-url-browser-function 'browse-url-default-browser
      eww-use-external-browser-for-content-type "\\`\\(video/\\|audio/\\|application/ogg\\|application/pdf\\)")

(defun mpv-play-link (url &rest args)
  (interactive)
  (start-process "mpv" nil "mpv" url))

(setq browse-url-handlers
      '(("https://www.youtube.com/\.*" . mpv-play-link)))

;;; xwidget-webkit

(add-hook 'xwidget-webkit-mode-hook (lambda () (display-line-numbers-mode 0)))

(require 'xwidget)
(keymap-set-keys xwidget-webkit-mode-map
  "h" 'xwidget-webkit-back
  "j" 'xwidget-webkit-scroll-up-line
  "k" 'xwidget-webkit-scroll-down-line
  "l" 'xwidget-webkit-forward
  "n" 'xwidget-webkit-back
  "e" 'xwidget-webkit-scroll-up-line
  "i" 'xwidget-webkit-scroll-down-line
  "o" 'xwidget-webkit-forward)

;;; package: xwwp
(keymap-set xwidget-webkit-mode-map "f" 'xwwp-follow-link)


;;; gnus

;; (add-to-list 'load-path "~/git/nnatom")
;; (require 'nnatom)

(with-eval-after-load 'mm-decode (add-to-list 'mm-attachment-override-types "image/.*"))

(setq gnus-asynchronous t
      gnus-use-cache t
      gnus-use-header-prefetch t
      gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n"

      gnus-select-method '(nnnil nil)
      gnus-secondary-select-methods '(;; (nntp "news.eternal-september.org")
                                      (nnatom "fasterthanli.me/index.xml"
                                              (nnatom-name "fasterthanli.me"))
                                      ;; Recieving mail
                                      (nnimap "rainboards"
                                              (nnimap-address "imap.soverin.net")
                                              (nnimap-server-port "imaps")
                                              (nnimap-stream ssl)
                                              (nnir-search-engine imap)
                                              (nnmail-expiry-target "nnimap+home:[Example]/Trash")
                                              (nnmail-expiry-wait 'immediate)))

      ;; Sending mail
      message-send-mail-function 'message-use-send-mail-function
      send-mail-function 'smtpmail-send-it
      gnus-posting-styles '(("rainboards"
	                           (address "tobias@rainboards.com")
	                           ("X-Message-SMTP-Method" "smtp smtp.soverin.net 587 tobias@rainboards.com")))
      gnus-parameters '(("rainboards"
	                       (gcc-self . "nnimap+personal:Sent"))))

;;; package: elpher

;;; package: magit

(with-eval-after-load 'magit
  (setcar (alist-get 'auto-revert-mode minor-mode-alist) ""))

;;; eshell

(setq eshell-banner-message ""
      eshell-scroll-to-bottom-on-input 'this
      comint-scroll-to-bottom-on-input 'this
      comint-prompt-read-only t)

(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Aliases from https://github.com/howardabrams/hamacs/blob/main/ha-eshell.org

(defun eshell-fn-on-files (fun1 fun2 args)
  "Call FUN1 on the first element in list, ARGS.
Call FUN2 on all the rest of the elements in ARGS."
  (unless (null args)
    (let ((filenames (flatten-list args)))
      (funcall fun1 (car filenames))
      (when (cdr filenames)
        (mapcar fun2 (cdr filenames))))
    ;; Return an empty string, as the return value from `fun1'
    ;; probably isn't helpful to display in the `eshell' window.
    ""))

(defun eshell/e (&rest files)
  "Essentially an alias to the `find-file' function."
  (eshell-fn-on-files 'find-file 'find-file-other-window files))

(defun eshell/ee (&rest files)
  "Edit one or more files in another window."
  (eshell-fn-on-files 'find-file-other-window 'find-file-other-window files))

(defalias 'eshell/emacs 'eshell/e)
(defalias 'eshell/vi 'eshell/e)
(defalias 'eshell/vim 'eshell/e)

(defun eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (eshell-fn-on-files 'view-file 'view-file-other-window files))

(defalias 'eshell/more 'eshell/less)
(defalias 'eshell/view 'eshell/less)

;;; package: pcmpl-args

(provide 'init-modes)
;;; init-modes.el ends here
