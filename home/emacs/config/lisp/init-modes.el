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

;;; gnus

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

(setq
 eshell-hist-ignoredups t
 eshell-history-size 1024
 eshell-prompt-function
 (lambda ()
   (concat
    (propertize (concat " " (abbreviate-file-name (eshell/pwd)) " ")
                'face `(:weight bold :background ,(face-foreground 'default) :foreground ,(face-background 'default)))
    (propertize (format-time-string "(%H:%M:%S)" (current-time))
                'face `(:background ,(face-foreground 'default) :foreground ,(face-background 'default)))
    (propertize (if-let ((status eshell-last-command-status))
                    (if (= status 0) " " (format " [%s] " status)))
                'face `(:weight bold :background ,(face-foreground 'default) :inherit error))
    "\n"))
 eshell-highlight-prompt nil
 eshell-banner-message ""
 eshell-scroll-to-bottom-on-input 'this
 comint-scroll-to-bottom-on-input 'this
 comint-prompt-read-only t)

(setenv "PAGER" "cat")

(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

(defun eshell-insert-history ()
  "Displays the eshell history to select and insert back into your eshell."
  (interactive)
  (insert (completing-read "Eshell history: "
                           (delete-dups
                            (ring-elements eshell-history-ring)))))

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

(with-eval-after-load 'eshell (keymap-set eshell-mode-map "M-r" 'eshell-insert-history))

;;; package: pcmpl-args

;;; package: eshell-syntax-highlighting
(eshell-syntax-highlighting-global-mode 1)

;;; package: esh-autosuggest
(add-hook 'eshell-mode-hook (lambda () (esh-autosuggest-mode 1)))

;;; package: fish-completion
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))

(provide 'init-modes)
;;; init-modes.el ends here
