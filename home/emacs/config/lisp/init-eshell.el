;;; init-eshell --- Configuration for Eshell  -*- lexical-binding: t; -*-

;;; Commentary:
;; TODO: Consider generalizing to init-modes.el in the future :^)

;;; Code:

;; Disable welcome message
(setq eshell-banner-message "")

;; Disable line numbers in eshell-mode
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

;; (with-package 'pcmpl-args)

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

(provide 'init-eshell)
;;; init-eshell.el ends here
