;;; init-completion.el --- Configuration for completion and minibuffer  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(savehist-mode)
(setq savehist-save-minibuffer-history t
      savehist-autosave-interval nil
      savehist-additional-variables '(register-alist
                                      kill-ring
                                      mark-ring global-mark-ring
                                      search-ring regexp-search-ring))

(setq completion-cycle-threshold 3)

(setq tab-always-indent 'complete)

(setq suggest-key-bindings t
      completions-detailed t)

(setq isearch-lazy-count t)

;;; package: hotfuzz
(setq completion-styles '(hotfuzz)
      completion-category-defaults nil
      completion-category-overrides '((eglot (styles hotfuzz))
                                      ;; Fixes consult-line's ordering, and makes it more like isearch
                                      (consult-location (styles substring)))
      completion-ignore-case t)

;; Workaround to show most recently used results first from https://github.com/axelf4/hotfuzz/issues/1
(defvar +hotfuzz--is-empty nil)
(defun +hotfuzz-all-completions--enable-history-a (orig content &rest args)
  "Set a variable needed for showing most recent entries."
  (setq +hotfuzz--is-empty (string-empty-p content))
  (apply orig content args))
(advice-add #'hotfuzz-all-completions
            :around #'+hotfuzz-all-completions--enable-history-a)
(defun +hotfuzz--adjust-metadata--enable-history-a (orig metadata)
  "Enable showing most recent entries for empty input."
  (if +hotfuzz--is-empty
      metadata
    (funcall orig metadata)))
(advice-add #'hotfuzz--adjust-metadata
            :around #'+hotfuzz--adjust-metadata--enable-history-a)

;;; package: vertico
(vertico-mode)

(require 'vertico-multiform)
(add-to-list 'vertico-multiform-categories
             '(jinx grid (vertico-grid-annotate . 20)))
(vertico-multiform-mode 1)

;;; package: marginalia
(marginalia-mode)

;;; package: consult

(require 'consult)

(keymap-global-set-keys
    "C-s" 'consult-line
    "C-x C-r" 'consult-recent-file
    "C-x b"   'consult-buffer
    "C-x p b" 'consult-project-buffer
    "M-g o"   'consult-outline
    "C-c \\"  'consult-register
    "C-c -"   'consult-register-load
    "C-c ="   'consult-register-store
    "C-c g"   'consult-grep
    "C-c G"   'consult-ripgrep
    "C-c d"   'consult-flymake)

;; Recent file tracking for consult-buffers virtual buffer types
(recentf-mode 1)

;; Compatibility with hotfuzz
(with-eval-after-load 'consult
  (setq consult--tofu-char  #x100000
        consult--tofu-range #x00fffe))

(consult-customize
 consult-line :initial (thing-at-point 'region))

;;; package: embark
(keymap-global-set "M-." 'embark-dwim)
(keymap-global-set "C-h B" 'embark-bindings)
(setq prefix-help-command #'embark-prefix-help-command)
(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))
(defun embark-external-file-handler (operation &rest args)
  (message (car args))
  (when (and (not (buffer-modified-p)) (zerop (buffer-size)) (called-interactively-p))
    (embark-open-externally (car args))
    (kill-buffer nil)))
(with-eval-after-load 'embark
  ;; (put 'embark-external-file-handler 'safe-magic t)
  ;; (put 'embark-external-file-handler 'operations '(insert-file-contents))
  ;; (add-to-list 'file-name-handler-alist '("\\.\\(?:jp?g\\|png\\|gif\\|webp\\|mp4\\|pdf\\|epub\\)\\'" . embark-external-file-handler))

  (set-keymap-parent embark-expression-map embark-general-map)
  (keymap-set-keys embark-expression-map
    "d" #'paredit-forward-down
    "n" #'paredit-forward
    "p" #'paredit-backward
    "s" #'paredit-forward-slurp-sexp
    "S" #'paredit-backward-slurp-sexp
    "a" #'paredit-forward-barf-sexp
    "A" #'paredit-backward-barf-sexp
    "r" #'paredit-raise-sexp)
  (add-to-list 'embark-pre-action-hooks  '(paredit-forward embark--end-of-target))
  (add-to-list 'embark-pre-action-hooks  '(paredit-backward embark--beginning-of-target))
  (add-to-list 'embark-post-action-hooks '(paredit-forward-down embark--beginning-of-target))
  (add-to-list 'embark-repeat-actions #'paredit-forward-down)
  (add-to-list 'embark-repeat-actions #'paredit-forward)
  (add-to-list 'embark-repeat-actions #'paredit-backward))

;;; package: corfu
(setq corfu-auto t
      corfu-auto-delay 0
      corfu-auto-prefix 1
      corfu-quit-no-match 'separator
      completion-style '(hotfuzz)
      corfu-popupinfo-delay 0
      corfu-popupinfo-hide nil)

(add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil)))
(add-hook 'shell-mode-hook (lambda () (setq-local corfu-auto nil)))
(corfu-popupinfo-mode t)
(global-corfu-mode)

;;; package: cape
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(provide 'init-completion)
;;; init-completion.el ends here
