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

;; Recent file tracking for consult-buffers virtual buffer types
(recentf-mode 1)

(use-package hotfuzz
  :init (setq completion-styles '(hotfuzz)
              completion-category-defaults nil
              completion-category-overrides '((eglot (styles hotfuzz))
                                              ;; Fixes consult-line's ordering, and makes it more like isearch
                                              (consult-location (styles substring)))
              completion-ignore-case t)
  :config
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
              :around #'+hotfuzz--adjust-metadata--enable-history-a))

(use-package vertico
  :demand t
  :config (vertico-mode 1))

(use-package vertico-multiform
  :after vertico
  :hook (vertico-mode . vertico-multiform-mode)
  :config (add-to-list 'vertico-multiform-categories
                       '(jinx grid (vertico-grid-annotate . 20))))

(use-package marginalia
  :demand t
  :config (marginalia-mode t))

(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x C-r" . consult-recent-file)
         ("C-x b"   . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-g o"   . consult-outline)
         ("C-c \\"  . consult-register)
         ("C-c -"   . consult-register-load)
         ("C-c ="   . consult-register-store)
         ("C-c g"   . consult-grep)
         ("C-c G"   . consult-ripgrep)
         ("C-c d"   . consult-flymake))
  :config
  (consult-customize
   consult-line :initial (thing-at-point 'region))
  ;; Compatibility with hotfuzz
  (setq consult--tofu-char  #x100000
        consult--tofu-range #x00fffe))

(use-package embark
  :after paredit
  :bind (("M-."   . embark-dwim)
         ("C-h B" . embark-bindings)
         :map embark-expression-map
         ("d" . paredit-forward-down)
         ("n" . paredit-forward)
         ("p" . paredit-backward)
         ("s" . paredit-forward-slurp-sexp)
         ("S" . paredit-backward-slurp-sexp)
         ("a" . paredit-forward-barf-sexp)
         ("A" . paredit-backward-barf-sexp)
         ("r" . paredit-raise-sexp))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-pre-action-hooks  '(paredit-forward embark--end-of-target))
  (add-to-list 'embark-pre-action-hooks  '(paredit-backward embark--beginning-of-target))
  (add-to-list 'embark-post-action-hooks '(paredit-forward-down embark--beginning-of-target))
  (add-to-list 'embark-repeat-actions #'paredit-forward-down)
  (add-to-list 'embark-repeat-actions #'paredit-forward)
  (add-to-list 'embark-repeat-actions #'paredit-backward))

(use-package corfu
  :demand t
  :init (setq corfu-auto t
              corfu-auto-delay 0.1
              corfu-auto-prefix 3
              corfu-quit-no-match 'separator
              completion-style '(hotfuzz)
              corfu-popupinfo-delay 0
              corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode 1)
  (global-corfu-mode 1))

(use-package cape
  :bind ("M-<tab>" . cape-prefix-map)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(provide 'init-completion)
;;; init-completion.el ends here
