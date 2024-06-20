;;; init-prog.el --- Configuration for programming modes  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq standard-indent 2)

(electric-pair-mode 1)

(setq safe-local-variable-directories '("~/projects/" "~/garden/"))

;;; treesit
(setq treesit-extra-load-path '("~/.guix-home/profile/lib/tree-sitter"))
(dolist (mapping '((bash-mode . bash-ts-mode)
                   (c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   (css-mode . css-ts-mode)
                   (html-mode . html-ts-mode)
                   (js-mode . js-ts-mode)
                   (json-mode . json-ts-mode)
                   (python-mode . python-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

(add-to-list 'auto-mode-alist '("\\.js$" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts$"  . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs$"  . rust-ts-mode))

;;; eglot
(setq eglot-events-buffer-size 0)
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)

;;; package: combobulate
(add-hook 'bash-ts-mode-hook       #'combobulate-mode)
(add-hook 'c-ts-mode-hook          #'combobulate-mode)
(add-hook 'c++-ts-mode-hook        #'combobulate-mode)
(add-hook 'css-ts-mode-hook        #'combobulate-mode)
(add-hook 'html-ts-mode-hook       #'combobulate-mode)
(add-hook 'js-ts-mode-hook         #'combobulate-mode)
(add-hook 'json-ts-mode-hook       #'combobulate-mode)
(add-hook 'python-ts-mode-hook     #'combobulate-mode)
(add-hook 'typescript-ts-mode-hook #'combobulate-mode)
(add-hook 'tsx-ts-mode-hook        #'combobulate-mode)
(add-hook 'ruby-ts-mode-hook       #'combobulate-mode)
(add-hook 'yaml-ts-mode-hook       #'combobulate-mode)

;;; package: tempel
(with-eval-after-load 'tempel
  (unless (listp 'tempel-path)
    (setq tempel-path (list tempel-path)))
  (add-to-list 'tempel-path "~/git/guix/etc/snippets/tempel/*"))
(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand completion-at-point-functions)))

(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

;;; package: ellama
(require 'ellama)
(require 'llm-openai)

(setq llm-warn-on-nonfree nil
      ellama-auto-scroll t
      ellama-spinner-type 'rotating-line
      ellama-keymap-prefix "C-c e"
      ellama-provider (make-llm-openai-compatible :url "http://127.0.0.1:8385"))

(defun start-llama ()
  (interactive)
  (when (string= (system-name) "okarthel")
    (setq llama-server-process
          (start-process-shell-command
           "llama-server-process"
           "*Llama server*"
           "llama-server -m ~/ai/models/deepseek-coder-6.7b-instruct.Q6_K.gguf -c 2048 -ngl 99 --chat-template deepseek --port 8385 --embeddings --log-disable"
           nil 0))))

(defun kill-llama ()
  (interactive)
  (delete-process llama-server-process))

;;; package: aggressive-indent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'shell-mode)

;;; package: avy
(avy-setup-default)
(keymap-global-set-keys "C-c C-j" 'avy-resume
                        "C-r"     'avy-goto-char
                        "C-."     'avy-goto-char
                        "C-:"     'avy-goto-char-2
                        "M-g g"   'avy-goto-line
                        "M-g w"   'avy-goto-word-1
                        "M-g e"   'avy-goto-word-0)

;;; package: expand-region
(require 'expand-region)
(keymap-global-set "C-=" 'er/expand-region)

;;; package: paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'paredit-mode-hook (lambda () (electric-indent-local-mode 0)))
(add-hook 'paredit-mode-hook (lambda () (electric-pair-local-mode 0)))
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(eldoc-add-command 'paredit-backward-delete 'paredit-close-round)
(with-eval-after-load 'paredit (setcar (alist-get 'paredit-mode minor-mode-alist) " ()"))

;;; Language major modes

;;; Clojure
;;; package: cider
(setq cider-repl-display-help-banner nil
      cider-use-overlays t
      cider-use-tooltips nil)

;;; Common lisp

(put 'define-configuration 'lisp-indent-function 'defun)

(setq inferior-lisp-program "sbcl")
(with-eval-after-load 'browse-url (add-to-list 'browse-url-handlers '("hyperspec" . eww-browse-url)))
;;; package: sly
(defun sly-eval-sexp-overlay ()
  (interactive)
  (let ((result (->> `(slynk:pprint-eval ,(sly-sexp-at-point))
                     sly-eval
                     s-trim
                     (s-replace "\n" ", "))))
    (eros--make-result-overlay result
      :where (point)
      :duration eros-eval-result-duration)))
;;; package: sly-asdf

;;; package: clhs
(require 'clhs)
(clhs-setup)

;;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;;; package: eros
(eros-mode 1)

;;; Scheme
;;; package: geiser
(require 'geiser)
(setq geiser-mode-auto-p t
      geiser-repl-query-on-kill-p nil)
;;; package: geiser-guile
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/git/guix"))
;;; package: guix
;; Broken package: https://issues.guix.gnu.org/55013
(with-eval-after-load 'guix-repl
  (setq guix-guile-program '("guix" "repl")))

;;; Web
;;; package: web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-part-padding 2
      web-mode-code-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-indent-style 4)

(provide 'init-prog)
;;; init-prog.el ends here
