;; TEMP
;; https://github.com/NixOS/nixpkgs/issues/108089
;; (add-to-list 'load-path "~/.emacs.d/elisp-tree-sitter/core")
;; (add-to-list 'load-path "~/.emacs.d/elisp-tree-sitter/lisp")
;; (add-to-list 'load-path "~/.emacs.d/elisp-tree-sitter/langs")
;; (require 'tree-sitter)
;; (require 'tree-sitter-hl)
;; (require 'tree-sitter-langs)
;; (require 'tree-sitter-debug)
;; (require 'tree-sitter-query)
;; END TEMP

;; (setq tree-sitter-major-mode-language-alist '((sh-mode . bash)
;;                                               (c-mode . c)
;;                                               (clojure-mode . clojure)
;;                                               (lisp-mode . commonlisp)
;;                                               (c++-mode . cpp)
;;                                               (emacs-lisp-mode . elisp)
;;                                               (fennel-mode . fennel)
;;                                               ;; (haskell-mode . haskell)
;;                                               ;; (lua-mode . lua)
;;                                               (nix-mode . nix)
;;                                               (python-mode . python)
;;                                               (ruby-mode . ruby)
;;                                               (rust-mode . rust)
;;                                               (rustic-mode . rust)
;;                                               (zig-mode . zig)))

;; (add-to-list 'tree-sitter-major-mode-language-alist '(nix-mode . nix))

;; Enable tree-sitter
;; (global-tree-sitter-mode)
;; (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode)

;; Automatically start Eglot on supported modes
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'nix-mode-hook 'eglot-ensure)
(add-hook 'racket-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rustic-mode-hook 'eglot-ensure)
(add-hook 'zig-mode-hook 'eglot-ensure)

;; Rustic should use Eglot
(setq rustic-lsp-client 'eglot)

;; Use Apheleia for automatically formatting buffers in prog-mode
(require 'apheleia)
(add-hook 'prog-mode-hook 'apheleia-mode)

;; Add zprint for formatting Clojure code to Apheleia
(push '(zprint . ("zprint"))
      apheleia-formatters)
(push '(clojure-mode . (zprint))
      apheleia-mode-alist)

;; Add nixpkgs-fmt to Apheleia
(push '(nixpkgs-fmt . ("nixpkgs-fmt"))
      apheleia-formatters)
(push '(nix-mode . (nixpkgs-fmt))
      apheleia-mode-alist)

;; Add raco fmt to Apheleia
(push '(racofmt . ("raco" "fmt"))
      apheleia-formatters)
(push '(racket-mode . (racofmt))
      apheleia-mode-alist)

;; Add zig fmt to Apheleia
(push '(zigfmt . ("zig" "fmt"))
      apheleia-formatters)
(push '(zig-mode . (zigfmt))
      apheleia-mode-alist)

;; Autostart Geiser REPL if none is running
(setq geiser-mode-start-repl-p t)

;; Don't query for killing the REPL process on exit
(setq geiser-repl-query-on-exit-p t)

;; Match pairs using electric-pairs-mode in non-lisp langs
(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)
(add-hook 'nix-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'rust-mode-hook 'electric-pair-mode)
(add-hook 'rustic-mode-hook 'electric-pair-mode)
(add-hook 'shell-mode-hook 'electric-pair-mode)
(add-hook 'zig-mode-hook 'electric-pair-mode)

;; Enable rainbow-delimiters
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight trailing whitespace
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Use spaces for indentation.
(setq-default indent-tabs-mode nil)

;; Set width of indent
(setq-default tab-width 4)
(setq standard-indent 4)

;; Highlight matching pairs of parentheses.
;; (setq show-paren-delay 0)
;; (show-paren-mode 1)

(provide 'init-langs)
