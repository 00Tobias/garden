(define-module (home emacs emacs)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  ;; Package definitions
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages aspell)    ;; aspell
  #:use-module (gnu packages graphviz)  ;; graphviz
  #:use-module (gnu packages fonts)     ;; font-hack
  #:use-module (gnu packages emacs)     ;; Emacs
  #:use-module (gnu packages emacs-xyz) ;; packages

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils))

(define emacs-org-roam-ui
  ;; No releases, no tags
  (let ((commit "1eb9abd4fccc7be767c5df1e158e2d17982f8193")
        (revision "0"))
    (package
     (name "emacs-org-roam-ui")
     (version (git-version "0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/org-roam/org-roam-ui")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16ld8ky0z3fi2bx79h3vrppvhgkhv62k5fymfcif7z0xmcv688kh"))))
     (build-system emacs-build-system)
     (propagated-inputs
       (list emacs-org-roam emacs-websocket emacs-simple-httpd emacs-f))
     (home-page "https://github.com/org-roam/org-roam-ui")
     (synopsis "A graphical frontend for exploring your org-roam Zettelkasten")
     (description
      "Org-Roam-UI is a frontend for exploring and interacting with your org-roam notes.")
     (license license:gpl3+))))

;; FIXME: Doesn't work
(define emacs-kaolin-themes
  (package
   (name "emacs-kaolin-themes")
   (version "1.6.7")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ogdenwebb/emacs-kaolin-themes")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "066iqbyvapc7i41xlci2jlnvdkdhkv7c8rj4ambz8rbj6i2sjb5s"))))
   (build-system emacs-build-system)
   (propagated-inputs
    (list emacs-autothemer))
   (home-page "https://github.com/ogdenwebb/emacs-kaolin-themes")
   (synopsis "Set of eye pleasing themes for GNU Emacs. Supports both GUI and terminal.")
   (description
    "Kaolin is a set of eye pleasing themes for GNU Emacs with support for a
large number of modes and external packages.")
   (license license:gpl3+)))

(define transform
  (options->transformation
   '((without-tests . "emacs-dash")
     (without-tests . "emacs-magit"))))

(define elisp-packages
  (map transform
       (list
        ;; System
        emacs-guix
        emacs-vterm
        emacs-magit
        emacs-elpher

        ;; Keybindings
        ;; emacs-meow
        ;; emacs-paredit
        emacs-lispy
        emacs-ace-window
        emacs-which-key

        ;; UI
        emacs-modus-themes
        emacs-moody
        emacs-diff-hl
        emacs-hl-todo
        emacs-rainbow-mode
        emacs-rainbow-delimiters
        emacs-popper

        ;; Completion
        emacs-vertico
        emacs-orderless
        emacs-consult
        emacs-marginalia
        emacs-corfu
        emacs-kind-icon

        ;; Org
        emacs-spell-fu
        emacs-ox-gemini
        emacs-org-roam
        emacs-org-roam-ui
        emacs-htmlize
        emacs-ox-reveal

        ;; Programming
        emacs-eglot
        emacs-apheleia
        emacs-yasnippet
        emacs-yasnippet-snippets
        emacs-auto-yasnippet
        emacs-dumb-jump

        ;; Clojure
        emacs-clojure-mode
        ;; emacs-clj-refactor
        emacs-cider

        ;; Common-lisp
        emacs-sly

        ;; Lua
        emacs-fennel-mode

        ;; Rust
        ;; emacs-rustic ;; Brings in lsp-mode >:(
        emacs-racer

        ;; Scheme
        emacs-geiser
        emacs-geiser-guile
        emacs-geiser-racket
        emacs-racket-mode

        ;; Zig
        ;; emacs-zig-mode
        )))

(define-public packages
  (list
   aspell
   aspell-dict-sv
   graphviz
   font-hack))

(define-public services
  (list
   (simple-service 'emacs-config
           home-files-service-type
           `(("emacs.d/init.el"
              ,(local-file "config/init.el"))
             ("emacs.d/early-init.el"
              ,(local-file "config/early-init.el"))
             ("emacs.d/lisp/init-completion.el"
              ,(local-file "config/lisp/init-completion.el"))
             ("emacs.d/lisp/init-keybindings.el"
              ,(local-file "config/lisp/init-keybindings.el"))
             ("emacs.d/lisp/init-langs.el"
              ,(local-file "config/lisp/init-langs.el"))
             ("emacs.d/lisp/init-org.el"
              ,(local-file "config/lisp/init-org.el"))
             ("emacs.d/lisp/init-ui.el"
              ,(local-file "config/lisp/init-ui.el"))))

   (service
    home-emacs-service-type
    (home-emacs-configuration
     (package emacs-next)
     ;; (rebuild-elisp-packages? #t)
     (server-mode? #t)
     (elisp-packages elisp-packages)))))
