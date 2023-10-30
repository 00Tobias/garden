(define-module (home emacs emacs)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages cmake) #:select (cmake-minimal))
  #:use-module ((guix licenses) #:select (gpl3+))

  #:use-module ((gnu packages emacs) #:select (emacs-next-tree-sitter))
  #:use-module ((gnu packages gnome) #:select (gsettings-desktop-schemas))
  #:use-module ((gnu packages webkit) #:select (webkitgtk-with-libsoup2))
  #:use-module ((gnu packages xorg) #:select (libxcomposite))
  #:use-module ((gnu packages aspell) #:select (aspell aspell-dict-en aspell-dict-sv))
  #:use-module ((gnu packages fonts) #:select (font-sarasa-gothic font-cozette))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages lisp) #:select (sbcl))
  #:use-module ((gnu packages clojure) #:select (clojure clojure-tools))
  #:use-module ((gnu packages java) #:select (openjdk))
  #:use-module ((gnu packages cpp) #:select (ccls))
  #:use-module ((gnu packages python-xyz) #:select (python-lsp-server))
  #:use-module ((gnu packages node) #:select (node))
  #:use-module ((contrib packages node-xyz) #:select (node-typescript node-typescript-language-server))
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (gnu home-services-utils))

(define emacs-combobulate
  (package (name "emacs-combobulate")
           (version (git-version "0" "1" "c7e4670a3047c0b58dff3746577a5c8e5832cfba"))
           (home-page "https://github.com/mickeynp/combobulate")
           (source (origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/mickeynp/combobulate")
                          (commit "c7e4670a3047c0b58dff3746577a5c8e5832cfba")))
                    (sha256 (base32 "063w2sm0c7xhg3ml31xp870azb0sv7z689lnbnjnbl3rfdy4kg50"))))
           (build-system emacs-build-system)
           (synopsis "Structured Editing and Navigation in Emacs")
           (description "This package adds structured editing and movement to a wide range of programming languages.")
           (license gpl3+)))

(define emacs-hotfuzz
  (let ((commit "3076cb250d0cb7ac6c3ec746dc4ccfea09ccdb25"))
    (package
     (name "emacs-hotfuzz")
     (version commit)
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/axelf4/hotfuzz")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256 (base32 "0hc13big8cnqf19cjcdwq9k1plpc9401hf2ddxf4vd0gyqlxlsxn"))))
     (build-system emacs-build-system)
     (arguments
      `(#:modules ((guix build emacs-build-system)
                   ((guix build cmake-build-system) #:prefix cmake:)
                   (guix build emacs-utils)
                   (guix build utils))
        #:imported-modules (,@%emacs-build-system-modules
                            (guix build cmake-build-system))
        #:phases
        (modify-phases %standard-phases
                       (add-after 'unpack 'patch-module-load
                                  (lambda* (#:key outputs #:allow-other-keys)
                                    (make-file-writable "hotfuzz.el")
                                    (emacs-substitute-sexps "hotfuzz.el"
                                                            ("(declare-function hotfuzz--filter-c"
                                                             (string-append (assoc-ref outputs "out") "/lib/hotfuzz-module.so"))
                                                            ("(use-module-p"
                                                             `(module-load ,(string-append (assoc-ref outputs "out") "/lib/hotfuzz-module.so"))))))
                       (add-after 'build 'configure
                                  (lambda* (#:key outputs #:allow-other-keys)
                                    ((assoc-ref cmake:%standard-phases 'configure)
                                     #:outputs outputs
                                     #:out-of-source? #f
                                     #:build-type "Release")))
                       (add-after 'configure 'make
                                  (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
                                    (apply invoke "cmake" "--build" "." make-flags)
                                    (install-file
                                     "hotfuzz-module.so"
                                     (string-append (assoc-ref outputs "out") "/lib")))))
        #:tests? #f))
     (native-inputs (list cmake-minimal))
     (home-page "https://github.com/axelf4/hotfuzz")
     (synopsis "Fuzzy Emacs completion style")
     (description "This is a fuzzy Emacs completion style similar to the built-in flex style, but with a better scoring algorithm.")
     (license gpl3+))))

(define elisp-packages
  (list
   ;; init-ui.el
   emacs-diff-hl
   emacs-vundo

   ;; init-completion.el
   emacs-hotfuzz
   emacs-vertico
   emacs-marginalia
   emacs-consult
   emacs-embark
   emacs-corfu
   emacs-cape
   emacs-kind-icon

   ;; init-prog.el
   emacs-combobulate
   emacs-tempel
   emacs-aggressive-indent
   emacs-avy
   emacs-paredit
   ;; Langs
   emacs-eros
   emacs-cider
   emacs-sly
   emacs-sly-asdf
   emacs-geiser
   emacs-geiser-guile
   emacs-guix
   emacs-web-mode

   ;; init-text.el
   ;; emacs-org-block-capf
   emacs-org-roam

   ;; init-modes.el
   ;; emacs-nnatom
   emacs-elpher
   emacs-magit
   emacs-pcmpl-args

   ;; init-frames.el
   emacs-frames-only-mode))

(define-public packages
  (list
   aspell
   aspell-dict-en
   aspell-dict-sv
   font-sarasa-gothic
   font-cozette
   ;; Langs
   python
   sbcl
   clojure
   clojure-tools
   openjdk
   ;; LSP
   ccls
   python-lsp-server
   node
   node-typescript
   node-typescript-language-server
   ;; Treesitter
   tree-sitter-bash
   tree-sitter-c
   tree-sitter-clojure
   tree-sitter-cpp
   tree-sitter-css
   tree-sitter-html
   tree-sitter-rust
   tree-sitter-python
   tree-sitter-javascript
   tree-sitter-json
   tree-sitter-typescript))

(define-public services
  (list
   (simple-service 'emacs-config
                   home-files-service-type
                   `((".emacs.d/init.el"                 ,(local-file "config/init.el"))
                     (".emacs.d/early-init.el"           ,(local-file "config/early-init.el"))
                     (".emacs.d/templates"               ,(local-file "config/templates"))
                     (".emacs.d/lisp/init-ui.el"         ,(local-file "config/lisp/init-ui.el"))
                     (".emacs.d/lisp/init-completion.el" ,(local-file "config/lisp/init-completion.el"))
                     (".emacs.d/lisp/init-prog.el"       ,(local-file "config/lisp/init-prog.el"))
                     (".emacs.d/lisp/init-text.el"       ,(local-file "config/lisp/init-text.el"))
                     (".emacs.d/lisp/init-modes.el"      ,(local-file "config/lisp/init-modes.el"))
                     (".emacs.d/lisp/init-modal.el"      ,(local-file "config/lisp/init-modal.el"))
                     (".emacs.d/lisp/init-frames.el"     ,(local-file "config/lisp/init-frames.el"))))
   (service
    home-emacs-service-type
    (home-emacs-configuration
     (emacs (replace-mesa emacs-next-tree-sitter))
     ;; (rebuild-elisp-packages? #t)
     ;; (server-mode? #t)
     (elisp-packages elisp-packages)))))
