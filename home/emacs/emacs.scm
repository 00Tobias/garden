(define-module (home emacs emacs)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages cmake) #:select (cmake-minimal))
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages)
  #:use-module ((gnu packages emacs) #:select (emacs-next emacs-next-pgtk))
  #:use-module ((gnu packages gtk) #:select (gtk+/fixed))
  #:use-module ((gnu packages rust-apps) #:select (ripgrep))
  #:use-module ((gnu packages enchant) #:select (enchant))
  #:use-module ((gnu packages aspell) #:select (aspell aspell-dict-en aspell-dict-sv))
  #:use-module ((gnu packages imagemagick) #:select (imagemagick))
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages compression) #:select (zip unzip))
  #:use-module ((gnu packages tex) #:select (texlive-scheme-basic
                                             texlive-wrapfig
                                             texlive-ulem
                                             texlive-capt-of))
  #:use-module ((gnu packages base) #:select (binutils coreutils gnu-make sed))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages lisp) #:select (sbcl))
  #:use-module ((gnu packages readline) #:select (rlwrap))
  #:use-module ((gnu packages clojure) #:select (clojure clojure-tools))
  #:use-module ((gnu packages java) #:select (openjdk23 icedtea java-slf4j-simple))
  #:use-module ((gnu packages cpp) #:select (ccls))
  #:use-module ((gnu packages guile) #:select (guile-next))
  #:use-module ((gnu packages guile-xyz) #:select (guile-ares-rs))
  #:use-module ((gnu packages python-xyz) #:select (python-lsp-server))
  #:use-module ((gnu packages rust) #:select (rust rust-analyzer))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module ((gnu packages node) #:select (node))
  #:use-module ((contrib packages node-xyz) #:select (node-typescript node-typescript-language-server))
  #:use-module ((gnu packages ocaml) #:select (emacs-tuareg))
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((nongnu packages emacs) #:select (clhs))
  #:use-module ((nongnu packages clojure) #:select (clj-kondo clojure-lsp))
  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module ((emacs-master) #:select (emacs-master-pgtk emacs-master))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (gnu home-services-utils)

  #:use-module ((trowel) #:select (aggressively-optimize))
  #:use-module ((home theme) #:prefix theme:))

(define-public emacs-package
  (cond ((or (string= (gethostname) "austrat")
             (string= (gethostname) "okarthel"))
         ;; NOTE: Emacs on the default Guix version of gtk+ segfaults when closing a frame on gdk_window_get_screen. Updating gtk seems to fix the issue.
         (replace-mesa
          (package
            (inherit emacs-master-pgtk)
            (inputs (modify-inputs (package-inputs emacs-master-pgtk)
                      (replace "gtk+"
                        (package
                          (inherit gtk+/fixed)
                          (name "gtk+")
                          (version "3.24.51")
                          (source (origin
                                    (method git-fetch)
                                    (uri (git-reference
                                          (url "https://gitlab.gnome.org/GNOME/gtk")
                                          (commit "ef9abe706ac0e35590aa188b104ca55228f5d6b4")))
                                    (file-name (git-file-name name version))
                                    (sha256 (base32 "13r0l774k8qjp3ngsc9pbv9s3skmdxcw03z8fgyqm2dra947n0sb")))))))))))
        (else (emacs-master))))

(define emacs-ultra-scroll
  (let ((commit "93cd969c2ed1e75a950e6dec112a0ab1e4a6903b"))
    (package
      (name "emacs-ultra-scroll")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/jdtsmith/ultra-scroll")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/jdtsmith/ultra-scroll")
                      (commit commit)))
                (sha256 (base32 "1r9940l361jjrir59gvs5ivlpsgjhs13cwbhqy8j3zsmxf45z3qc"))))
      (build-system emacs-build-system)
      (synopsis "Scroll Emacs like lightning")
      (description "ultra-scroll is a smooth-scrolling package for emacs, with
native support for standard builds as well as emacs-mac.")
      (license license:gpl3+))))

(define emacs-flymake-posframe
  (let ((commit "fadf75a51a9ab479e3ad00e23b7f38cbadaec460"))
    (package
      (name "emacs-flymake-posframe")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/Ladicle/flymake-posframe")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Ladicle/flymake-posframe")
                      (commit commit)))
                (sha256 (base32 "0css70ncc9ygv932mf89schh86ybcw5qx4fg897lrbrma99fc9w2"))))
      (inputs (list emacs-posframe))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-for-flymake-1.4
              (lambda _
                (substitute* "flymake-posframe.el"
                  (("flymake--diag-text") "flymake-diagnostic-text")))))))
      (synopsis "Showing flymake diagnostics at point using posframe")
      (description "Showing flymake diagnostics at point using posframe.")
      (license license:gpl3+))))

(define emacs-nerd-icons-completion
  (let ((commit "8e5b995eb2439850ab21ba6062d9e6942c82ab9c"))
    (package
      (name "emacs-nerd-icons-completion")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/rainstormstudio/nerd-icons-completion")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rainstormstudio/nerd-icons-completion")
                      (commit commit)))
                (sha256 (base32 "0nbyrzz5sscycbr1h65ggzrm1m9agfwig2mjg7jljzw8dk1bmmd2"))))
      (inputs (list emacs-nerd-icons))
      (build-system emacs-build-system)
      (synopsis "Use nerd-icons for completion")
      (description "Use nerd-icons for completion.")
      (license license:gpl3+))))

(define emacs-nerd-icons-dired
  (let ((commit "c0b0cda2b92f831d0f764a7e8c0c6728d6a27774"))
    (package
      (name "emacs-nerd-icons-dired")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/rainstormstudio/nerd-icons-dired")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rainstormstudio/nerd-icons-dired")
                      (commit commit)))
                (sha256 (base32 "1iwqzh32j6fsx0nl4y337iqkx6prbdv6j83490riraklzywv126a"))))
      (inputs (list emacs-nerd-icons))
      (build-system emacs-build-system)
      (synopsis "Use nerd-icons for Dired")
      (description "Use nerd-icons for Dired.")
      (license license:gpl3+))))

(define emacs-nerd-icons-corfu
  (let ((commit "41110180ceab9d0edaa856d19633b2b3fdf82e75"))
    (package
      (name "emacs-nerd-icons-corfu")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/LuigiPiucco/nerd-icons-corfu")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/LuigiPiucco/nerd-icons-corfu")
                      (commit commit)))
                (sha256 (base32 "0mwng5khhq6iqmr0ip8fv227cnkv0mv5664qz57r7sbjplqyabgf"))))
      (inputs (list emacs-nerd-icons))
      (build-system emacs-build-system)
      (synopsis "Icons for corfu via nerd-icons")
      (description "Nerd-icons-corfu.el is a library for adding icons to completions in Corfu.")
      (license license:gpl3+))))

(define emacs-ocaml-eglot
  (let ((commit "387e53ce2ce4970e7f754013f57a4e41933de016"))
    (package
      (name "emacs-ocaml-eglot")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/tarides/ocaml-eglot")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tarides/ocaml-eglot")
                      (commit commit)))
                (sha256 (base32 "1c8viz2gpmm6cilxjbk4r4j5bzpl9954amrz2gfl1g84fgycxhqa"))))
      (build-system emacs-build-system)
      (arguments
       (list
        ;; Linting the package returns errors during 'check'
        #:test-command #~(list "make" "test-ert")
        #:phases
        #~(modify-phases %standard-phases ; From emacs-xyz.scm
            (add-after 'unpack 'inject-makel
              (lambda* (#:key inputs #:allow-other-keys)
                (symlink (search-input-file inputs "/include/makel.mk")
                         "makel.mk"))))))
      (inputs (list makel))
      (synopsis "An overlay on Eglot for editing OCaml code using LSP")
      (description "ocaml-eglot is a lightweight Emacs minor mode designed to
enhance the experience of writing OCaml code by leveraging the Eglot Language
Server Protocol (LSP) client.")
      (license license:x11))))          ; MIT

(define emacs-hotfuzz
  (let ((commit "48fcdae4b6aef1c9a92e600449e7a1e057b745d7"))
    (package
      (name "emacs-hotfuzz")
      (version (git-version "0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/axelf4/hotfuzz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 "0q96w0n30pfmnvr5ri0088m2fhb63qjcy6vahcn3ymjn4yjwkm8d"))))
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
      (license license:gpl3+))))


(define sbcl-config (plain-file "sbcl-config" "
(require \"asdf\")
(let ((guix-profile (format nil \"~a/.guix-profile/lib/\" (uiop:getenv \"HOME\")))
      (guix-home (format nil \"~a/.guix-home/profile/lib/\" (uiop:getenv \"HOME\"))))
  (when (ignore-errors (asdf:load-system \"cffi\"))
    (when (probe-file guix-profile)
      (push guix-profile
            (symbol-value (find-symbol (string '*foreign-library-directories*)
                                       (find-package 'cffi)))))
    (when (probe-file guix-home)
      (push guix-home
            (symbol-value (find-symbol (string '*foreign-library-directories*)
                                       (find-package 'cffi)))))))
"))

(define without-tests
  ;; Disable the tests that fail on native-comp / emacs-next
  (options->transformation
   '((without-tests . "emacs-eldev")
     (without-tests . "emacs-libgit"))))

(define elisp-packages
  (map without-tests
       (list
        ;; init-ui.el
        emacs-diminish
        emacs-ultra-scroll
        emacs-diff-hl
        emacs-vundo
        emacs-posframe
        emacs-transient-posframe
        emacs-flymake-posframe
        emacs-eldoc-box
        emacs-ligature
        emacs-nerd-icons
        emacs-nerd-icons-dired
        emacs-nerd-icons-completion
        emacs-nerd-icons-corfu

        ;; init-completion.el
        emacs-hotfuzz
        emacs-vertico
        emacs-marginalia
        emacs-consult
        emacs-embark
        emacs-corfu
        emacs-cape

        ;; init-prog.el
        emacs-combobulate
        emacs-tempel
        emacs-aggressive-indent
        emacs-avy
        emacs-expand-region
        emacs-paredit
        emacs-devdocs
        ;; Langs
        emacs-cider
        emacs-sly
        emacs-sly-asdf
        emacs-eros
        emacs-tuareg
        emacs-ocaml-eglot
        emacs-arei
        emacs-fennel-mode
        emacs-web-mode

        ;; init-text.el
        emacs-jinx
        emacs-org-bullets

        ;; init-modes.el
        emacs-elfeed
        emacs-elfeed-org
        emacs-elpher
        emacs-pdf-tools
        emacs-libgit
        emacs-magit
        emacs-vterm
        emacs-pcmpl-args
        emacs-eshell-syntax-highlighting
        emacs-fish-completion

        ;; init-modal.el
        emacs-mwim

        ;; init-frames.el
        emacs-frames-only-mode)))

(define-public packages
  (list
   ripgrep
   enchant
   aspell
   aspell-dict-en
   aspell-dict-sv
   imagemagick                          ; Needed for image-dired
   curl                                 ; For elfeed
   ;; Org mode
   zip
   unzip
   texlive-scheme-basic
   texlive-wrapfig
   texlive-ulem
   texlive-capt-of
   ;; Langs
   binutils                             ; Fixes odd missing 'as' native comp error
   ;; Python
   python
   ;; Common Lisp
   sbcl
   clhs
   ;; Clojure
   ;; FIXME: https://issues.guix.gnu.org/73432
   rlwrap
   ;; (package
   ;;   (inherit clojure-tools)
   ;;   (inputs (modify-inputs (package-inputs clojure-tools)
   ;;             (append java-slf4j-simple))))
   clj-kondo
   clojure-lsp
   `(,openjdk23 "jdk")
   ;; Ocaml
   coreutils
   gnu-make
   sed
   ;; Scheme
   guile-next
   guile-ares-rs
   ;; Rust
   rust
   ;; Libraries for cargo
   gcc-toolchain
   pkg-config
   openssl
   ;; LSP
   ccls
   rust-analyzer
   python-lsp-server
   node
   ;; Treesitter
   tree-sitter-bash
   tree-sitter-c
   tree-sitter-clojure
   tree-sitter-cpp
   tree-sitter-css
   tree-sitter-html
   tree-sitter-lua
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
     (emacs emacs-package)
     (native-comp? #t)
     (elisp-packages elisp-packages)))
   (simple-service 'prog-home-config
                   home-files-service-type
                   `((".local/bin/cc" ,#~(string-append #$gcc-toolchain "/bin/gcc"))
                     (".sbclrc" ,sbcl-config)
                     (".npmrc"  ,(plain-file "npmrc" "prefix=~/.local/lib/npm/\n"))))
   (simple-service 'prog-env-vars
                   home-environment-variables-service-type
                   `(("CC" . ,#~(string-append #$gcc-toolchain "/bin/gcc"))
                     ("SBCL_HOME" . "$HOME/.guix-home/profile/lib/sbcl")
                     ("JAVA_HOME" . ,openjdk23)
                     ("CARGO_HOME" . "$HOME/.local/lib/cargo/")
                     ("XTDB_ENABLE_BYTEUTILS_SHA1" . "true")))))
