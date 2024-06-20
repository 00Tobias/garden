(define-module (home emacs emacs)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages cmake) #:select (cmake-minimal))
  #:use-module ((guix licenses) #:select (gpl3+))

  #:use-module (gnu packages)
  #:use-module ((gnu packages emacs) #:select (emacs-next emacs-next-pgtk))
  #:use-module ((gnu packages rust-apps) #:select (ripgrep))
  #:use-module ((gnu packages enchant) #:select (enchant))
  #:use-module ((gnu packages aspell) #:select (aspell aspell-dict-en aspell-dict-sv))
  #:use-module ((gnu packages fonts) #:select (font-sarasa-gothic))
  #:use-module ((gnu packages compression) #:select (zip unzip))
  #:use-module ((gnu packages tex) #:select (texlive-scheme-basic
                                             texlive-wrapfig
                                             texlive-ulem
                                             texlive-capt-of))
  #:use-module ((gnu packages base) #:select (binutils))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages lisp) #:select (sbcl))
  #:use-module ((gnu packages clojure) #:select (clojure clojure-tools))
  #:use-module ((gnu packages java) #:select (openjdk21))
  #:use-module ((gnu packages cpp) #:select (ccls))
  #:use-module ((gnu packages python-xyz) #:select (python-lsp-server))
  #:use-module ((gnu packages rust) #:select (rust rust-analyzer))
  #:use-module ((gnu packages rust-apps) #:select (rust-cargo))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module ((gnu packages node) #:select (node-lts))
  #:use-module ((contrib packages node-xyz) #:select (node-typescript node-typescript-language-server))
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((rde packages fonts) #:select (font-iosevka-nerd))

  #:use-module ((nongnu packages emacs) #:select (clhs))
  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services emacs)
  #:use-module (gnu home-services-utils)

  #:use-module ((trowel) #:select (aggressively-optimize)))

(define-public emacs-package
  (cond ((string= (gethostname) "okarthel")
         (replace-mesa (aggressively-optimize emacs-next)))
        ((string= (gethostname) "austrat")
         (aggressively-optimize emacs-next-pgtk))
        (else (emacs-next))))

(define emacs-nerd-icons-completion
  (let ((commit "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))
    (package
      (name "emacs-nerd-icons-completion")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/rainstormstudio/nerd-icons-completion")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rainstormstudio/nerd-icons-completion")
                      (commit commit)))
                (sha256 (base32 "10ll0dj6ym5prrkv6smj0ac2ail4b3rqcrh1lyr61y3cj422vn9z"))))
      (inputs (list emacs-nerd-icons))
      (build-system emacs-build-system)
      (synopsis "Use nerd-icons for completion")
      (description "Use nerd-icons for completion.")
      (license gpl3+))))

(define emacs-nerd-icons-dired
  (let ((commit "c1c73488630cc1d19ce1677359f614122ae4c1b9"))
    (package
      (name "emacs-nerd-icons-dired")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/rainstormstudio/nerd-icons-dired")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rainstormstudio/nerd-icons-dired")
                      (commit commit)))
                (sha256 (base32 "1ln73ii7c3chl4lvarwiwrdmx49q528wc0h6a7xbl68pc2pyyvq2"))))
      (inputs (list emacs-nerd-icons))
      (build-system emacs-build-system)
      (synopsis "Use nerd-icons for Dired")
      (description "Use nerd-icons for Dired.")
      (license gpl3+))))

(define emacs-nerd-icons-corfu
  (let ((commit "7077bb76fefc15aed967476406a19dc5c2500b3c"))
    (package
      (name "emacs-nerd-icons-corfu")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/LuigiPiucco/nerd-icons-corfu")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/LuigiPiucco/nerd-icons-corfu")
                      (commit commit)))
                (sha256 (base32 "13m20k242zma6jw7pkbw89fk3dnbkwdajcpiyay5xx2l9241snb7"))))
      (inputs (list emacs-nerd-icons))
      (build-system emacs-build-system)
      (synopsis "Icons for corfu via nerd-icons")
      (description "Nerd-icons-corfu.el is a library for adding icons to completions in Corfu.")
      (license gpl3+))))

(define emacs-org-block-capf
  (let ((commit "9c1e5c63e38f94238dafeb6bbea312920b6e9901"))
    (package
      (name "emacs-org-block-capf")
      (version (git-version "0" "1" commit))
      (home-page "https://github.com/xenodium/org-block-capf")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/xenodium/org-block-capf")
                      (commit commit)))
                (sha256 (base32 "0bvx693gxvy2f6j6rj9zmvy261cj5lxf0c50yklbcmh26hfibw7a"))))
      (build-system emacs-build-system)
      (synopsis "completion-at-point function for Org Mode blocks")
      (description "This package adds a completion-at-point function for Org Mode blocks")
      ;; FIXME: doesn't actually have a license
      (license gpl3+))))

(define emacs-hotfuzz
  (let ((commit "622329477d893a9fc2528a75935cfe1f8614f4bc"))
    (package
      (name "emacs-hotfuzz")
      (version (git-version "0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/axelf4/hotfuzz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 "1ax98352dl7mbgz7xphdj5xwxxxpmmnvhysic4ccpmrkgim1y7s4"))))
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

(define sbcl-config (plain-file "sbcl-config" "
(require \"asdf\")
(let ((guix-profile (format nil \"~a/.guix-profile/lib/\" (uiop:getenv \"HOME\")))
      (guix-home (format nil \"~a/.guix-home/profile/lib/\" (uiop:getenv \"HOME\"))))
  (when (and (probe-file guix-profile)
             (probe-file guix-home)
             (ignore-errors (asdf:load-system \"cffi\")))
    (push guix-profile
          (symbol-value (find-symbol (string '*foreign-library-directories*)
                                     (find-package 'cffi))))
    (push guix-home
          (symbol-value (find-symbol (string '*foreign-library-directories*)
                                     (find-package 'cffi))))))
"))

(define elisp-packages
  (list
   ;; init-ui.el
   emacs-diff-hl
   emacs-vundo
   emacs-svg-tag-mode
   emacs-hide-lines
   emacs-transient-posframe
   emacs-flymake-popon
   emacs-eldoc-box
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
   emacs-kind-icon

   ;; init-prog.el
   emacs-combobulate
   emacs-tempel
   emacs-llm
   emacs-ellama
   emacs-aggressive-indent
   emacs-avy
   emacs-expand-region
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
   emacs-jinx
   emacs-org-block-capf
   emacs-org-bullets

   ;; init-modes.el
   emacs-elpher
   emacs-libgit
   emacs-magit
   emacs-vterm
   emacs-pcmpl-args

   ;; init-frames.el
   emacs-frames-only-mode))

(define-public packages
  (list
   ripgrep
   enchant
   aspell
   aspell-dict-en
   aspell-dict-sv
   font-sarasa-gothic
   font-iosevka-nerd
   ;; Org mode
   zip
   unzip
   texlive-scheme-basic
   texlive-wrapfig
   texlive-ulem
   texlive-capt-of
   ;; Langs
   binutils                             ; Fixes odd missing 'as' native comp error
   python
   sbcl
   clhs
   clojure
   clojure-tools
   `(,openjdk21 "jdk")
   rust
   rust-cargo
   ;; Libraries for cargo
   gcc-toolchain
   pkg-config
   openssl
   ;; LSP
   ccls
   rust-analyzer
   python-lsp-server
   node-lts                             ; Normal is at v10?
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
     (emacs emacs-package)
     (native-comp? #t)
     (elisp-packages elisp-packages)))
   (simple-service 'prog-config
                   home-files-service-type
                   `((".sbclrc" ,sbcl-config)
                     (".npmrc"  ,(plain-file "npmrc" "prefix=~/.local/lib/npm/\n"))))
   (simple-service 'prog-env-vars
                   home-environment-variables-service-type
                   `(("CC" . ,#~(string-append #$gcc-toolchain "/bin/gcc"))
                     ("SBCL_HOME" . "$HOME/.guix-home/profile/lib/sbcl")
                     ("CARGO_HOME" . "$HOME/.local/lib/cargo/")
                     ("XTDB_ENABLE_BYTEUTILS_SHA1" . "true")))))
