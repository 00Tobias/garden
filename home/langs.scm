(define-module (home langs)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:)

  ;; C
  #:use-module ((gnu packages llvm) #:select (clang))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  ;; Clojure
  #:use-module ((gnu packages readline) #:select (rlwrap))
  #:use-module ((gnu packages clojure) #:select (clojure clojure-tools))
  #:use-module ((gnu packages java) #:select (openjdk23 icedtea java-slf4j-simple))
  #:use-module ((nongnu packages clojure) #:select (clj-kondo clojure-lsp))
  ;; Common Lisp
  #:use-module ((gnu packages lisp) #:select (sbcl))
  #:use-module ((gnu packages lisp-xyz) #:select (sbcl-cffi))
  #:use-module ((nongnu packages emacs) #:select (clhs))
  ;; Fennel
  #:use-module ((gnu packages lua) #:select (fennel))
  ;; JS
  #:use-module ((gnu packages node) #:select (node))
  ;; OCaml
  #:use-module ((gnu packages base) #:select (binutils coreutils gnu-make sed))
  ;; Python
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-xyz) #:select (python-lsp-server))
  ;; Rust
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages tls) #:select (openssl))
  ;; Scheme
  #:use-module ((gnu packages guile) #:select (guile-next))
  #:use-module ((gnu packages guile-xyz) #:select (guile-ares-rs))
  ;; Zig
  #:use-module ((gnu packages zig) #:select (zig-0.15))
  ;; Treesitter
  #:use-module (gnu packages tree-sitter)

  #:use-module (gnu services)

  #:use-module (gnu home services))

(define-public font-uiua386
  (package
    (name "font-uiua386")
    (version "1.0.0")
    (source (local-file "fonts/Uiua386.ttf"))
    (build-system font-build-system)
    (home-page "https://www.uiua.org/")
    (synopsis "Font for Uiua")
    (description "")
    (license license:x11)))             ; MIT

(define tree-sitter-grammar
  (@@ (gnu packages tree-sitter) tree-sitter-grammar))

(define tree-sitter-uiua
  (tree-sitter-grammar
   "uiua" "Uiua"
   "1pwhdsvdi6p70r9iij3mqnpdl0m2vz242l2qxlanplfcasf58sf9"
   "0.11.0"
   #:repository-url "https://github.com/shnarazk/tree-sitter-uiua"))

(define common-lisp-config (mixed-text-file "common-lisp-config" "
(require \"asdf\")
(when (ignore-errors (asdf:load-system \"cffi\"))
  (dolist (dir (list
                (format nil \"~a/.guix-profile/lib/\"      (uiop:getenv \"HOME\"))
                (format nil \"~a/.guix-home/profile/lib/\" (uiop:getenv \"HOME\"))
                (format nil \"~a/lib/\"                    (uiop:getenv \"GUIX_ENVIRONMENT\"))))
    (when (probe-file dir)
      (push dir
            (symbol-value (find-symbol \"*FOREIGN-LIBRARY-DIRECTORIES*\"
                                       (find-package 'cffi)))))))

(load \"~/quicklisp/setup.lisp\")
"))

(define guile-geiser-config (mixed-text-file "guile-geiser-config" "
(use-modules (guix gexp)
             (guix utils)
             (guix transformations)
             (guix packages)
             (guix download)
             (guix git-download)

             (gnu packages)

             (gnu services)
             (gnu home services))
"))

(define ocaml-config (mixed-text-file "ocaml-config" "
#require \"core.top\";;
#require \"ppx_jane\";;
open Base;;
"))

(define-public packages
  (list
   ;; C
   clang
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
   ;; Common Lisp
   sbcl
   sbcl-cffi
   clhs
   ;; Fennel
   fennel
   ;; JS
   node                 ; npm install -g typescript-language-server typescript
   ;; OCaml
   coreutils
   gnu-make
   sed
   ;; Python
   python
   python-lsp-server
   ;; Rust
   rust
   `(,rust "cargo")
   `(,rust "tools")
   ;; Libraries for cargo
   gcc-toolchain
   pkg-config
   openssl
   ;; Scheme
   guile-next
   guile-ares-rs
   ;; Uiua
   font-uiua386
   ;; Zig
   zig-0.15

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
   tree-sitter-typescript
   tree-sitter-uiua))

(define-public services
  (list
   (simple-service 'prog-home-config
                   home-files-service-type
                   `((".local/bin/cc"  ,#~(string-append #$gcc-toolchain "/bin/gcc"))
                     (".sbclrc"        ,common-lisp-config)
                     (".ccl-init.lisp" ,common-lisp-config)
                     (".guile-geiser"  ,guile-geiser-config)
                     (".ocamlinit"     ,ocaml-config)
                     (".npmrc"         ,(plain-file "npmrc" "prefix=~/.local/lib/npm/\n"))))
   (simple-service 'prog-env-vars
                   home-environment-variables-service-type
                   `(("CC" . ,#~(string-append #$gcc-toolchain "/bin/gcc"))
                     ("SBCL_HOME" . "$HOME/.guix-home/profile/lib/sbcl")
                     ("JAVA_HOME" . ,openjdk23)
                     ("CARGO_HOME" . "$HOME/.local/lib/cargo/")
                     ("XTDB_ENABLE_BYTEUTILS_SHA1" . "true")))))
