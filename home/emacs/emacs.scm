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
  #:use-module ((gnu packages emacs) #:select (emacs-next-tree-sitter))
  #:use-module ((gnu packages enchant) #:select (enchant))
  #:use-module ((gnu packages aspell) #:select (aspell aspell-dict-en aspell-dict-sv))
  #:use-module ((gnu packages fonts) #:select (font-sarasa-gothic))
  #:use-module ((gnu packages compression) #:select (zip unzip))
  #:use-module ((gnu packages base) #:select (binutils))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages lisp) #:select (sbcl))
  #:use-module ((gnu packages clojure) #:select (clojure clojure-tools))
  #:use-module ((gnu packages java) #:select (openjdk))
  #:use-module ((gnu packages cpp) #:select (ccls))
  #:use-module ((gnu packages python-xyz) #:select (python-lsp-server))
  #:use-module ((gnu packages node) #:select (node-lts))
  #:use-module ((contrib packages node-xyz) #:select (node-typescript node-typescript-language-server))
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((system packages nvidia) #:select (replace-mesa))

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

(define emacs-org-block-capf
  (package (name "emacs-org-block-capf")
           (version (git-version "0" "1" "9c1e5c63e38f94238dafeb6bbea312920b6e9901"))
           (home-page "https://github.com/xenodium/org-block-capf")
           (source (origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://github.com/xenodium/org-block-capf")
                          (commit "9c1e5c63e38f94238dafeb6bbea312920b6e9901")))
                    (sha256 (base32 "0bvx693gxvy2f6j6rj9zmvy261cj5lxf0c50yklbcmh26hfibw7a"))))
           (build-system emacs-build-system)
           (synopsis "completion-at-point function for Org Mode blocks")
           (description "This package adds a completion-at-point function for Org Mode blocks")
           ;; FIXME: doesn't actually have a license
           (license gpl3+)))

(define emacs-nnatom
  (package (name "emacs-nnatom")
           (version (git-version "0" "1" "30e4592e39e1d4a484e3bb2496335e43e0478ab7"))
           (home-page "https://git.sr.ht/~dsemy/nnatom")
           (source (origin
                    (method git-fetch)
                    (uri (git-reference
                          (url "https://git.sr.ht/~dsemy/nnatom")
                          (commit "30e4592e39e1d4a484e3bb2496335e43e0478ab7")))
                    (sha256 (base32 "084y4ar15phsjlw8dz0djbx1q92sxkslak94gvmzpwkhkybnc93j"))))
           (build-system emacs-build-system)
           (synopsis "Atom feed backend for Gnus")
           (description "This package adds an atom feed backend for Gnus.")
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

(define sbcl-config (mixed-text-file "sbcl-config" "
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
   emacs-transient-posframe
   emacs-flymake-popon
   emacs-eldoc-box

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
   emacs-org-roam

   ;; init-modes.el
   emacs-nnatom
   emacs-elpher
   emacs-libgit
   emacs-magit
   emacs-vterm
   emacs-pcmpl-args

   ;; init-frames.el
   emacs-frames-only-mode))

(define-public packages
  (list
   enchant
   aspell
   aspell-dict-en
   aspell-dict-sv
   font-sarasa-gothic
   ;; Org mode
   zip
   unzip
   ;; Langs
   binutils                             ; Fixes odd missing 'as' native comp error
   python
   sbcl
   clojure
   clojure-tools
   `(,openjdk "jdk")
   ;; LSP
   ccls
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
     (emacs (if (string= (gethostname) "okarthel")
                (replace-mesa emacs-next-tree-sitter)
                emacs-next-tree-sitter))
     ;; (rebuild-elisp-packages? #t)
     ;; (server-mode? #t)
     (elisp-packages elisp-packages)))
   (simple-service 'sbcl-config
                   home-files-service-type
                   `((".sbclrc" ,sbcl-config)))))
