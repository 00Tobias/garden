(define-module (home emacs emacs)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages)
  #:use-module ((gnu packages emacs) #:select (emacs-next-pgtk emacs-next))
  #:use-module ((gnu packages gtk) #:select (gtk+))
  #:use-module ((gnu packages rust-apps) #:select (ripgrep))
  #:use-module ((gnu packages enchant) #:select (enchant))
  #:use-module ((gnu packages aspell) #:select (aspell aspell-dict-en aspell-dict-sv))
  #:use-module ((gnu packages imagemagick) #:select (imagemagick))
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages base) #:select (binutils))
  #:use-module ((gnu packages compression) #:select (zip unzip))
  #:use-module ((gnu packages tex) #:select (texlive-scheme-basic
                                             texlive-wrapfig
                                             texlive-ulem
                                             texlive-capt-of))
  #:use-module ((gnu packages ocaml) #:select (emacs-tuareg))
  #:use-module (gnu packages emacs-xyz)

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (incognita packages emacs-xyz)

  #:use-module (gnu home services)
  #:use-module (rde home services emacs)

  #:use-module ((trowel) #:select (aggressively-optimize))
  #:use-module ((home theme) #:prefix theme:))

(define-public emacs-package
  (cond ((or (string= (gethostname) "austrat")
             (string= (gethostname) "okarthel"))
         ;; NOTE: Emacs on the default Guix version of gtk+ segfaults when closing a frame on gdk_window_get_screen. Updating gtk seems to fix the issue.
         (replace-mesa
          (package
            (inherit emacs-next-pgtk)
            (inputs (modify-inputs (package-inputs emacs-next-pgtk)
                      (replace "gtk+"
                        (package
                          (inherit gtk+)
                          (name "gtk+")
                          (version "3.24.52")
                          (source (origin
                                    (method git-fetch)
                                    (uri (git-reference
                                          (url "https://gitlab.gnome.org/GNOME/gtk")
                                          (commit "f1dbc95ebbeed771621bddebc223352a99854a93")))
                                    (file-name (git-file-name name version))
                                    (sha256 (base32 "1ykmar93ll6nlpzi0s3a1kxrkqxkbzg7y6qbr5yaw8742bpibcn2"))
                                    (patches (search-patches
                                              "gtk3-respect-GUIX_GTK3_PATH.patch"
                                              "gtk3-respect-GUIX_GTK3_IM_MODULE_FILE.patch"))))
                          (arguments
                           (substitute-keyword-arguments (package-arguments gtk+)
                             ((#:tests? _ #f) #f)
                             ((#:phases phases)
                              #~(modify-phases #$phases
                                  (delete 'disable-failing-tests))))))))))))
        (else (emacs-next))))

(define without-tests
  ;; Disable the tests that fail on native-comp / emacs-next
  (options->transformation
   '((without-tests . "emacs-eldev")
     (without-tests . "emacs-libgit")
     (without-tests . "emacs-dash"))))

(define elisp-packages
  (map without-tests
       (list
        ;; init-ui.el
        emacs-diminish
        emacs-ultra-scroll
        emacs-diff-hl
        emacs-vundo
        emacs-posframe
        emacs-which-key-posframe
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
        emacs-minuet
        ;; Langs
        emacs-cider
        emacs-sly
        emacs-sly-asdf
        emacs-eros
        emacs-tuareg
        emacs-ocaml-eglot
        emacs-cargo-transient
        emacs-arei
        emacs-fennel-mode
        emacs-reformatter
        emacs-uiua-mode
        emacs-uiua-ts-mode
        emacs-web-mode

        ;; init-text.el
        emacs-jinx
        emacs-org-bullets
        emacs-markdown-mode

        ;; init-modes.el
        emacs-elfeed
        emacs-elfeed-org
        emacs-elpher
        emacs-pdf-tools
        emacs-libgit
        emacs-magit
        emacs-pinentry
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
   binutils                             ; Fixes odd missing 'as' native comp error
   ;; Org mode
   zip
   unzip
   texlive-scheme-basic
   texlive-wrapfig
   texlive-ulem
   texlive-capt-of))

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
     (elisp-packages elisp-packages)))))
