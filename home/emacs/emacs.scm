(define-module (home emacs emacs)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module ((gnu packages emacs) #:select (emacs-next))
  #:use-module ((gnu packages aspell) #:select (aspell aspell-dict-sv))
  #:use-module ((gnu packages fonts) #:select (font-hack))
  #:use-module ((gnu packages xorg) #:select (xhost xsetroot))
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module (gnu packages emacs-xyz)

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils))

(define xsession
  #~(begin
      (system* #$(file-append xhost "/bin/xhost") "+SI:localuser:tobias")
      (system* #$(file-append xsetroot "/bin/xsetroot") "-cursor_name" "left_ptr")
      (system* #$(file-append dbus "/bin/dbus-launch") "--exit-with-session"
               #$(file-append emacs-next "/bin/emacs") "-l" "~/.emacs.d/exwm.el")))

(define transform
  (options->transformation
   '((without-tests . "emacs-magit"))))

(define elisp-packages
  (map transform
       (list
        emacs-diminish

        ;; major modes
        emacs-guix
        emacs-magit
        emacs-elpher
        emacs-mastodon
        emacs-vterm

        ;; exwm
        emacs-exwm
        emacs-exwm-edit
        emacs-ednc
        emacs-dmenu
        emacs-windower     ; TODO: Move this to init-ui with hyper key

        ;; init-ui
        emacs-paren-face
        emacs-ace-window
        emacs-hl-todo
        emacs-diff-hl
        emacs-which-key

        ;; init-completion
        emacs-orderless
        emacs-vertico
        emacs-marginalia
        emacs-consult
        emacs-embark
        ;; emacs-embark-consult
        emacs-corfu
        emacs-kind-icon

        ;; init-prog
        emacs-lispy
        emacs-yasnippet
        emacs-yasnippet-snippets
        emacs-aggressive-indent
        emacs-expand-region
        emacs-avy
        ;; languages
        emacs-cider
        emacs-sly
        emacs-geiser
        emacs-geiser-guile

        ;; init-eshell
        emacs-pcmpl-args)))

(define-public packages
  (list
   aspell
   aspell-dict-sv
   font-hack
   git))

(define-public services
  (list
   (simple-service 'emacs-config
                   home-files-service-type
                   `(;; (".xsession"                        ,(program-file "xsession" xsession))
                     ;; (".emacs.d/exwm.el"                 ,(local-file "config/exwm.el"))
                     (".emacs.d/init.el"                 ,(local-file "config/init.el"))
                     (".emacs.d/early-init.el"           ,(local-file "config/early-init.el"))
                     (".emacs.d/lisp/init-ui.el"         ,(local-file "config/lisp/init-ui.el"))
                     (".emacs.d/lisp/init-completion.el" ,(local-file "config/lisp/init-completion.el"))
                     (".emacs.d/lisp/init-prog.el"       ,(local-file "config/lisp/init-prog.el"))
                     (".emacs.d/lisp/init-text.el"       ,(local-file "config/lisp/init-text.el"))
                     (".emacs.d/lisp/init-eshell.el"     ,(local-file "config/lisp/init-eshell.el"))
                     (".emacs.d/lisp/init-colemak.el"    ,(local-file "config/lisp/init-colemak.el"))))
   (service
    home-emacs-service-type
    (home-emacs-configuration
     (package emacs-next)
     ;; (rebuild-elisp-packages? #t)
     (server-mode? #t)
     (elisp-packages elisp-packages)))))
