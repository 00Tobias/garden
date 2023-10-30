(define-module (home stumpwm stumpwm)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix licenses)

  #:use-module ((gnu packages xorg) #:select (xhost xset xsetroot xrandr))
  #:use-module ((gnu packages kde) #:select (kdeconnect))
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages lisp) #:select (sbcl))
  #:use-module (gnu packages lisp-xyz)
  #:use-module ((gnu packages wm) #:select (stumpwm
                                            ;; sbcl-stumpwm-ttf-fonts
                                            sbcl-stumpwm-cpu
                                            sbcl-stumpwm-mem
                                            sbcl-stumpwm-net
                                            sbcl-stumpwm-wifi
                                            sbcl-stumpwm-battery-portable
                                            sbcl-stumpwm-notify
                                            sbcl-stumpwm-pass))

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define xsession
  #~(begin
      (system* #$(file-append xhost "/bin/xhost") "+SI:localuser:tobias")
      (if (string= (gethostname) "okarthel")
          (system* #$(file-append xset "/bin/xset") "s" "off" "-dpms"))
      (system* #$(file-append xsetroot "/bin/xsetroot") "-cursor_name" "left_ptr" "-solid" "black")
      ;; (system* #$(file-append kdeconnect "/libexec/kdeconnectd"))
      (system* #$(file-append dbus "/bin/dbus-run-session") ;; "--exit-with-session"
               #$(file-append stumpwm "/bin/stumpwm"))))


;; https://issues.guix.gnu.org/50274
(define stumpwm-contrib
  (let ((commit "7d1e57ca1e011e0e56059ebf6e833e0976904d2a")
        (revision "5"))
    (package
     (name "stumpwm-contrib")
     (version (git-version "0.0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stumpwm/stumpwm-contrib")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1jap13725f4xnypb8yph2lqn91z6gl5fb2c4ygzj8zwxdvgfan0i"))))
     (build-system copy-build-system)
     (arguments '(#:install-plan
                  '(("media" "media")
                    ("minor-mode" "minor-mode")
                    ("modeline" "modeline")
                    ("util" "util"))))
     ;; (arguments '(#:builder (begin (mkdir %output) #t)))
     (inputs `(("stumpwm" ,stumpwm "lib")))
     (home-page "https://github.com/stumpwm/stumpwm-contrib")
     (synopsis "StumpWM extra modules")
     (description "This package provides extra modules for StumpWM.")
     (license (list gpl2+ gpl3+ bsd-2)))))


(define-public packages
  (list playerctl
        pulsemixer
        xrandr
        xsetroot

        sbcl
        sbcl-slynk
        stumpwm
        ;; stumpwm-contrib

        ;; sbcl-clx
        ;; cl-stumpwm

        ;; notify
        ;; sbcl-bordeaux-threads
        ;; sbcl-dbus
        ;; sbcl-xml-emitter

        sbcl-stumpwm-cpu
        sbcl-stumpwm-mem
        sbcl-stumpwm-net
        sbcl-stumpwm-battery-portable
        sbcl-stumpwm-wifi
        sbcl-stumpwm-notify
        sbcl-stumpwm-pass))

(define-public services
  (list
   (simple-service 'nyxt-config
                   home-files-service-type
                   `((".xsession"            ,(program-file "xsession" xsession))
                     (".xinitrc"             ,(program-file "xinitrc" xsession))
                     (".stumpwm.d/init.lisp" ,(local-file "config/init.lisp"))
                     ;; (".stumpwm.d/contrib"   ,(file-append stumpwm-contrib))
                     )))) ; again, https://issues.guix.gnu.org/50274

