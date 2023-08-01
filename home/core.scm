(define-module (home core)
  #:use-module (gnu home)

  #:use-module ((gnu packages video) #:select (mpv))
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module ((gnu packages freedesktop) #:select (xdg-desktop-portal-gtk))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((home bash) #:prefix bash:)
  #:use-module ((home stumpwm stumpwm) #:prefix stumpwm:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home nyxt nyxt) #:prefix nyxt:))

(home-environment
 (packages (append
            stumpwm:packages
            emacs:packages
            nyxt:packages
            (list rxvt-unicode mpv flatpak xdg-desktop-portal-gtk)))
 ;; guix shell --check --with-graft=mesa=nvda flatpak -- flatpak run com.valvesoftware.Steam
 (services
  (append
   (list (simple-service 'env-vars-service
                         home-environment-variables-service-type
                         `(("SBCL_HOME"    . "$HOME/.guix-home/profile/lib/sbcl")
                           ("LESS" . "-R --use-color -Dd+r$Du+b"))))
   bash:services
   stumpwm:services
   emacs:services
   nyxt:services)))
