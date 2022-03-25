(define-module (home core)
  #:use-module (gnu home)

  ;; This is very temp
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages browser-extensions)

  #:use-module ((home bash) #:prefix bash:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home nyxt nyxt) #:prefix nyxt:)
  #:use-module ((home xorg bspwm) #:prefix bspwm:)
  #:use-module ((home xorg dunst) #:prefix dunst:))

(home-environment
 (packages (append
            emacs:packages
            nyxt:packages
            bspwm:packages
            (list kakoune ungoogled-chromium ublock-origin/chromium)))
 (services
  (append
   bash:services
   emacs:services
   nyxt:services
   bspwm:services
   dunst:services)))
