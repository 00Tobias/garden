(define-module (home core)
  #:use-module (gnu home)

  #:use-module ((gnu packages video) #:select (mpv))
  ;; #:use-module ((gnu packages chromium) #:select (ungoogled-chromium))
  ;; #:use-module ((gnu packages browser-extensions) #:select (ublock-origin/chromium))

  #:use-module ((home bash) #:prefix bash:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home nyxt nyxt) #:prefix nyxt:))

(home-environment
 (packages (append
            emacs:packages
            nyxt:packages
            (list mpv)))
 (services
  (append
   bash:services
   emacs:services
   nyxt:services)))
