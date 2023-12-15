(define-module (home core)
  #:use-module (guix transformations)
  #:use-module (gnu home)

  #:use-module ((gnu packages vpn) #:select (wireguard-tools))
  #:use-module ((gnu packages admin) #:select (htop))
  #:use-module ((gnu packages gnupg) #:select (pinentry))
  #:use-module ((gnu packages password-utils) #:select (password-store))
  #:use-module ((gnu packages gnuzilla) #:select (icecat))

  #:use-module ((gnu packages fonts) #:select (font-google-noto
                                               font-google-noto-sans-cjk
                                               font-google-noto-emoji))

  #:use-module ((gnu packages games) #:select (quakespasm
                                               yamagi-quake2))
  #:use-module ((gnu packages game-development) #:select (ioquake3))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module ((nongnu packages mozilla) #:select (firefox))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type))
  #:use-module (rde home services desktop)
  #:use-module (home services pipewire)

  #:use-module ((home creative) #:prefix creative:)
  #:use-module ((home bash) #:prefix bash:)
  #:use-module ((home gtk) #:prefix gtk:)
  #:use-module ((home xorg xresources) #:prefix xresources:)
  #:use-module ((home xorg bspwm) #:prefix bspwm:)
  #:use-module ((home xorg dunst) #:prefix dunst:)
  #:use-module ((home xorg polybar) #:prefix polybar:)
  #:use-module ((home xorg rofi) #:prefix rofi:)
  #:use-module ((home wayland sway) #:prefix sway:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home nyxt nyxt) #:prefix nyxt:)
  #:use-module ((home qutebrowser qutebrowser) #:prefix qutebrowser:)
  #:use-module ((home mpv mpv) #:prefix mpv:))

;; TODO: rename to main.scm
;; TODO: Yubikey/GPG
(home-environment
 (packages (append
            creative:packages
            gtk:packages
            sway:packages
            xresources:packages
            bspwm:packages
            rofi:packages
            emacs:packages
            nyxt:packages
            qutebrowser:packages
            mpv:packages
            ;; TODO: Clean this
            (list icecat)               ; Doesn't like replace-mesa
            (let ((lst (list
                        pinentry
                        password-store
                        wireguard-tools
                        htop
                        firefox

                        ;; Fonts
                        font-google-noto
                        font-google-noto-sans-cjk
                        font-google-noto-emoji

                        ;; Games
                        quakespasm
                        yamagi-quake2
                        ioquake3)))
              (if (string= (gethostname) "okarthel")
                  (map replace-mesa lst)
                  lst))))

 (services
  (append
   bash:services
   gtk:services
   xresources:services
   bspwm:services
   dunst:services
   polybar:services
   rofi:services
   sway:services
   emacs:services
   nyxt:services
   mpv:services
   (list (simple-service 'env-vars-service
                         home-environment-variables-service-type
                         `(;; ("GUIX_LOCPATH" . "$HOME/.guix-profile/lib/locale")
                           ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share")
                           ("SBCL_HOME"    . "$HOME/.guix-home/profile/lib/sbcl")
                           ("LESS" . "-R --use-color -Dd+r$Du+b")
                           ("TERM" . "xterm-256color")
                           ("XTDB_ENABLE_BYTEUTILS_SHA1" . "true"))) ; NOTE: Temp
         (service home-dbus-service-type)
         (service home-pipewire-service-type)
         (service home-udiskie-service-type
                  (home-udiskie-configuration
                   (config '((notify . #f)))))))))
