(define-module (home core)
  #:use-module (guix transformations)
  #:use-module (gnu home)

  #:use-module ((gnu packages video) #:select (mpv))
  #:use-module ((gnu packages vpn) #:select (wireguard-tools))
  #:use-module ((gnu packages xdisorg) #:select (rxvt-unicode))
  #:use-module ((gnu packages rust-apps) #:select (rbw))
  #:use-module ((gnu packages finance) #:select (xmrig))
  #:use-module ((gnu packages admin) #:select (htop))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages gnuzilla) #:select (icecat))
  #:use-module ((gnu packages gnupg) #:select (pinentry))
  #:use-module ((gnu packages password-utils) #:select (password-store))
  #:use-module ((gnu packages xdisorg) #:select (xclip))
  #:use-module ((gnu packages python) #:select (python))

  #:use-module ((gnu packages fonts) #:select (font-google-noto
                                               font-google-noto-sans-cjk
                                               font-google-noto-emoji))

  #:use-module ((gnu packages games) #:select (quakespasm
                                               yamagi-quake2))
  #:use-module ((gnu packages game-development) #:select (ioquake3))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type))
  #:use-module (home services pipewire)

  #:use-module ((home creative) #:prefix creative:)
  #:use-module ((home bash) #:prefix bash:)
  #:use-module ((home xorg bspwm) #:prefix bspwm:)
  #:use-module ((home xorg dunst) #:prefix dunst:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home nyxt nyxt) #:prefix nyxt:)
  #:use-module ((home qutebrowser qutebrowser) #:prefix qutebrowser:)
  #:use-module ((home mpv mpv) #:prefix mpv:))

;; TODO: rename to main.scm
;; TODO: Yubikey/GPG
(home-environment
 (packages (append
            creative:packages
            bspwm:packages
            emacs:packages
            nyxt:packages
            qutebrowser:packages
            mpv:packages
            ;; TODO: Clean this
            (list icecat)               ; Doesn't like replace-mesa
            (map replace-mesa
                 (list rbw
                       pinentry
                       password-store
                       xmrig
                       wireguard-tools
                       rxvt-unicode
                       htop
                       pulsemixer
                       xclip
                       python

                       ;; Fonts
                       font-google-noto
                       font-google-noto-sans-cjk
                       font-google-noto-emoji

                       ;; Games
                       quakespasm
                       yamagi-quake2
                       ioquake3
                       ;; TODO: Dwarf Fortress
                       ))))

 (services
  (append
   bash:services
   bspwm:services
   dunst:services
   emacs:services
   nyxt:services
   mpv:services
   (list (simple-service 'env-vars-service
                         home-environment-variables-service-type
                         `(("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share")
                           ("SBCL_HOME"    . "$HOME/.guix-home/profile/lib/sbcl")
                           ("LESS" . "-R --use-color -Dd+r$Du+b")
                           ("XTDB_ENABLE_BYTEUTILS_SHA1" . "true"))) ; NOTE: Temp
         (service home-dbus-service-type)
         (service home-pipewire-service-type)))))
