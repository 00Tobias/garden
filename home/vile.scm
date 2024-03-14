(define-module (home vile)
  #:use-module (guix gexp)

  #:use-module ((nongnu packages chrome) #:select (google-chrome-stable))
  #:use-module ((nongnu packages game-client) #:select (steam steam-nvidia protonup-ng))

  #:use-module ((games packages minecraft) #:select (prismlauncher))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services))

;; --ozone-platform-hint=auto
(define discord-desktop-entry (mixed-text-file "discord-desktop-entry" "
[Desktop Entry]
Name=Discord (Chrome)
Exec=" (if (string= (gethostname) "okarthel")
           (replace-mesa google-chrome-stable)
           google-chrome-stable) "/bin/google-chrome --app=https://canary.discord.com/channels/@me --new-window --enable-features=WebRTCPipeWireCapturer,TouchpadOverscrollHistoryNavigation
Type=Application
Terminal=false"))

(define spotify-desktop-entry (mixed-text-file "spotify-desktop-entry" "
[Desktop Entry]
Name=Spotify (Chrome)
Exec=" (if (string= (gethostname) "okarthel")
           (replace-mesa google-chrome-stable)
           google-chrome-stable) "/bin/google-chrome --app=https://open.spotify.com/ --new-window  --enable-features=WebRTCPipeWireCapturer,TouchpadOverscrollHistoryNavigation
Type=Application
Terminal=false"))

(define-public packages
  (let ((lst (list
              google-chrome-stable
              (if (string= (gethostname) "okarthel")
                  steam-nvidia
                  steam)
              protonup-ng
              prismlauncher)))
    (if (string= (gethostname) "okarthel")
        (map replace-mesa lst)
        lst)))

(define-public services
  (list
   (simple-service 'desktop-entries
                   home-xdg-data-files-service-type
                   `(("applications/discord.desktop" ,discord-desktop-entry)
                     ("applications/spotify.desktop" ,spotify-desktop-entry)))))
