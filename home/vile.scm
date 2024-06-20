(define-module (home vile)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (nonguix build-system binary)

  #:use-module ((gnu packages java) #:select (icedtea))
  #:use-module ((gnu packages qt) #:select (qtbase
                                            qt5compat
                                            qtnetworkauth))
  #:use-module ((gnu packages libusb) #:select (libusb))
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module ((gnu packages linux) #:select (pipewire))

  #:use-module ((nongnu packages chrome) #:select (google-chrome-stable))
  #:use-module ((nongnu packages game-client) #:select (steam steam-nvidia protonup-ng))

  #:use-module ((games packages minecraft) #:select (prismlauncher))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define prismlauncher-latest
  (package
    (inherit prismlauncher)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PrismLauncher/PrismLauncher")
                    (recursive? #t)
                    (commit "0ecdceccd2ea7432ae6810a7e41afe848ccc452f")))
              (sha256 (base32 "1vfpyb4vrl7h3ms2bjgf2xpzmjzkm4y8vddryldbcrfpsk8rjwql"))))
    (propagated-inputs (list `(,icedtea "jdk")))
    (inputs
     (modify-inputs (package-inputs prismlauncher)
       (replace "qtbase" qtbase)
       (prepend qt5compat qtnetworkauth libusb)))))

(define-public virtmic
  (package
   (name "virtmic")
   (version "0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/edisionnano/Screenshare-with-audio-on-Discord-with-Linux")
           (commit "a10c99729ca11a5dc246b913d2f437cba52774e0")))
     (sha256 (base32 "1b96zlcqgmfc6nsp2n2wy2s5iapqlf3zf7maz86dgn9zf8ikv8sv"))))
   (build-system binary-build-system)
   (inputs (list `(,gcc "lib") pipewire))
   (arguments
    '(#:patchelf-plan '(("virtmic" ("libc" "gcc" "pipewire")))
      #:install-plan '(("virtmic" "bin/virtmic"))
      #:phases
      (modify-phases %standard-phases
                     (add-after 'install 'make-executable
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (binary (string-append out "/bin/virtmic")))
                                    (chmod binary #o755)))))))
   (home-page "https://github.com/edisionnano/Screenshare-with-audio-on-Discord-with-Linux")
   (synopsis "Screenshare with audio on Discord with Linux")
   (description "Screenshare with audio on Discord with Linux.")
   (license license:expat)))

(define discord-desktop-entry (mixed-text-file "discord-desktop-entry" "
[Desktop Entry]
Name=Discord (Chrome)
Exec=" (if (string= (gethostname) "okarthel")
           (replace-mesa google-chrome-stable)
           google-chrome-stable) "/bin/google-chrome --app=https://canary.discord.com/channels/@me --new-window "
           (if (string= (gethostname) "austrat")
               "--ozone-platform-hint=auto"
               "")
           " --process-per-site --enable-features=WebRTCPipeWireCapturer,TouchpadOverscrollHistoryNavigation
Type=Application
Terminal=false"))

(define spotify-desktop-entry (mixed-text-file "spotify-desktop-entry" "
[Desktop Entry]
Name=Spotify (Chrome)
Exec=" (if (string= (gethostname) "okarthel")
           (replace-mesa google-chrome-stable)
           google-chrome-stable) "/bin/google-chrome --app=https://open.spotify.com/ --new-window "
           (if (string= (gethostname) "austrat")
               "--ozone-platform-hint=auto"
               "")
           " --process-per-site --enable-features=WebRTCPipeWireCapturer,TouchpadOverscrollHistoryNavigation
Type=Application
Terminal=false"))

(define-public packages
  (let ((lst (list
              google-chrome-stable
              virtmic
              (if (or (string= (gethostname) "okarthel")
                      (string= (gethostname) "austrat"))
                  steam-nvidia
                  steam)
              protonup-ng
              prismlauncher-latest)))
    (if (or (string= (gethostname) "okarthel")
            (string= (gethostname) "austrat"))
        (map replace-mesa lst)
        lst)))

(define-public services
  (append
   (list
    (simple-service 'vile-desktop-entries
                    home-xdg-data-files-service-type
                    `(("applications/discord.desktop" ,discord-desktop-entry)
                      ("applications/spotify.desktop" ,spotify-desktop-entry))))
   (if (string= (gethostname) "okarthel")
       (list
        (simple-service 'okarthel-vile-env-vars
                        home-environment-variables-service-type
                        `(("GUIX_SANDBOX_EXTRA_SHARES" . "/bulk/games"))))
       '())))
