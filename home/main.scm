(define-module (home main)
  #:use-module (guix gexp)
  #:use-module (guix channels)

  #:use-module ((gnu packages vpn) #:select (wireguard-tools))
  #:use-module ((gnu packages admin) #:select (htop))
  #:use-module ((gnu packages gnupg) #:select (pinentry))
  #:use-module ((gnu packages password-utils) #:select (password-store))
  #:use-module ((gnu packages pdf) #:select (zathura zathura-pdf-mupdf))

  #:use-module ((gnu packages fonts) #:select (font-google-noto
                                               font-google-noto-sans-cjk
                                               font-google-noto-emoji))

  #:use-module ((nongnu packages mozilla) #:select (firefox))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type))
  #:use-module ((gnu home services sound) #:select (home-pipewire-service-type))
  #:use-module (rde home services desktop)

  #:use-module ((home bash) #:prefix bash:)
  #:use-module ((home gtk) #:prefix gtk:)
  #:use-module ((home xorg xresources) #:prefix xresources:)
  #:use-module ((home xorg i3) #:prefix i3:)
  #:use-module ((home xorg dunst) #:prefix dunst:)
  #:use-module ((home wayland sway) #:prefix sway:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home nyxt nyxt) #:prefix nyxt:)
  #:use-module ((home qutebrowser qutebrowser) #:prefix qutebrowser:)
  #:use-module ((home mpv mpv) #:prefix mpv:)
  #:use-module ((home creative) #:prefix creative:)
  ;; #:use-module ((home ai) #:prefix ai:)
  ;; #:use-module ((home games) #:prefix games:)
  #:use-module ((home vile) #:prefix vile:))

(define-public main-home
  (home-environment
   (packages (append
              gtk:packages
              xresources:packages
              i3:packages
              emacs:packages
              nyxt:packages
              qutebrowser:packages
              mpv:packages
              creative:packages
              ;; ai:packages
              ;; games:packages
              vile:packages
              (let ((lst (list
                          pinentry
                          password-store
                          wireguard-tools
                          htop
                          zathura
                          zathura-pdf-mupdf
                          firefox

                          ;; Fonts
                          font-google-noto
                          font-google-noto-sans-cjk
                          font-google-noto-emoji)))
                (if (string= (gethostname) "okarthel")
                    (map replace-mesa lst)
                    lst))))

   (services
    (append
     bash:services
     gtk:services
     xresources:services
     i3:services
     dunst:services
     emacs:services
     nyxt:services
     qutebrowser:services
     mpv:services
     vile:services
     (list
      (service home-dbus-service-type)
      (service home-pipewire-service-type)
      (simple-service 'git-config home-files-service-type
                      `((".gitconfig"  ,(plain-file "gitconfig" "
[user]
	email = tobias@nights.rest
	name = tobias
"))))
      (simple-service 'channels home-channels-service-type
                      (list
                       (channel
                        (name 'nonguix)
                        (url "https://gitlab.com/nonguix/nonguix")
                        (introduction
                         (make-channel-introduction
                          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                          (openpgp-fingerprint
                           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
                       (channel
                        (name 'guix-science-nonfree)
                        (url "https://github.com/guix-science/guix-science-nonfree.git")
                        (introduction
                         (make-channel-introduction
                          "58661b110325fd5d9b40e6f0177cc486a615817e"
                          (openpgp-fingerprint
                           "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
                       (channel
                        (name 'guix-gaming-games)
                        (url "https://gitlab.com/guix-gaming-channels/games.git")
                        (introduction
                         (make-channel-introduction
                          "c23d64f1b8cc086659f8781b27ab6c7314c5cca5"
                          (openpgp-fingerprint
                           "50F3 3E2E 5B0C 3D90 0424  ABE8 9BDC F497 A4BB CC7F"))))
                       (channel
                        (name 'rde)
                        (url "https://git.sr.ht/~abcdw/rde")
                        (introduction
                         (make-channel-introduction
                          "257cebd587b66e4d865b3537a9a88cccd7107c95"
                          (openpgp-fingerprint
                           "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))
      (simple-service 'main-env-vars
                      home-environment-variables-service-type
                      `(("PATH" . "$PATH:$HOME/.local/bin/:$HOME/.local/lib/npm/bin/")
                        ("HISTCONTROL" . "ignoredups:ignorespace")
                        ("HISTSIZE" . "10000")
                        ("HISTFILE" . "$HOME/.local/share/shell/history")
                        ("LESS" . "-R --use-color -Dd+r$Du+b")
                        ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                        ("QT_QPA_PLATFORM" . "xcb") ; wayland
                        ;; ("XDG_SESSION_TYPE" . "wayland")
                        ;; ("XDG_CURRENT_DESKTOP" . "sway")
                        ;; ("WLR_NO_HARDWARE_CURSORS" . "1")
                        ;; ("WLR_RENDERER" . "vulkan")
                        ("TERM" . "xterm-256color"))))))))

main-home