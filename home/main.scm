(define-module (home main)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (guix utils)
  #:use-module (guix packages)

  #:use-module ((gnu packages compression) #:select (atool
                                                     unrar-free
                                                     zip
                                                     unzip
                                                     p7zip))
  #:use-module ((gnu packages vpn) #:select (wireguard-tools))
  #:use-module ((gnu packages admin) #:select (btop))
  #:use-module ((gnu packages gnupg) #:select (pinentry))
  #:use-module ((gnu packages password-utils) #:select (password-store))
  #:use-module ((gnu packages pdf) #:select (zathura zathura-pdf-mupdf))
  #:use-module ((gnu packages wine) #:select (wine64-staging))

  #:use-module ((gnu packages fonts) #:select (font-google-noto
                                               font-google-noto-sans-cjk
                                               font-google-noto-emoji))

  #:use-module ((nongnu packages mozilla) #:select (firefox))
  #:use-module ((nongnu packages wine) #:select (winetricks))
  #:use-module ((nongnu packages nvidia) #:select (replace-mesa nvda nvidia-driver))

  #:use-module (gnu services)
  #:use-module ((gnu services shepherd) #:select (shepherd-service))

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module ((gnu home services shepherd) #:select (home-shepherd-service-type))
  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type))
  #:use-module ((gnu home services mcron) #:select (home-mcron-service-type home-mcron-configuration))
  #:use-module ((gnu home services sound) #:select (home-pipewire-service-type))
  #:use-module (rde home services desktop)

  #:use-module ((trowel) #:select (aggressively-optimize))
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
  #:use-module ((home ai) #:prefix ai:)
  #:use-module ((home games) #:prefix games:)
  #:use-module ((home vile) #:prefix vile:))

(define btop-nvidia
  (package
    (inherit btop)
    (inputs
     (modify-inputs (package-inputs btop)
       (prepend nvidia-driver)))
    (arguments
     (substitute-keyword-arguments (package-arguments btop)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'fix-libnvidia
              (lambda _
                (substitute* "src/linux/btop_collect.cpp"
                  (("libnvidia-ml.so.1")
                   (string-append #$(this-package-input "nvidia-driver")
                                  "/lib/libnvidia-ml.so.1")))))))
       ((#:make-flags flags #~'())
        #~(append #$flags (list "GPU_SUPPORT=true")))))))

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
              ai:packages
              games:packages
              vile:packages
              (let ((lst (list
                          atool
                          unrar-free
                          zip
                          unzip
                          p7zip

                          pinentry
                          password-store
                          wireguard-tools
                          zathura
                          zathura-pdf-mupdf
                          wine64-staging
                          winetricks
                          firefox
                          (if (or (string= (gethostname) "okarthel")
                                  (string= (gethostname) "austrat"))
                              (aggressively-optimize btop-nvidia)
                              btop)

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
[filter.lfs]
	required = true
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	clean = git-lfs clean -- %f
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
                      `(("PATH" . "$PATH:$HOME/.local/bin/:$HOME/.local/lib/cargo/bin/:$HOME/.local/lib/npm/bin/")
                        ("HISTCONTROL" . "ignoredups:ignorespace")
                        ("HISTSIZE" . "10000")
                        ("HISTFILE" . "$HOME/.local/state/shell/history")
                        ("LESS" . "-R --use-color -Dd+r$Du+b")
                        ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                        ("TERM" . "xterm-256color"))))
     (if (string= (gethostname) "okarthel")
         (list
          (simple-service 'okarthel-env-vars
                          home-environment-variables-service-type
                          '(("XDG_SESSION_TYPE" . "xcb"))))
         '())
     (if (string= (gethostname) "austrat")
         (list
          (simple-service
           'austrat-color-scheme-shepherd
           home-shepherd-service-type
           (list
            (shepherd-service
             (documentation "Change the color-scheme based on the current time when the system starts up.")
             (provision '(color-scheme))
             (requirement '(dbus))
             (one-shot? #t)
             (start
              #~(make-forkexec-constructor
                 (list
                  #$(program-file
                     "austrat-color-scheme"
                     #~(begin
                         (let ((current-hour (tm:hour (localtime (current-time)))))
                           (if (or (>= current-hour 20) (< current-hour 7))
                               (system* "/run/current-system/profile/bin/gsettings"
                                        "set" "org.gnome.desktop.interface"
                                        "color-scheme" "'prefer-dark'")
                               (system* "/run/current-system/profile/bin/gsettings"
                                        "set" "org.gnome.desktop.interface"
                                        "color-scheme" "'prefer-light'"))))))))
             (stop  #~(make-kill-destructor)))))
          (service
           home-mcron-service-type
           (home-mcron-configuration
            (jobs
             (list #~(job '(next-hour '(20))
                          "/run/current-system/profile/bin/gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'")
                   #~(job '(next-hour '(7))
                          "/run/current-system/profile/bin/gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'")))))
          (simple-service
           'austrat-env-vars
           home-environment-variables-service-type
           `(("QT_QPA_PLATFORM" . "wayland-egl")
             ("XDG_SESSION_TYPE" . "wayland")
             ("XDG_CURRENT_DESKTOP" . "sway")
             ("MOZ_ENABLE_WAYLAND" . "1")
             ("__EGL_VENDOR_LIBRARY_FILENAMES" . ,(file-append nvda "/share/glvnd/egl_vendor.d/50_mesa.x86_64.json"))
             ("__GLX_VENDOR_LIBRARY_NAME" . "mesa"))))
         '())))))

main-home
