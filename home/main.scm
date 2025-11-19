(define-module (home main)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)

  #:use-module ((gnu packages compression) #:select (atool
                                                     unrar-free
                                                     zip
                                                     unzip
                                                     p7zip))
  #:use-module ((gnu packages vpn) #:select (wireguard-tools))
  #:use-module ((gnu packages xfce) #:select (thunar))
  #:use-module ((gnu packages admin) #:select (btop))
  #:use-module ((gnu packages password-utils) #:select (keepassxc))
  #:use-module ((gnu packages video) #:select (pipe-viewer))
  #:use-module ((gnu packages pdf) #:select (zathura zathura-pdf-mupdf))
  #:use-module ((gnu packages music) #:select (picard))
  #:use-module ((gnu packages bittorrent) #:select (qbittorrent))
  #:use-module ((gnu packages gnupg) #:select (pinentry-emacs))
  #:use-module ((gnu packages qt) #:select (qtwayland qtwayland-5 qt6ct))

  #:use-module ((incognita packages syndication) #:select (photon))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa nvda nvidia-driver))

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type))
  #:use-module ((gnu home services sound) #:select (home-pipewire-service-type))

  #:use-module ((trowel) #:select (aggressively-optimize))
  #:use-module ((home shell) #:prefix shell:)
  #:use-module ((home langs) #:prefix langs:)
  #:use-module ((home gtk) #:prefix gtk:)
  #:use-module ((home wayland sway) #:prefix sway:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home qutebrowser qutebrowser) #:prefix qutebrowser:)
  #:use-module ((home librewolf) #:prefix librewolf:)
  #:use-module ((home mpd) #:prefix mpd:)
  #:use-module ((home mpv mpv) #:prefix mpv:)
  #:use-module ((home creative) #:prefix creative:)
  #:use-module ((home ai) #:prefix ai:)
  #:use-module ((home games) #:prefix games:)
  #:use-module ((home theme) #:prefix theme:)

  #:export (%home-environment))

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

(define git-config (plain-file "gitconfig" "
[user]
	email = tobias@nights.rest
	name = Tobias
	signingkey = 492508B325EA2DB37D05959572F061B81813165A
[commit]
	gpgsign = true
[filter.lfs]
	required = true
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	clean = git-lfs clean -- %f
"))

(define %home-environment
  (home-environment
    (packages
     (append
      shell:packages
      langs:packages
      gtk:packages
      sway:packages
      emacs:packages
      qutebrowser:packages
      librewolf:packages
      mpd:packages
      mpv:packages
      creative:packages
      ai:packages
      games:packages
      (let ((lst (list
                  theme:font-package
                  theme:font-package-mono

                  ;; Archive management
                  atool
                  unrar-free
                  zip
                  unzip
                  p7zip

                  keepassxc                 ; Password manager
                  qt6ct                     ; QT6 theming
                  wireguard-tools           ; VPN connectivity
                  thunar                    ; File manager
                  photon                    ; RSS/Atom
                  pipe-viewer               ; Search YouTube
                  zathura zathura-pdf-mupdf ; PDF reader
                  qbittorrent               ; Torrent client
                  picard                    ; Music tagging
                  (if (or (string= (gethostname) "okarthel")
                          (string= (gethostname) "austrat"))
                      (aggressively-optimize btop-nvidia)
                      btop))))
        (if (string= (gethostname) "okarthel")
            (map replace-mesa lst)
            lst))))

    (services
     (append
      shell:services
      langs:services
      gtk:services
      sway:services
      emacs:services
      qutebrowser:services
      mpd:services
      mpv:services
      (list
       (service home-gpg-agent-service-type
                (home-gpg-agent-configuration
                 (pinentry-program (file-append pinentry-emacs "/bin/pinentry-emacs"))
                 (ssh-support? #t)
                 (default-cache-ttl 3000)
                 (max-cache-ttl 6000)
                 (extra-content (string-append
                                 "\n"
                                 "allow-loopback-pinentry\n"
                                 "allow-emacs-pinentry\n"))))
       (service home-dbus-service-type)
       (service home-pipewire-service-type)
       (simple-service 'home-config home-files-service-type
                       `((".gitconfig"  ,git-config)))
       (simple-service 'main-env-vars
                       home-environment-variables-service-type
                       `(("PATH" . "$HOME/.local/bin/:$PATH:$HOME/.local/lib/cargo/bin/:$HOME/.local/lib/npm/bin/:$HOME/.opam/default/bin/")
                         ("HISTCONTROL" . "ignoredups:ignorespace")
                         ("HISTSIZE" . "10000")
                         ("HISTFILE" . "$HOME/.local/state/shell/history")
                         ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                         ("TERM" . "xterm-256color")
                         ("QT_QPA_PLATFORMTHEME" . "qt6ct")
                         ("QT_PLUGIN_PATH" . ,#~(string-append #$qtwayland "/lib/qt6/plugins:"
                                                               #$qtwayland-5 "/lib/qt5/plugins:"
                                                               #$qt6ct "/lib/qt6/plugins:$QT_PLUGIN_PATH")))))
      (if (string= (gethostname) "okarthel")
          (list
           (simple-service 'okarthel-env-vars
                           home-environment-variables-service-type
                           `(("QT_QPA_PLATFORM" . "wayland-egl")
                             ("XDG_SESSION_TYPE" . "wayland")
                             ("XDG_CURRENT_DESKTOP" . "sway")
                             ("MOZ_ENABLE_WAYLAND" . "1")
                             ("GST_AUDIO_SINK" . "pulsesink")

                             ;; Variables for nvidia-vaapi-driver
                             ("MOZ_DISABLE_RDD_SANDBOX" . "1")
                             ("LIBVA_DRIVER_NAME" . "nvidia")
                             ("__EGL_VENDOR_LIBRARY_FILENAMES" . ,(file-append nvda "/share/glvnd/egl_vendor.d/10_nvidia.x86_64.json")))))
          '())
      (if (string= (gethostname) "austrat")
          (list
           (simple-service
            'austrat-env-vars
            home-environment-variables-service-type
            `(("QT_QPA_PLATFORM" . "wayland-egl")
              ("XDG_SESSION_TYPE" . "wayland")
              ("XDG_CURRENT_DESKTOP" . "sway")
              ("MOZ_ENABLE_WAYLAND" . "1")
              ("GST_AUDIO_SINK" . "pulsesink")

              ;; Variables for nvidia-vaapi-driver
              ("MOZ_DISABLE_RDD_SANDBOX" . "1")
              ("LIBVA_DRIVER_NAME" . "nvidia")
              ("__EGL_VENDOR_LIBRARY_FILENAMES" . ,(file-append nvda "/share/glvnd/egl_vendor.d/10_nvidia.x86_64.json")))))
          '())))))
%home-environment
