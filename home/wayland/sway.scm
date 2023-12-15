(define-module (home wayland sway)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (srfi srfi-1)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module ((gnu packages wm) #:select (sway wlroots dunst))
  #:use-module ((gnu packages vulkan) #:select (vulkan-loader
                                                vulkan-headers
                                                vulkan-tools
                                                glslang))
  #:use-module ((gnu packages gl) #:select (libglvnd))
  #:use-module ((gnu packages xorg) #:select (eglexternalplatform))
  #:use-module ((gnu packages pciutils) #:select (hwdata))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages xdisorg) #:select (pixman
                                                 libxkbcommon
                                                 libdrm
                                                 j4-dmenu-desktop
                                                 bemenu
                                                 wl-clipboard))
  #:use-module ((gnu packages image) #:select (grim slurp))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages linux) #:select (brightnessctl))
  #:use-module ((gnu packages terminals) #:select (foot))
  #:use-module ((gnu packages python-xyz) #:select (i3-autotiling))

  #:use-module ((home mpv mpv) #:select (vulkan-loader-nvidia))

  #:use-module ((system packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)

  #:use-module ((home theme) #:prefix theme:))

(define pixman-42
  (package
   (inherit pixman)
   (version "0.42.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://www.cairographics.org/releases/pixman-"
                         version ".tar.gz"))
     (sha256 (base32 "0pk298iqxqr64vk3z6nhjwr6vjg1971zfrjkqy5r9zd2mppq057a"))))))

(define libxkbcommon-1.6
  (package
   (inherit libxkbcommon)
   (version "1.6.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://xkbcommon.org/download/libxkbcommon-"
                                version ".tar.xz"))
            (sha256 (base32 "0awwz5pg9x5bj0d7dpg4a7bd4gl6k55mlpxwb12534fkrpn19p0f"))))))

(define hwdata-all
  (package
   (inherit hwdata)
   (arguments
    (list
     #:tests? #f
     #:target #f
     #:configure-flags #~(list (string-append "--datadir=" #$output "/share"))))))

(define libdisplay-info
  (package
   (name "libdisplay-info")
   (version "0.2.0-dev")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.freedesktop.org/emersion/libdisplay-info.git")
           (commit "ae6cb5242e40563bbea0048690e432a223f6f452")))
     (sha256 (base32 "022biq79nqyjgzjmcj2p0g240dnimvsm01hk4s40mggn1g49j59i"))))
   (build-system meson-build-system)
   (native-inputs (list pkg-config hwdata-all python))
   (arguments (list #:tests? #f))
   (synopsis "EDID and DisplayID library")
   (description "EDID and DisplayID library")
   (home-page "https://gitlab.freedesktop.org/emersion/libdisplay-info")
   (license license:expat)))

(define wlroots-git
  (package
   (inherit wlroots)
   (propagated-inputs
    (modify-inputs (package-propagated-inputs wlroots)
                   (replace "pixman" pixman-42)
                   (replace "libxkbcommon" libxkbcommon-1.6)
                   (prepend vulkan-loader vulkan-headers glslang hwdata-all libdisplay-info
                            libglvnd eglexternalplatform)))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
           (commit "36cc698bc5513655e5c57ba62693fce9001d86e7")))
     (sha256 (base32 "0ppzdb9p10bp754cw637b9p90nagixfb8q23b06vgwq766igad7f"))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments wlroots)
     ((#:phases phases)
      #~(modify-phases
         #$phases
         (replace 'fix-meson-file
                  (lambda* (#:key native-inputs inputs #:allow-other-keys)
                    (substitute* "backend/drm/meson.build"
                                 (("hwdata.get_variable(pkgconfig: 'pkgdatadir')")
                                  (string-append #$(this-package-input "hwdata")
                                                 "/share/hwdata/")))))))
     ((#:configure-flags flags #~'())
      #~(append #$flags
                (list "-Drenderers=vulkan,gles2"
                      "-Dbackends=drm,libinput")))))))

(define sway-git
  (package
   (inherit sway)
   (inputs
    (modify-inputs (package-inputs sway)
                   (prepend pixman-42)
                   (replace "libxkbcommon" libxkbcommon-1.6)
                   (replace "wlroots" wlroots-git)))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/swaywm/sway")
           (commit "bc7d15d64da1d8b97d52928b8f9ce5688c8dbdd0")))
     (sha256 (base32 "057dq4j66mplixfp88h2jfcbk7cqgqckb72jaidmjqgn3igm45k0"))))))

(define wlroots-git-nvidia
  (package
   (inherit wlroots-git)
   (propagated-inputs
    (modify-inputs (package-propagated-inputs wlroots-git)
                   (replace "vulkan-loader" vulkan-loader-nvidia)))))

(define sway-git-nvidia
  (package
   (inherit sway-git)
   (inputs
    (modify-inputs (package-inputs sway-git)
                   (replace "wlroots" wlroots-git-nvidia)))))

(define vulkan-tools-nvidia
  (package
   (inherit vulkan-tools)
   (inputs
    (modify-inputs (package-inputs vulkan-tools)
                   (replace "vulkan-loader" vulkan-loader-nvidia)))
   (arguments
    (substitute-keyword-arguments
     (package-arguments vulkan-tools)
     ((#:configure-flags flags #~'())
      #~(append #$flags
                (list "-DBUILD_ICD=off")))))))

(define ws-bindings
  (map (lambda (ws)
         `(,(string->symbol (format #f "Mod4+~d" ws))
           workspace number ,ws))
       (iota 9 1)))

(define ws-move-bindings
  (map (lambda (ws)
         `(,(string->symbol (format #f "Mod4+Shift+~d" ws))
           move container to workspace number ,ws))
       (iota 9 1)))

(define-public services
  (list
   (service
    home-sway-service-type
    (home-sway-configuration
     (package (if (string= (gethostname) "okarthel")
                  (replace-mesa sway-git-nvidia)
                  sway-git))
     (config
      `((smart_borders on)
        (title_align center)
        (seat * xcursor_theme Adwaita 16)
        (seat * hide_cursor 8000)
        (seat * hide_cursor when-typing enable)

        (input type:keyboard ((xkb_layout se)
                              (xkb_options ctrl:swapcaps)))
        (input type:touchpad ((tap enabled)
                              (natural_scroll enabled)
                              (middle_emulation enabled)))

        (output HDMI-1 ((transform 90)
                        (position 0 0)))

        (font ,(string-append theme:font " " theme:font-size))
        (client.focused ,theme:fg ,theme:accent ,theme:accent ,theme:accent)
        (client.unfocused ,theme:fg ,theme:bg ,theme:accent ,theme:accent)
        (default_border normal 2)
        (gaps inner 6)

        ;; (set $term ,(file-append foot "/bin/foot"))

        (bindsym
         ((Mod4+h focus left)
          (Mod4+j focus down)
          (Mod4+k focus up)
          (Mod4+l focus right)
          (Mod4+Shift+h move left)
          (Mod4+Shift+j move down)
          (Mod4+Shift+k move up)
          (Mod4+Shift+l move right)

          (Mod4+n focus left)
          (Mod4+e focus down)
          (Mod4+i focus up)
          (Mod4+o focus right)
          (Mod4+Shift+n move left)
          (Mod4+Shift+e move down)
          (Mod4+Shift+i move up)
          (Mod4+Shift+o move right)

          (Mod4+Control+h resize grow left 10 px)
          (Mod4+Control+j resize grow down 10 px)
          (Mod4+Control+k resize grow up 10 px)
          (Mod4+Control+l resize grow right 10 px)
          (Mod4+Control+Shift+h resize shrink left 10 px)
          (Mod4+Control+Shift+j resize shrink down 10 px)
          (Mod4+Control+Shift+k resize shrink up 10 px)
          (Mod4+Control+Shift+l resize shrink right 10 px)

          (Mod4+Control+n resize grow left 10 px)
          (Mod4+Control+e resize grow down 10 px)
          (Mod4+Control+i resize grow up 10 px)
          (Mod4+Control+o resize grow right 10 px)
          (Mod4+Control+Shift+n resize shrink left 10 px)
          (Mod4+Control+Shift+e resize shrink down 10 px)
          (Mod4+Control+Shift+i resize shrink up 10 px)
          (Mod4+Control+Shift+o resize shrink right 10 px)

          (Mod4+y split h)
          (Mod4+u split v)
          (Mod4+f fullscreen toggle)
          (Mod4+r floating toggle)
          (Mod4+s layout toggle-split)
          (Mod4+t layout tabbed)

          (Mod4+a focus parent)
          (Mod4+Shift+r focus mode_toggle)

          (Mod4+minus scratchpad show)
          (Mod4+Shift+minus move scratchpad)

          (Mod4+q kill)
          (Mod4+Return exec ,(file-append foot "/bin/foot"))
          ;; TODO: tofi?
          (Mod4+space exec ,(file-append j4-dmenu-desktop "/bin/j4-dmenu-desktop")
                      --dmenu ,(file-append bemenu "/bin/bemenu")
                      -cil 10 -W 0.3 -p "'run:'" -B 2
                      --fn  ,(string-append "'" theme:font " " theme:font-size "'")
                      --nb  ,(string-append "'" theme:bg "'")
                      --ab  ,(string-append "'" theme:bg "'")
                      --fb  ,(string-append "'" theme:bg "'")
                      --bdr ,(string-append "'" theme:fg "'")
                      --nf  ,(string-append "'" theme:fg "'")
                      --af  ,(string-append "'" theme:fg "'")
                      --ff  ,(string-append "'" theme:fg "'")
                      --term ,(file-append foot "/bin/foot")
                      --no-generic)

          (Mod4+p exec ,(file-append grim "/bin/grim")
                  -g ,#~(string-append "$(" #$(file-append slurp "/bin/slurp") ")") "-"
                  "|" ,(file-append wl-clipboard "/bin/wl-copy") -t image/png)

          (Mod4+Shift+c reload)
          (Mod4+Shift+r restart)
          (Mod4+Shift+q exec swaymsg exit)

          ,@(append-map
             (lambda (x)
               `((bindsym ,(format #f "Mod4+~a" (modulo x 10))
                          workspace number ,x)
                 (bindsym ,(format #f "Mod4+Shift+~a" (modulo x 10))
                          move container to workspace number ,x)))
             (iota 10 1))

          ;; ,@ws-bindings
          ;; ,@ws-move-bindings
          ))

        (bindsym
         --locked
         ((XF86AudioRaiseVolume exec ,(file-append playerctl "/bin/playerctl") volume 0.03+)
          (XF86AudioLowerVolume exec ,(file-append playerctl "/bin/playerctl") volume 0.03-)
          (XF86AudioMute exec ,(file-append pulsemixer "/bin/pulsemixer") --toggle-mute)
          (XF86AudioPlay exec ,(file-append playerctl "/bin/playerctl") play-pause)
          (XF86AudioNext exec ,(file-append playerctl "/bin/playerctl") next)
          (XF86AudioPrev exec ,(file-append playerctl "/bin/playerctl") previous)
          (XF86MonBrightnessUp exec ,(file-append brightnessctl "/bin/brightnessctl") s 10%+)
          (XF86MonBrightnessDown exec ,(file-append brightnessctl "/bin/brightnessctl") s 10%-)))

        (for_window
         "[title=\"minibuffer\"]"
         floating enable, resize set width 70 ppt height 70 ppt, focus)

        (assign "[title=\"minibuffer\"]" 10)
        (assign "[app_id=\"armcord\"]" 7)

        (exec ,(file-append dunst "/bin/dunst"))))))
   ;; (service home-swaylock-service-type
   ;;          (home-swaylock-configuration
   ;;           ()))
   ))

(define-public packages
  (list
   i3-autotiling
   vulkan-tools-nvidia))
