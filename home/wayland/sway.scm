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
  #:use-module ((gnu packages base) #:select (findutils))
  #:use-module ((gnu packages freedesktop) #:select (wayland-protocols))
  #:use-module ((gnu packages xdisorg) #:select (pixman
                                                 libxkbcommon
                                                 libdrm
                                                 tofi
                                                 wl-clipboard))
  #:use-module ((gnu packages image) #:select (grim slurp))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages linux) #:select (brightnessctl))
  #:use-module ((gnu packages terminals) #:select (foot))
  #:use-module ((gnu packages emacs) #:select (emacs-next-tree-sitter))
  #:use-module ((gnu packages python-xyz) #:select (i3-autotiling))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)

  #:use-module ((home theme) #:prefix theme:))

(define libdrm-2.4.120
  (package
   (inherit libdrm)
   (version "2.4.120")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://dri.freedesktop.org/libdrm/libdrm-"
                  version ".tar.xz"))
            (sha256
             (base32
              "0yijzgg6rdsa68bz03sw0lcfa2nclv9m3as1cja50wkcyxim7x9v"))))))

(define wayland-protocols-1.33
  (package
   (inherit wayland-protocols)
   (name "wayland-protocols")
   (version "1.33")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://gitlab.freedesktop.org/wayland/"
                                name "/-/releases/" version "/downloads/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0flnqfc8npzn6rxsw4afpr8yifwsn5kq81327yh62vhd145wbw4l"))))))

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
                   (replace "wayland-protocols" wayland-protocols-1.33)
                   (prepend vulkan-loader vulkan-headers glslang hwdata-all libdisplay-info
                            libglvnd eglexternalplatform libdrm-2.4.120)))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
           (commit "cb815e88476cac1fb5ce2e369672e4beb4f5d469")))
     (sha256 (base32 "05g6g189aks8j89ar9hvxyad0vpgjgb11xray87fcsszcqif1vx2"))))
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
                (list "-Drenderers=vulkan" ;; ,gles2
                      "-Dbackends=drm,libinput")))))))

(define sway-git
  (package
   (inherit sway)
   (inputs
    (modify-inputs (package-inputs sway)
                   (replace "wlroots" wlroots-git)
                   (prepend libdrm-2.4.120)))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/swaywm/sway")
           (commit "93d391651c1ad43eb8b54449769e034eb1f2380c")))
     (sha256 (base32 "1mpqw26c30krcsk0khzjf7g84acjb5dmilxiwvw4418m7k2c5sk5"))))))

(define-public services
  (list
   (service
    home-sway-service-type
    (home-sway-configuration
     (package (if (string= (gethostname) "okarthel")
                  (replace-mesa sway-git)
                  sway-git))
     (config
      `((smart_borders on)
        (title_align center)
        (focus_follows_mouse no)
        (focus_wrapping no)
        (hide_edge_borders smart)
        (default_border pixel 1)
        (default_floating_border pixel 1)

        (seat * xcursor_theme Adwaita 16)
        (seat * hide_cursor 8000)
        (seat * hide_cursor when-typing enable)

        (input type:keyboard ((xkb_layout se)
                              (xkb_options ctrl:nocaps)))
        (input type:touchpad ((tap enabled)
                              (natural_scroll enabled)
                              (middle_emulation enabled)))

        (output * bg "#000000 solid_color")
        (output DP-1 mode 3440x1440@144Hz)
        (output HDMI-A-2 transform 90)

        (font ,(string-append theme:font " " theme:font-size))
        (client.focused ,theme:fg ,theme:fg ,theme:bg ,theme:fg)
        (client.focused_inactive ,theme:accent ,theme:accent ,theme:fg ,theme:accent)
        (client.unfocused ,theme:accent ,theme:accent ,theme:fg ,theme:accent)
        (client.urgent ,theme:highlight ,theme:highlight ,theme:bg ,theme:highlight)
        (client.background ,theme:bg)
        (gaps inner 0)

        (floating_modifier Mod4)

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
          ;; (Mod4+Shift+r focus mode_toggle)

          (Mod4+minus scratchpad show)
          (Mod4+Shift+minus move scratchpad)

          (Mod4+q kill)
          (Mod4+Return exec ,(file-append (replace-mesa emacs-next-tree-sitter) "/bin/emacsclient") -cne "'(switch-to-buffer nil)'")
          (Mod4+Shift+Return exec ,(file-append foot "/bin/foot"))

          (Mod4+space exec ,(file-append tofi "/bin/tofi-drun")
                      "|" ,(file-append findutils "/bin/xargs") ,(file-append (replace-mesa sway-git) "/bin/swaymsg") "exec" "--")

          (Mod4+p exec ,(file-append grim "/bin/grim")
                  -g ,#~(string-append "$(" #$(file-append slurp "/bin/slurp") ")") "-"
                  "|" ,(file-append wl-clipboard "/bin/wl-copy") -t image/png)

          (Mod4+Shift+c reload)
          (Mod4+Shift+r restart)
          (Mod4+Shift+q exec swaymsg exit)

          (Mod4+1 workspace number 1)
          (Mod4+Shift+1 move container to workspace number 1)
          (Mod4+2 workspace number 2)
          (Mod4+Shift+2 move container to workspace number 2)
          (Mod4+3 workspace number 3)
          (Mod4+Shift+3 move container to workspace number 3)
          (Mod4+4 workspace number 4)
          (Mod4+Shift+4 move container to workspace number 4)
          (Mod4+5 workspace number 5)
          (Mod4+Shift+5 move container to workspace number 5)
          (Mod4+6 workspace number 6)
          (Mod4+Shift+6 move container to workspace number 6)
          (Mod4+7 workspace number 7)
          (Mod4+Shift+7 move container to workspace number 7)
          (Mod4+8 workspace number 8)
          (Mod4+Shift+8 move container to workspace number 8)
          (Mod4+9 workspace number 9)
          (Mod4+Shift+9 move container to workspace number 9)
          (Mod4+0 workspace number 10)
          (Mod4+Shift+0 move container to workspace number 10)))

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
         floating enable, resize set width 70 ppt height 30 ppt)

        (assign "[title=\"minibuffer\"]" 10)
        (assign "[app_id=\"armcord\"]" 7)

        (for_window "[instance=\"floating-terminal\"]" floating enable)
        (for_window "[class=\"steam\"]" floating enable)
        (for_window "[title=\"^notificationtoasts.*\"]" floating enable)
        (no_focus "[class=\"steam\"]")
        (assign "[class=\"steam\"]" 5)

        (for_window "[class=\"hl2_linux\"]" fullscreen enable)
        (assign "[class=\"hl2_linux\"]" 6)

        (exec ,(file-append dunst "/bin/dunst"))))))
   ;; (service home-swaylock-service-type
   ;;          (home-swaylock-configuration
   ;;           ()))
   ))

(define-public packages
  (list
   i3-autotiling
   (if (string= (gethostname) "okarthel")
       (replace-mesa sway-git)
       sway-git)))
