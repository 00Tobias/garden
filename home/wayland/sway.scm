(define-module (home wayland sway)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)

  #:use-module ((gnu packages wm) #:select (sway dunst wlroots))
  #:use-module ((gnu packages vulkan) #:select (vulkan-loader
                                                vulkan-headers
                                                vulkan-tools
                                                glslang))
  #:use-module ((gnu packages terminals) #:select (foot))
  #:use-module ((gnu packages base) #:select (coreutils findutils))
  #:use-module ((gnu packages xdisorg) #:select (tofi
                                                 wl-clipboard))
  #:use-module ((gnu packages image-viewers) #:select (imv))
  #:use-module ((gnu packages image) #:select (slurp flameshot))
  #:use-module ((gnu packages gnome) #:select (libnotify network-manager))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages admin) #:select (btop))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages linux) #:select (brightnessctl))
  #:use-module ((gnu packages kde) #:select (kdeconnect))
  #:use-module ((gnu packages monitoring) #:select (batsignal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (gsettings-desktop-schemas))
  #:use-module ((gnu packages gnome-xyz) #:select (bibata-cursor-theme))
  #:use-module ((gnu packages video) #:select (obs-wlrobs))
  #:use-module ((gnu packages freedesktop) #:select (xdg-desktop-portal
                                                     xdg-desktop-portal-gtk
                                                     xdg-desktop-portal-wlr))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)

  #:use-module (rde serializers ini)

  #:use-module ((trowel) #:select (replace-mesa aggressively-optimize))
  #:use-module ((home emacs emacs) #:select (emacs-package))
  #:use-module ((home theme) #:prefix theme:))

(define wlroots-vulkan
  (package
    (inherit wlroots)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs wlroots)
       (prepend vulkan-loader vulkan-headers glslang)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments wlroots)
       ((#:configure-flags flags #~'())
        #~(append #$flags
                  (list "-Drenderers=vulkan"
                        "-Dbackends=drm,libinput")))))))

(define sway-vulkan
  (package
    (inherit sway)
    (inputs
     (modify-inputs (package-inputs sway)
       (replace "wlroots" wlroots-vulkan)))))

(define foot-config (mixed-text-file "foot-config" "
term=xterm-256color
font=" theme:font-term ":size=" theme:font-size "
[colors]
background=" (substring theme:bg 1 (string-length theme:bg)) "
foreground=" (substring theme:fg 1 (string-length theme:fg)) "
"))

(define tofi-config (mixed-text-file "tofi-config" "
text-cursor = true
prompt-text = \"\"
font = " theme:font-package "/share/fonts/truetype/Sarasa-Regular.ttc
font-size = 12
border-width = 1
outline-width = 1
width = 272
height = 266
padding-top = 4
padding-bottom = 4
padding-left = 4
padding-right = 4
scale = true
" (if (string= (gethostname) "okarthel")
      "output = DP-3"
      "") "
background-color = " theme:bg "
border-color = " theme:fg "
outline-color = " theme:bg "
selection-color = " theme:highlight "
"))

(define-public sway-package
  (if (string= (gethostname) "okarthel")
      (replace-mesa sway)
      sway))

(define-public services
  (list
   (simple-service 'sway-xdg-config home-xdg-configuration-files-service-type
                   `(("foot/foot.ini" ,foot-config)
                     ("tofi/config" ,tofi-config)
                     ("xdg-desktop-portal/sway-portals.conf"
                      ,(apply mixed-text-file "xdg-desktop-portal-sway-portals"
                              (serialize-ini-config
                               `((preferred
                                  ((default . gtk)
                                   (org.freedesktop.impl.portal.Screenshot . wlr)
                                   (org.freedesktop.impl.portal.ScreenCast . wlr)))))))
                     ("xdg-desktop-portal-wlr/config"
                      ,(apply mixed-text-file "xdg-desktop-portal-wlr-config"
                              (serialize-ini-config
                               `((screencast
                                  ((max_fps . 30)
                                   (chooser_cmd . ,#~(string-append
                                                      #$(file-append slurp "/bin/slurp")
                                                      " -f %o -or -c ff0000"))
                                   (chooser_type . simple)))))))))
   (service
    home-sway-service-type
    (home-sway-configuration
     (package sway-package)
     (config
      (append
       (if (string= (gethostname) "okarthel")
           '((input type:keyboard ((xkb_layout "se(nodeadkeys)")
                                   (xkb_options ctrl:nocaps)))

             (output DP-3 mode 3440x1440@144Hz)
             (output HDMI-A-2 transform 90)
             (workspace 9 output HDMI-A-2))
           '())
       (if (string= (gethostname) "austrat")
           '((input type:keyboard ((xkb_layout "us(altgr-intl)")
                                   (xkb_options ctrl:nocaps)))
             (input type:touchpad ((tap enabled)
                                   (natural_scroll enabled)
                                   (middle_emulation enabled)
                                   (accel_profile flat)
                                   (pointer_accel 1.0)))
             (input 1267:11350:ELAN9008:00_04F3:2C56 map_to_output eDP-1)
             (input 1267:11299:ELAN9009:00_04F3:2C23 map_to_output DP-3)

             (output eDP-1 position 0 0)
             (output eDP-1 scale 2)
             (output DP-3 position 0 1080)
             (output DP-3 scale 2)
             (workspace 1 output eDP-1)
             (workspace 9 output DP-3))
           '())
       `((focus_follows_mouse no)
         (focus_wrapping no)
         (smart_borders on)
         (hide_edge_borders smart)
         (workspace_layout tabbed)
         (default_border normal 1)
         (default_floating_border pixel 1)

         (seat * xcursor_theme Bibata-Original-Ice 16)

         (output * bg "#000000 solid_color")

         (font ,(string-append theme:font " " theme:font-size))
         (client.focused ,theme:fg ,theme:fg ,theme:bg ,theme:fg)
         (client.focused_inactive ,theme:accent ,theme:accent ,theme:fg ,theme:bg)
         (client.unfocused ,theme:accent ,theme:accent ,theme:fg ,theme:bg)
         (client.urgent ,theme:highlight ,theme:highlight ,theme:bg ,theme:highlight)
         (client.background ,theme:bg)

         (floating_modifier Mod4)

         (bindsym
          ((Mod4+Shift+c reload)
           (Mod4+Shift+w restart)
           (Mod4+Shift+q exec swaymsg exit)
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

           (Mod4+h focus left)
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
           (Mod4+Control+Shift+h resize shrink right 10 px)
           (Mod4+Control+Shift+j resize shrink up 10 px)
           (Mod4+Control+Shift+k resize shrink down 10 px)
           (Mod4+Control+Shift+l resize shrink left 10 px)

           (Mod4+Control+n resize grow left 10 px)
           (Mod4+Control+e resize grow down 10 px)
           (Mod4+Control+i resize grow up 10 px)
           (Mod4+Control+o resize grow right 10 px)
           (Mod4+Control+Shift+n resize shrink right 10 px)
           (Mod4+Control+Shift+e resize shrink up 10 px)
           (Mod4+Control+Shift+i resize shrink down 10 px)
           (Mod4+Control+Shift+o resize shrink left 10 px)

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
           (Mod4+Shift+0 move container to workspace number 10)

           (Mod4+Return exec ,(file-append emacs-package "/bin/emacsclient") -cne "'(switch-to-buffer nil)'")
           (Mod4+Shift+Return exec ,(file-append emacs-package "/bin/emacsclient") -cne "'(eshell t)'")
           (Mod4+Control+Return exec ,(file-append foot "/bin/foot"))

           (Mod4+space exec ,(file-append tofi "/bin/tofi-drun")
                       "|" ,(file-append findutils "/bin/xargs")
                       ,(file-append sway-package "/bin/swaymsg") "exec" "--")

           (Mod4+p exec ,(file-append flameshot "/bin/flameshot") gui --raw
                   "|" ,(file-append wl-clipboard "/bin/wl-copy"))

           (Mod4+d exec ,(file-append libnotify "/bin/notify-send")
                   -h string:x-canonical-private-synchronous:anything
                   "$(",(file-append coreutils "/bin/date") +%D%n%T")")

           (Mod4+b exec ,(file-append libnotify "/bin/notify-send")
                   -h string:x-canonical-private-synchronous:anything
                   "\"$(",(file-append coreutils "/bin/cat") "'/sys/class/power_supply/BAT0/status')\""
                   "\"$(",(file-append coreutils "/bin/cat") "'/sys/class/power_supply/BAT0/capacity')%"
                   "($(",(file-append coreutils "/bin/cat") "'/sys/bus/pci/devices/0000:01:00.0/power/runtime_status'))\"")

           (Mod4+v exec ,(file-append foot "/bin/foot") -a floating-terminal ,(file-append pulsemixer "/bin/pulsemixer"))
           (Mod4+Shift+v exec ,(file-append foot "/bin/foot") -a floating-terminal ,(file-append network-manager "/bin/nmtui"))
           (Mod4+c exec ,(file-append foot "/bin/foot") -a floating-terminal ,(file-append btop "/bin/btop"))))

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

         (for_window "[title=\"^dedicated-minibuffer-frame$\"]" floating enable, border pixel 1)
         (for_window "[app_id=\"floating-terminal\"]" floating enable, border pixel 1)
         (for_window "[class=\"steam\"]" floating enable)
         (for_window "[title=\"^notificationtoasts.*\"]" floating enable)
         (no_focus "[class=\"steam\"]")
         (assign "[class=\"steam\"]" 5)

         (for_window "[class=\"hl2_linux\"]" fullscreen enable)
         (assign "[class=\"hl2_linux\"]" 6)

         (exec ,(file-append dunst "/bin/dunst"))
         (exec ,(file-append kdeconnect "/bin/kdeconnectd"))
         (exec ,(file-append batsignal "/bin/batsignal") -e -f 100 -w 30 -c 25 -d 10)
         (exec ,(file-append xdg-desktop-portal-gtk "/libexec/xdg-desktop-portal-gtk"))
         (exec ,(file-append xdg-desktop-portal-wlr "/libexec/xdg-desktop-portal-wlr"))
         (exec ,(file-append xdg-desktop-portal "/libexec/xdg-desktop-portal") -r)
         (exec /run/current-system/profile/bin/gsettings set org.gnome.desktop.interface color-scheme prefer-dark)
         (exec /run/current-system/profile/bin/gsettings set org.gnome.desktop.interface cursor-theme Bibata-Original-Ice)
         (exec /run/current-system/profile/bin/gsettings set org.gnome.desktop.interface cursor-size 16))))))))

(define-public packages
  (append
   (list `(,glib "bin"))
   (let ((lst (list
               gsettings-desktop-schemas
               bibata-cursor-theme
               wl-clipboard
               imv
               obs-wlrobs
               xdg-desktop-portal
               xdg-desktop-portal-gtk
               xdg-desktop-portal-wlr
               sway-package)))
     (if (string= (gethostname) "okarthel")
         (map replace-mesa lst)
         lst))))
