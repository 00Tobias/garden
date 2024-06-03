(define-module (home wayland sway)
  #:use-module (guix gexp)

  #:use-module ((gnu packages wm) #:select (sway dunst))
  #:use-module ((gnu packages base) #:select (findutils))
  #:use-module ((gnu packages xdisorg) #:select (tofi
                                                 wl-clipboard))
  #:use-module ((gnu packages image) #:select (grim slurp))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages linux) #:select (brightnessctl))
  #:use-module ((gnu packages terminals) #:select (foot))
  ;; #:use-module ((gnu packages glib) #:select (glib))
  ;; #:use-module ((gnu packages gnome) #:select (gsettings-desktop-schemas))
  #:use-module ((gnu packages video) #:select (obs-wlrobs))
  #:use-module ((gnu packages freedesktop) #:select (xdg-desktop-portal-wlr))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)

  #:use-module ((home emacs emacs) #:select (emacs-package))
  #:use-module ((home theme) #:prefix theme:))

(define-public sway-package
  (if (string= (gethostname) "okarthel")
      (replace-mesa sway)
      sway))

(define-public services
  (list
   (service
    home-sway-service-type
    (home-sway-configuration
     (package sway-package)
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

        (input type:keyboard ((xkb_layout "se(nodeadkeys)")
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
          (Mod4+Return exec ,(file-append emacs-package "/bin/emacsclient") -cne "'(switch-to-buffer nil)'")
          (Mod4+Shift+Return exec ,(file-append foot "/bin/foot"))

          (Mod4+space exec ,(file-append tofi "/bin/tofi-drun")
                      "|" ,(file-append findutils "/bin/xargs") ,(file-append sway-package "/bin/swaymsg") "exec" "--")

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

        (for_window "[instance=\"floating-terminal\"]" floating enable)
        (for_window "[title=\"^minibuffer$\"]" floating enable)
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
  (let ((lst (list
              ;; `(,glib "bin")
              ;; gsettings-desktop-schemas
              wl-clipboard
              obs-wlrobs
              xdg-desktop-portal-wlr
              sway-package)))
    (if (string= (gethostname) "okarthel")
        (map replace-mesa lst)
        lst)))
