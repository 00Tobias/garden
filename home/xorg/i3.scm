(define-module (home xorg i3)
  #:use-module (guix gexp)

  #:use-module ((gnu packages wm) #:select (i3-wm i3status dunst))
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages gnome) #:select (libnotify))
  #:use-module ((gnu packages xorg) #:select (xhost xset xrdb xsetroot xrandr))
  #:use-module ((gnu packages kde) #:select (kdeconnect))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages music) #:select (playerctl))
  #:use-module ((gnu packages linux) #:select (brightnessctl))
  #:use-module ((gnu packages image-viewers) #:select (feh))
  #:use-module ((gnu packages xdisorg) #:select (sxhkd
                                                 rxvt-unicode
                                                 maim
                                                 unclutter
                                                 xclip
                                                 xdotool
                                                 j4-dmenu-desktop
                                                 bemenu))
  #:use-module ((gnu packages python-xyz) #:select (i3-autotiling))
  #:use-module ((gnu packages emacs) #:select (emacs-next-tree-sitter))
  #:use-module ((gnu packages admin) #:select (sudo))
  #:use-module ((gnu packages gnome) #:select (network-manager))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services wm)

  #:use-module ((system packages nvidia) #:select (replace-mesa))

  #:use-module ((home theme) #:prefix theme:))

(define xsession
  #~(begin
      (system* #$(file-append xhost "/bin/xhost") "+SI:localuser:tobias")
      (if (string= (gethostname) "okarthel")
          (system* #$(file-append xset "/bin/xset") "s" "off" "-dpms"))
      (system* #$(file-append xsetroot "/bin/xsetroot") "-cursor_name" "left_ptr" "-solid" "black")
      (system (string-append #$(file-append xrdb "/bin/xrdb") " -load ~/.Xresources"))
      (system* #$(file-append dbus "/bin/dbus-run-session")
               #$(file-append i3-wm "/bin/i3"))))

(define i3status-config (mixed-text-file "i3status-config" "
disk \"/\" {
    format = \"%free\"
}"))

;; TODO: i3-base-config for sway.scm?
(define i3-config (mixed-text-file "i3-config" "
focus_follows_mouse no
focus_wrapping no
smart_borders on
hide_edge_borders smart
default_border pixel 1
default_floating_border pixel 1

font pango:" theme:font " " theme:font-size "
client.focused " theme:fg " " theme:fg " " theme:bg " " theme:fg "
client.focused_inactive " theme:accent " " theme:accent " " theme:fg " " theme:accent "
client.unfocused " theme:accent " " theme:accent " " theme:fg " " theme:accent "
client.urgent " theme:highlight " " theme:highlight " " theme:bg " " theme:highlight "
client.background " theme:bg "

for_window [all] title_window_icon yes
floating_modifier Mod4

bindsym Mod4+Shift+c reload
bindsym Mod4+Shift+w restart
bindsym Mod4+Shift+q exit
bindsym Mod4+y split h
bindsym Mod4+u split v
bindsym Mod4+f fullscreen toggle
bindsym Mod4+r floating toggle
bindsym Mod4+s layout toggle-split
bindsym Mod4+t layout tabbed
bindsym Mod4+a focus parent
bindsym Mod4+Shift+r focus mode_toggle
bindsym Mod4+minus scratchpad show
bindsym Mod4+Shift+minus move scratchpad
bindsym Mod4+q kill

bindsym Mod4+h focus left
bindsym Mod4+j focus down
bindsym Mod4+k focus up
bindsym Mod4+l focus right
bindsym Mod4+Shift+h move left
bindsym Mod4+Shift+j move down
bindsym Mod4+Shift+k move up
bindsym Mod4+Shift+l move right
bindsym Mod4+n focus left
bindsym Mod4+e focus down
bindsym Mod4+i focus up
bindsym Mod4+o focus right
bindsym Mod4+Shift+n move left
bindsym Mod4+Shift+e move down
bindsym Mod4+Shift+i move up
bindsym Mod4+Shift+o move right
bindsym Mod4+Control+h resize grow left 10 px
bindsym Mod4+Control+j resize grow down 10 px
bindsym Mod4+Control+k resize grow up 10 px
bindsym Mod4+Control+l resize grow right 10 px
bindsym Mod4+Control+Shift+h resize shrink right 10 px
bindsym Mod4+Control+Shift+j resize shrink up 10 px
bindsym Mod4+Control+Shift+k resize shrink down 10 px
bindsym Mod4+Control+Shift+l resize shrink left 10 px
bindsym Mod4+Control+n resize grow left 10 px
bindsym Mod4+Control+e resize grow down 10 px
bindsym Mod4+Control+i resize grow up 10 px
bindsym Mod4+Control+o resize grow right 10 px
bindsym Mod4+Control+Shift+n resize shrink right 10 px
bindsym Mod4+Control+Shift+e resize shrink up 10 px
bindsym Mod4+Control+Shift+i resize shrink down 10 px
bindsym Mod4+Control+Shift+o resize shrink left 10 px

bindsym Mod4+1 workspace number 1
bindsym Mod4+Shift+1 move container to workspace number 1
bindsym Mod4+2 workspace number 2
bindsym Mod4+Shift+2 move container to workspace number 2
bindsym Mod4+3 workspace number 3
bindsym Mod4+Shift+3 move container to workspace number 3
bindsym Mod4+4 workspace number 4
bindsym Mod4+Shift+4 move container to workspace number 4
bindsym Mod4+5 workspace number 5
bindsym Mod4+Shift+5 move container to workspace number 5
bindsym Mod4+6 workspace number 6
bindsym Mod4+Shift+6 move container to workspace number 6
bindsym Mod4+7 workspace number 7
bindsym Mod4+Shift+7 move container to workspace number 7
bindsym Mod4+8 workspace number 8
bindsym Mod4+Shift+8 move container to workspace number 8
bindsym Mod4+9 workspace number 9
bindsym Mod4+Shift+9 move container to workspace number 9
bindsym Mod4+0 workspace number 10
bindsym Mod4+Shift+0 move container to workspace number 10

bindsym Mod4+Return exec " (replace-mesa emacs-next-tree-sitter) "/bin/emacsclient -cne '(switch-to-buffer nil)'
bindsym Mod4+Shift+Return exec " (replace-mesa emacs-next-tree-sitter) "/bin/emacsclient -cne '(eshell t)'
bindsym Mod4+Control+Return exec " rxvt-unicode "/bin/urxvt
bindsym Mod4+space exec --no-startup-id " j4-dmenu-desktop "/bin/j4-dmenu-desktop"
" --dmenu=\"" bemenu "/bin/bemenu"
" -cil 10 -W 0.3 -p 'run:' -B 2"
" --fn '"  theme:font " " theme:font-size "'"
" --nb '"  theme:bg "'"
" --ab '"  theme:bg "'"
" --fb '"  theme:bg "'"
" --bdr '" theme:fg "'"
" --nf '"  theme:fg "'"
" --af '"  theme:fg "'"
" --ff '"  theme:fg "'"
"\" --term " rxvt-unicode "/bin/urxvt"
" --no-generic

bindsym --release Mod4+p exec --no-startup-id " maim "/bin/maim"
" -us | " xclip "/bin/xclip"
" -selection clipboard -t image/png

bindsym Mod4+v exec " rxvt-unicode "/bin/urxvt -name floating-terminal -e " pulsemixer "/bin/pulsemixer
bindsym Mod4+Shift+v exec " rxvt-unicode "/bin/urxvt -name floating-terminal -e " network-manager "/bin/nmtui

bindsym XF86AudioRaiseVolume exec --no-startup-id " playerctl "/bin/playerctl volume 0.03+
bindsym XF86AudioLowerVolume exec --no-startup-id " playerctl "/bin/playerctl volume 0.03-
bindsym XF86AudioMute exec --no-startup-id " pulsemixer "/bin/pulsemixer --toggle-mute
bindsym XF86AudioPlay exec --no-startup-id " playerctl "/bin/playerctl play-pause
bindsym XF86AudioNext exec --no-startup-id " playerctl "/bin/playerctl next
bindsym XF86AudioPrev exec --no-startup-id " playerctl "/bin/playerctl previous
bindsym XF86MonBrightnessUp exec --no-startup-id " brightnessctl "/bin/brightnessctl s 10%+
bindsym XF86MonBrightnessDown exec --no-startup-id " brightnessctl "/bin/brightnessctl s 10%-

bar {
    i3bar_command " i3-wm "/bin/i3bar
    status_command " i3status "/bin/i3status # --config " i3status-config "
    mode dock
    position top
    font pango:" theme:font " " theme:font-size "
    colors {
        background " theme:bg "
        statusline " theme:fg "
        separator "  theme:accent "
        focused_workspace " theme:fg " " theme:fg " " theme:bg "
        active_workspace " theme:fg " " theme:fg " " theme:bg "
        inactive_workspace " theme:accent " " theme:bg " " theme:fg "
        urgent_workspace " theme:fg " " theme:highlight " " theme:bg "
    }
}

for_window [title=\"minibuffer\"] floating enable
for_window [instance=\"floating-terminal\"] floating enable
for_window [class=\"steam\"] floating enable
for_window [title=\"^notificationtoasts.*\"] floating enable
no_focus [class=\"steam\"]
assign [title=\"minibuffer\"] 10
assign [class=\"steam\"] 5
assign [class=\"discord\"] 7

for_window [class=\"hl2_linux\"] fullscreen enable
assign [class=\"hl2_linux\"] 6

exec_always --no-startup-id " i3-autotiling "/bin/autotiling
exec --no-startup-id " dunst "/bin/dunst
exec --no-startup-id " unclutter "/bin/unclutter
exec --no-startup-id " xrandr "/bin/xrandr --output DP-0 --mode 3440x1440 --rate 144"))

(define-public packages
  (list i3-wm rxvt-unicode feh))

(define-public services
  (list
   (simple-service 'bspwm-home-config
                   home-files-service-type
                   `((".xsession" ,(program-file "xsession" xsession))))
   (simple-service 'i3-config
                   home-xdg-configuration-files-service-type
                   `(("i3/config" ,i3-config)))))
