(define-module (home xorg bspwm)
  #:use-module (guix gexp)

  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages xorg) #:select (xhost xset xsetroot xrandr))
  #:use-module ((gnu packages wm) #:select (bspwm))
  #:use-module ((gnu packages gnome) #:select (libnotify))
  #:use-module ((gnu packages xdisorg) #:select (sxhkd
                                                 maim
                                                 unclutter
                                                 xclip
                                                 j4-dmenu-desktop
                                                 bemenu))
  #:use-module ((gnu packages kde) #:select (kdeconnect))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages music) #:select (playerctl))

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define xsession
  #~(begin
      (system* #$(file-append xhost "/bin/xhost") "+SI:localuser:tobias")
      (if (string= (gethostname) "okarthel")
          (system* #$(file-append xset "/bin/xset") "s" "off" "-dpms"))
      (system* #$(file-append xsetroot "/bin/xsetroot") "-cursor_name" "left_ptr" "-solid" "black")
      ;; (system* #$(file-append kdeconnect "/libexec/kdeconnectd"))
      (system* #$(file-append dbus "/bin/dbus-run-session") ;; "--exit-with-session"
               #$(file-append bspwm "/bin/bspwm"))))

(define bspwmrc
  #~(begin
      (system* #$(file-append bspwm "/bin/bspc") "config" "border_width" "1")
      (system* #$(file-append bspwm "/bin/bspc") "config" "window_gap" "0")
      (system* #$(file-append bspwm "/bin/bspc") "config" "split_ratio" "0.50")
      (system* #$(file-append bspwm "/bin/bspc") "config" "borderless_monocle" "true")
      (system* #$(file-append bspwm "/bin/bspc") "config" "single_monocle" "true")
      (system* #$(file-append bspwm "/bin/bspc") "config" "gapless_monocle" "true")
      (system* #$(file-append bspwm "/bin/bspc") "config" "focus_follows_pointer" "false")
      (system* #$(file-append bspwm "/bin/bspc") "config" "ignore_ewmh_focus" "false")

      (if (not (string= (gethostname) "okarthel"))
          (system* #$(file-append bspwm "/bin/bspc") "monitor" "-d" "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" "X")
          (begin
            (system* #$(file-append bspwm "/bin/bspc") "monitor" "DP-0" "-d" "I" "II" "III" "IV" "V" "VI")
            (system* #$(file-append bspwm "/bin/bspc") "monitor" "HDMI-1" "-d" "VII" "VIII" "IX" "X")))

      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "Emacs" "state=tiled")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "discord" "desktop='^7'")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "Spotify" "desktop='^7'")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "steam" "desktop='^5'" "state=floating" "focus=off")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "steam:*:Steam" "state=tiled")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "steamwebhelper" "desktop='^5'" "state=floating" "focus=off")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "steamwebhelper:*:*" "desktop='^5'" "state=floating" "focus=off")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "cs2" "desktop='^6'" "state=fullscreen" "focus=on")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "Overwatch" "desktop='^6'" "state=fullscreen" "focus=on")
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "steam_app_1172470" "desktop='^6'" "state=fullscreen" "focus=on")

      (system* #$(file-append xsetroot "/bin/xsetroot") "-cursor_name" "left_ptr" "-solid" "black")
      (system* #$(file-append sxhkd "/bin/sxhkd"))
      (system* #$(file-append unclutter "/bin/unclutter"))
      ;; FIXME: dumb bandage
      (if (string= (gethostname) "okarthel")
          (system* #$(file-append xrandr "/bin/xrandr") "--output" "DP-0" "--mode" "3440x1440" "--rate" "144"))))

(define sxhkdrc "
# WM

super + Escape
	pkill -USR1 -x sxhkd

super + alt + {q,r}
	bspc {quit,wm -r}

super + {_,shift + }q
	bspc node -{c,k}

# Apps

super + Return
	emacsclient -cne '(switch-to-buffer nil)'

super + shift + Return
	emacsclient -cne '(eshell t)'

super + ctrl + Return
	urxvt

super + @space
    j4-dmenu-desktop --dmenu='bemenu -cil 10 -W 0.3 -p \"run:\" -B 1 --fn \"Sarasa Mono TC 11\" --nb \"#000000\" --ab \"#000000\" --fb \"#000000\" --bdr \"#ffffff\" --nf \"#ffffff\" --af \"#ffffff\" --ff \"#ffffff\"' --term='urxvt' --no-generic

super + d
	notify-send -h string:x-canonical-private-synchronous:anything $(date +%D%n%T)

super + v
	urxvt -name floating-terminal -e pulsemixer

super + shift + s
	maim -s | xclip -selection clipboard -t image/png

super + shift + p
	passmenu

# Desktops and nodes

super + m
	bspc desktop -l next

super + y
	bspc node newest.marked.local -n newest.!automatic.local

super + {t,shift + t,s,f}
	bspc node -t {tiled,pseudo_tiled,floating,fullscreen}

super + ctrl + {m,x,y,z}
	bspc node -g {marked,locked,sticky,private}

super + {_,shift + }{h,j,k,l}
	bspc node -{f,s} {west,south,north,east}

super + {_,shift + }{n,e,i,o}
	bspc node -{f,s} {west,south,north,east}

super + ctrl + {h,j,k,l}
	bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

super + ctrl + {n,e,i,o}
	bspc node -z {left -20 0,bottom 0 20,top 0 -20,right 20 0}

super + ctrl + shift + {h,j,k,l}
	bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

super + ctrl + shift + {n,e,i,o}
	bspc node -z {right -20 0,top 0 20,bottom 0 -20,left 20 0}

super + alt + {h,j,k,l}
	bspc node -p {west,south,north,east}

super + alt + {n,e,i,o}
	bspc node -p {west,south,north,east}

super + alt + {1-9}
	bspc node -o 0.{1-9}

super + alt + space
	bspc node -p cancel

super + {p,b,comma,period}
	bspc node -f @{parent,brother,first,second}

super + {_,shift + }c
	bspc node -f {next,prev}.local.!hidden.window

super + bracket{left,right}
	bspc desktop -f {prev,next}.local

super + {grave,Tab}
	bspc {node,desktop} -f last

super + {o,i}
	bspc wm -h off; \
	bspc node {older,newer} -f; \
	bspc wm -h on

super + {_,shift + }{1-9,0}
	bspc {desktop -f,node -d} '^{1-9,10}'

super + ctrl + shift + space
	bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel

# Volume and brightness

XF86Audio{Raise,Lower}Volume
	playerctl volume 0.03{+,-}

XF86AudioMute
	pulsemixer --toggle-mute

XF86AudioPlay
	playerctl play-pause

XF86Audio{Next,Prev}
	playerctl {next,previous}
")

(define-public packages
  (list
   bspwm
   sxhkd
   unclutter
   maim
   xclip
   xrandr
   xsetroot
   libnotify
   pulsemixer
   playerctl
   j4-dmenu-desktop
   bemenu))

(define-public services
  (list
   (simple-service 'bspwm-home-config
                   home-files-service-type
                   `((".xsession" ,(program-file "xsession" xsession))))
   (simple-service 'bspwm-xdg-config
                   home-xdg-configuration-files-service-type
                   `(("bspwm/bspwmrc" ,(program-file "bspwmrc" bspwmrc))
                     ("sxhkd/sxhkdrc" ,(plain-file "sxhkdrc" sxhkdrc))))))
