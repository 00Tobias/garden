(define-module (home sway)
  #:use-module (guix gexp)

  #:use-module ((gnu packages linux)      #:select (light))
  #:use-module ((gnu packages wm)         #:select (sway mako))
  #:use-module ((gnu packages music)      #:select (playerctl))
  #:use-module ((gnu packages pulseaudio) #:select (pulsemixer))
  #:use-module ((gnu packages terminals)  #:select (foot))
  #:use-module ((gnu packages xdisorg)    #:select (j4-dmenu-desktop))
  #:use-module ((gnu packages python-xyz) #:select (i3-autotiling))

  #:use-module (gnu services)

  #:use-module (home services mako)

  #:use-module ((rde packages) #:select (sway-latest))
  #:use-module (rde home services wm))

(define ws-bindings
  (map (lambda (ws)
         `(,(string->symbol (format #f "$mod+~a" ws))
           workspace number ,ws))
       (iota 9 1)))

(define ws-move-bindings
  (map (lambda (ws)
         `(,(string->symbol (format #f "$mod+Shift+~a" ws))
           move container to workspace number ,ws))
       (iota 9 1)))

(define-public services
  (list
   (service home-sway-service-type
            (home-sway-configuration
             (package sway-latest)
             (config
              `((xwayland enable)

                (smart_borders on)
                (title_align center)

	            ;; (window_)

                ;; (output * bg ,(local-file "files/wp.jpg") fill)

                ;; (font "Iosevka, Light 14")
                ;; (client.focused "#f0f0f0" "#f0f0f0" "#721045" "#721045" "#721045")
                ;; (client.unfocused "#ffffff" "#ffffff" "#595959")
                ;; (seat * xcursor_theme Adwaita 24)

	            (input type:keyboard ((xkb_layout "se")))
	            (input type:touchpad ((tap "enabled")
			                          (natural_scroll "enabled")
			                          (middle_emulation "enabled")))

	            (seat * ((hide_cursor "80000")))

                ;; (floating.criteria ((title "*Minibuf-1*")))

                (for_window
                 appid="emacs"
                 floating enable)

                (for_window
                 "[title=\"\\*Minibuf-1\\*\"]"
                 floating disable)

                (set $mod Mod4)
                (set $left  h)
	            (set $down  j)
	            (set $up    k)
                (set $right l)

                (set $term ,(file-append foot "/bin/foot"))
                (set $menu ,(file-append j4-dmenu-desktop "/bin/j4-dmenu-desktop"))

                (bindsym
                 (($mod+Return exec $term)
                  ($mod+space exec $menu)

                  ($mod+q kill)
                  ($mod+Shift+r reload)
                  ($mod+Shift+q exec ,(file-append sway "/bin/swaymsg") exit)

                  ($mod+$left  focus left)
	              ($mod+$down  focus down)
	              ($mod+$up    focus up)
	              ($mod+$right focus right)

	              ($mod+Shift+$left  move left)
	              ($mod+Shift+$down  move down)
	              ($mod+Shift+$up    move up)
	              ($mod+Shift+$right move right)

	              ($mod+Control+$left  resize grow left 10 px)
	              ($mod+Control+$down  resize grow down 10 px)
	              ($mod+Control+$up    resize grow up 10 px)
	              ($mod+Control+$right resize grow right 10 px)

	              ($mod+Control+Shift+$left  resize shrink right 10 px)
	              ($mod+Control+Shift+$down  resize shrink up 10 px)
	              ($mod+Control+Shift+$up    resize shrink down 10 px)
	              ($mod+Control+Shift+$right resize shrink left 10 px)

                  ($mod+Tab layout toggle split tabbed)
                  ($mod+Shift+Tab split toggle)
                  ($mod+grave floating toggle)
                  ($mod+Shift+grave focus mode_toggle)
                  ($mod+g exec ,(file-append mako "/bin/makoctl") dismiss --all)
                  ($mod+m exec ,(file-append mako "/bin/makoctl") set-mode dnd)
                  ($mod+Shift+m exec ,(file-append mako "/bin/makoctl") set-mode default)
                  ,@ws-bindings
                  ,@ws-move-bindings))

                (bindsym
                 --locked
                 ((XF86AudioRaiseVolume exec ,(file-append playerctl "/bin/playerctl") volume 0.03+)
                  (XF86AudioLowerVolume exec ,(file-append playerctl "/bin/playerctl") volume 0.03-)
                  (XF86AudioMute exec ,(file-append pulsemixer "/bin/pulsemixer") --toggle-mute)
                  (XF86AudioPlay exec ,(file-append playerctl "/bin/playerctl") play-pause)
                  (XF86AudioNext exec ,(file-append playerctl "/bin/playerctl") next)
                  (XF86AudioPrev exec ,(file-append playerctl "/bin/playerctl") prev)
                  (XF86MonBrightnessUp exec ,(file-append light "/bin/light") -A 10)
                  (XF86MonBrightnessDown exec ,(file-append light "/bin/light") -U 10)))

                ;; (exec ,(file-append swayidle "/bin/swayidle") -w
                ;;       before-sleep "'swaylock -f'"
                ;;       timeout 1800 "'swaylock -f'"
                ;;       timeout 2400 "'swaymsg \"output * dpms off\"'"
                ;;       resume "'swaymsg \"output * dpms on\"'")
                (exec ,(file-append mako "/bin/mako"))
	            (exec_always ,(file-append i3-autotiling "/bin/autotiling"))))))

   (service
    home-mako-service-type
    (home-mako-configuration
     (package mako)
     (config
      `((sort . time)
	    (text-color . "#FFFFFF")
	    (background-color . "#000000")))))))
