(define-module (home xorg dunst)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module ((gnu packages wm) #:select (dunst))

  #:use-module ((home theme) #:prefix theme:))

(define dunstrc (mixed-text-file "dunstrc" "
[global]
    monitor = 0
    follow = none

    width = (0, 500)
    height = 300
    origin = top-center
    offset = 10x10

    notification_limit = 3

    separator_height = 1

    padding = 2

    horizontal_padding = 2

    text_icon_padding = 0

    frame_width = 2

    frame_color = \"#1c1c1c\"

    idle_threshold = 120

    font = " theme:font " " theme:font-size "

    format = \"<b>%s</b>\\n%b\"

    show_age_threshold = 60

    stack_duplicates = true

    hide_duplicate_count = false

    show_indicators = yes

    min_icon_size = 32

    max_icon_size = 32

    sticky_history = yes

    history_length = 20

    mouse_left_click = do_action, close_current
    mouse_middle_click = close_current
    mouse_right_click = close_all

[urgency_low]
    background = \"#000000\"
    foreground = \"#ffffff\"
    timeout = 3

[urgency_normal]
    background = \"#000000\"
    foreground = \"#ffffff\"
    timeout = 3

[urgency_critical]
    background = \"#000000\"
    foreground = \"#ffffff\"
    timeout = 0

[fullscreen_delay_everything]
    fullscreen = delay
[fullscreen_show_critical]
    msg_urgency = critical
    fullscreen = show
"))

(define-public services
  (list
   (simple-service 'dunst-xdg-config
                   home-xdg-configuration-files-service-type
                   `(("dunst/dunstrc" ,dunstrc)))))
