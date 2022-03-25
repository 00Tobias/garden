(define-module (home xorg dunst)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services wm)
  #:use-module (home services dunst)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages chromium)
  #:use-module (guix gexp))

(define-public services
  (list
   (service
    home-dunst-service-type
    (home-dunst-configuration
     (config
      `((global
         ((follow . keyboard)

          (geometry . "\"400x20-30+20\"")
          (indicate_hidden . #t)
          (timeout . 5)
          (shrink . #f)
          (transparency . 0)
          (notification_height . 0)
          (separator_height . 2)
          (padding . 8)
          (horizontal_padding . 8)
          (frame_width . 3)
          ;; (frame_color . "\"#8fbcbb\"")
          ;; (separator_color . frame)
          (sort . #t)
          (idle_threshold . 12)
          (font . "Cozette")
          (line_height . 0)
          (markup . full)
          ;; (format . "\"<b>%s</b>\\n%b\"")
          ;; (alignment . left)
          (show_age_threshold . 60)
          (word_wrap . #t)
          (ellipsize . middle)
          (ignore_newline . #f)
          (stack_duplicates . #t)
          (hide_duplicate_count . #f)
          (show_indicators . #t)
          (icon_position . off)
          (max_icon_size . 32)
          (sticky_history . #t)
          (history_length . 20)
          (mouse_left_click . do_action)
          (mouse_middle_click . close_current)
          (mouse_right_click . close_all)))

        (shortcuts
         ((close . ctrl+space)
          (close_all . ctrl+shift+space)
          (history . ctrl+grave)
          (context . ctrl+shift+period)))

        (urgency_low
         ((background . "\"#262831\"")
          (foreground . "\"#88c0d0\"")
          (timeout . 5)))

        (urgency_normal
         ((background . "\"#262831\"")
          (foreground . "\"#a3be8c\"")
          (timeout . 5)))

        (urgency_critical
         ((background . "\"#262831\"")
          (foreground . "\"#bf616a\"")
          (frame_color . "\"#4c566a\"")
          (timeout . 5)))))))))
