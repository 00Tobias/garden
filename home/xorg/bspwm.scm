(define-module (home xorg bspwm)
  #:use-module (guix gexp)

  #:use-module (gnu packages wm)            ;; bspwm
  #:use-module (gnu packages xdisorg)       ;; sxhkd
  #:use-module (gnu packages compton)       ;; picom
  #:use-module (gnu packages image)         ;; maim
  #:use-module (gnu packages image-viewers) ;; feh
  #:use-module (gnu packages terminals)     ;; alacritty
  #:use-module (gnu packages pulseaudio)    ;; pulsemixer
  #:use-module (gnu packages music)         ;; playerctl
  #:use-module (gnu packages dunst)         ;; dunst
  #:use-module (gnu packages xorg)          ;; xsetroot

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define bspwmrc
  #~(begin
      ;; WM Config
      (system* #$(file-append bspwm "/bin/bspc") "config" "border_width" "3")
      (system* #$(file-append bspwm "/bin/bspc") "config" "window_gap" "10")
      (system* #$(file-append bspwm "/bin/bspc") "config" "split_ratio" "0.50")
      (system* #$(file-append bspwm "/bin/bspc") "config" "focus_follows_pointer" "true")

      ;; Monitors / Workspaces
      ;; TODO: System dependent config of this
      (system* #$(file-append bspwm "/bin/bspc") "monitor" "-d" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10")

      ;; Rules
      (system* #$(file-append bspwm "/bin/bspc") "rule" "-a" "'Emacs'" "desktop='^1'")

      ;; Start programs
      (system* #$(file-append xsetroot "/bin/xsetroot") "-cursor_name" "left_ptr")
      (system* #$(file-append picom "/bin/picom") "-b" "-c" "-C" "-G" "--vsync" "--experimental-backends")
      (system* #$(file-append xbindkeys "/bin/xbindkeys"))
      (system* #$(file-append sxhkd "/bin/sxhkd"))
      ;; (system* #$(file-append dunst "/bin/dunst") "dunst")
      ;; (system* #$(file-append unclutter "/bin/unclutter") "unclutter")

      ;; FIXME: Remove this entirely
      ;; (system* #$(file-append bash "/bin/bash") "-i" #$(local-file "bspwm/bspwmrc"))
      ))

;; Currently does nothing, however I plan on migrating from sxhkd at some point
(define xbindkeysrc
  #~(begin
      (xbindkey '(mod4 control shift q) "xbindkeys_show")))

(define-public packages
  (list
   bspwm
   sxhkd
   xbindkeys
   picom

   ;; WM Tools
   unclutter ;; Cursor hiding
   maim      ;; Screenshotting
   xsel      ;; Clipboard
   feh       ;; Background and image viewer

   ;; Explicitly defined here for bspwm, configs are in their respective files
   ;; TODO: Remove this after migrating fully to xbindkeys
   alacritty
   pulsemixer
   playerctl
   rofi
   dunst))

(define-public services
  (list
   (simple-service 'bspwm-config
           home-files-service-type
           `(("config/bspwm/bspwmrc"
              ,(program-file "bspwmrc" bspwmrc))
             ("xbindkeysrc.scm"
              ,(program-file "xbindkeysrc.scm" xbindkeysrc))
             ("config/sxhkd/sxhkdrc"
              ,(local-file "bspwm/sxhkdrc"))))))
