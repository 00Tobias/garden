(define-module (system xorg)
  #:use-module (gnu system keyboard)

  #:use-module (gnu packages xorg)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages wm)

  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sddm))

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
  Option \"NaturalScrolling\" \"true\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define-public packages
  (list
   sddm
   sugar-dark-sddm-theme
   bspwm stumpwm))                            ;; Temp

(define-public services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout (keyboard-layout "se")) ;; Base keyboard layout, overwritten based on host

     ;; Use libinput in Xorg
     (extra-config
      (list %xorg-libinput-config))
     (modules
      (filter
       (lambda (mod)
         (not (eq? mod xf86-input-synaptics)))
       %default-xorg-modules)))
    sddm-service-type)

   (service
    sddm-service-type
    (sddm-configuration
     (theme "sugar-dark")
     (auto-login-user "toxic")
     (auto-login-session
      "bspwm.desktop")))

   (modify-services
    %desktop-services
    (delete gdm-service-type))))
