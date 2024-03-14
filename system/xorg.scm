(define-module (system xorg)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop))

(define-public %libinput-config "
Section \"InputClass\"
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
  Identifier \"Mice\"
  Driver \"libinput\"
  MatchIsPointer \"on\"
  Option \"AccelProfile\" \"flat\"
  Option \"AccelSpeed\" \"0\"
  Option \"Emulate3Buttons\" \"on\"
EndSection

Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define-public %nvidia-config "
Section \"Monitor\"
  Identifier \"DP-0\"
  Option \"Primary\" \"true\"
  ModeLine \"3440x1440_144.00_rb2\"  782.12  3440 3448 3480 3520  1440 1529 1537 1543 +hsync -vsync
  Option \"PreferredMode\" \"3440x1440_144.00_rb2\"
EndSection

Section \"Monitor\"
  Identifier \"HDMI-1\"
  Option \"Rotate\" \"right\"
EndSection
")

(define-public services
  (list
   (service elogind-service-type)
   (service slim-service-type
            (slim-configuration
             (default-user "tobias")
             (auto-login? #t)))))
