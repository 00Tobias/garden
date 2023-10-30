(define-module (system xorg)
  #:use-module (guix transformations)

  #:use-module (gnu system keyboard)

  #:use-module (gnu packages xorg)

  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services sound)     ; pulseaudio, ALSA

  ;; TEMP
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia))

(define %xorg-libinput-config "
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

(define %xorg-nvidia-config "
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
  (cons*
   (service slim-service-type (slim-configuration
                               (default-user "tobias")
                               (auto-login? #t)
                               (xorg-configuration
                                (xorg-configuration
                                 (keyboard-layout (keyboard-layout "se" "nodeadkeys"))
                                 ;; TODO: Move
                                 (modules (cons* nvidia-driver %default-xorg-modules))
                                 (server (replace-mesa xorg-server))
                                 (drivers '("nvidia"))
                                 (extra-config (list %xorg-libinput-config
                                                     %xorg-nvidia-config))))))
   (modify-services %desktop-services
                    (delete gdm-service-type)
                    (delete upower-service-type)
                    (delete screen-locker-service-type)
                    (delete network-manager-service-type)
                    (delete pulseaudio-service-type)
                    (delete alsa-service-type))))
