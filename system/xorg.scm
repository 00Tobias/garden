(define-module (system xorg)
  #:use-module (gnu system keyboard)

  #:use-module (gnu packages xorg)
  ;; #:use-module (gnu packages display-managers)
  ;; #:use-module (gnu packages wm)

  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services lightdm)
  #:use-module (gnu services sound) ; pulseaudio, ALSA
  )

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
EndSection")

(define-public services
  (cons*
   (modify-services %desktop-services
                    ;; (delete gdm-service-type)
                    (gdm-service-type config => (gdm-configuration
                                                 (inherit config)
                                                 (default-user "tobias")
                                                 (auto-login? #t)
                                                 (xorg-configuration
                                                  (xorg-configuration
                                                   (keyboard-layout (keyboard-layout "se"))
                                                   (extra-config (list %xorg-libinput-config))))))
                    ;; (delete pulseaudio-service-type)
                    ;; (delete alsa-service-type)
                    )))
