(define-module (home xorg rofi)
  #:use-module ((gnu packages gnome) #:select (adwaita-icon-theme))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services xdisorg)

  #:use-module ((gnu packages fonts) #:select (font-sarasa-gothic))

  #:use-module ((home theme) #:prefix theme:))

(define-public packages
  (list adwaita-icon-theme))

(define-public services
  (list
   (service
    home-rofi-service-type
    (home-rofi-configuration
     (config-rasi
      `((configuration
         ((font . ,(string-append theme:font " " theme:font-size))
          (show-icons . #t)
          (icon-theme . "Adwaita")))
        (* ((background . ,theme:bg)
            (foreground . ,theme:fg)
            (border-color . ,theme:fg)
            (separator-color . ,theme:fg)))
        (window ((padding . "20px")
                 (height . "320px")
                 (border . "2px")
                 (border-color . ,theme:accent)))
        (mainbox ((padding . "5px")
                  (border-radius . "5px 5px 0px")))
        ;; (inputbar ((children . #("prompt" "entry"))))
        (prompt ((background-color . ,theme:accent)
                 (padding . "5px 5px 0px")))
        (textbox-prompt-colon ((expand . #f)
                               (str . ":")))
        (entry ((padding . "5px")))
        (listview ((border . "0px 0px 0px")
                   (padding . "6px 0px 0px")))
        (element ((padding . "5px")))
        (element-icon ((size . "25px")))
        ;; ("element selected" ((background-color . ,theme:accent)))
        ))))))
