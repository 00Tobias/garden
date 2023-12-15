(define-module (home xorg xresources)
  #:use-module ((gnu packages gnome-xyz) #:select (bibata-cursor-theme))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services xorg)

  #:use-module ((home theme) #:prefix theme:))

(define-public services
  (list
   (service home-xresources-service-type
            (home-xresources-configuration
             (config
              `((Xcursor.size . 16)
                (Xcursor.theme . "Bibata-Original-Ice")
                ("*.foreground" . ,theme:fg)
                ("*.background" . ,theme:bg)
                ("*.cursorColor" . ,theme:fg)

                (URxvt.font . ,(string-append "xft:" theme:font-term ":size=" theme:font-size))
                (URxvt.letterSpace . 0)
                (URxvt.lineSpace . 0)
                (URxvt.scrollBar . #f)

                (URxvt.secondaryScreen . 1)
                (URxvt.secondaryScroll . 0)

                (URxvt.copyCommand . "xclip -i -selection clipboard")
                (URxvt.pasteCommand . "xclip -o -selection clipboard")
                (URxvt.keysym.Shift-Control-V . "eval:paste_clipboard")
                (URxvt.keysym.Shift-Control-C . "eval:selection_to_clipboard")
                (URxvt.iso14755 . #f)
                (URxvt.iso14755_52 . #f)))))))

(define-public packages
  (list bibata-cursor-theme))
