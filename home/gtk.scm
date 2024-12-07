(define-module (home gtk)
  #:use-module ((gnu packages gnome-xyz) #:select (bibata-cursor-theme))

  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services gtk))

(define-public services
  (list
   (service home-gtk3-service-type
            (home-gtk3-configuration
             (default-cursor "Bibata-Original-Ice")
             (settings-ini
              '((Settings
                 ((gtk-application-prefer-dark-theme . #t)
                  (gtk-menu-popup-delay . 0)
                  (gtk-decoration-layout . "menu")
                  (gtk-cursor-theme-size . 16)
                  (gtk-icon-sizes . "panel-menu=16,16:panel=16,16:gtk-menu=16,16:gtk-large-toolbar=16,16:gtk-small-toolbar=16,16:gtk-button=16,16")))))))))

(define-public packages
  (list bibata-cursor-theme))
