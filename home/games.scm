(define-module (home games)
  #:use-module (guix gexp)

  #:use-module ((gnu packages games) #:select (0ad
                                               wesnoth
                                               nethack))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module ((games packages dwarf-fortress) #:select (dwarf-fortress)))

(define-public packages
  (let ((lst (list
              0ad
              wesnoth
              dwarf-fortress
              nethack)))
    (if (string= (gethostname) "okarthel")
        (map replace-mesa lst)
        lst)))
