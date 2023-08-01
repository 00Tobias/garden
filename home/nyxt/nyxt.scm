(define-module (home nyxt nyxt)
  #:use-module (guix gexp)

  #:use-module ((gnu packages web-browsers) #:select (nyxt))

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define-public packages
  (list
   nyxt))

(define-public services
  (list
   (simple-service 'nyxt-config
                   home-files-service-type
                   `((".config/nyxt/config.lisp" ,(local-file "config/config.lisp"))
                     ;; FIXME: This is goofy
                     ;; (".config/nyxt/guix-config.lisp" ,(local-file "config/config.lisp"))
                     ))))

