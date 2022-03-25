(define-module (home nyxt nyxt)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu packages web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu home services))

(define-public packages
  (list nyxt))

(define-public services
  (list
   (simple-service 'nyxt-config
           home-files-service-type
           `(("config/nyxt/init.lisp"
              ,(local-file "config/init.lisp"))))))

