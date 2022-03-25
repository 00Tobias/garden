(define-module (system syncthing)
  #:use-module (gnu system keyboard)

  #:use-module (gnu services)
  #:use-module (gnu services syncthing))

(define-public services
  (list
   ;; (service
   ;;  syncthing-service-type
   ;;  (syncthing-configuration (user "toxic")))
   ))
