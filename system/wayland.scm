(define-module (system wayland)
  #:use-module (gnu services)
  #:use-module (gnu services desktop))

(define-public services
  (list (service seatd-service-type)))
