(define-module (system common)
  #:use-module (guix gexp)
  #:use-module ((guix store) #:select (%default-substitute-urls))

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services guix))

(define-public services
  (append
   (modify-services %base-services
     (guix-service-type config =>
                        (guix-configuration
                         (inherit config)
                         (extra-options '("--disable-deduplication")) ; Disable store deduplication, as I do my own with bees.
                         (substitute-urls
                          (append (list "https://substitutes.nonguix.org"
                                        "https://guix.bordeaux.inria.fr"
                                        "https://cache-cdn.guix.moe")
                                  %default-substitute-urls))
                         (authorized-keys
                          (append (list
                                   (plain-file "nonguix-signing-key.pub"
                                               "
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
 )
)")
                                   (plain-file "guix-moe-signing-key.pub"
                                               "
(public-key
 (ecc (curve Ed25519)
  (q #374EC58F5F2EC0412431723AF2D527AD626B049D657B5633AAAEBC694F3E33F9#)
 )
)"))
                                  %default-authorized-guix-keys)))))))
