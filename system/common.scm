(define-module (system common)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services guix))

(define-public services
  (append
   (modify-services %base-services
     (guix-service-type config =>
                        (guix-configuration
                         (inherit config)
                         (substitute-urls
                          (append (list "https://substitutes.nonguix.org" "https://guix.bordeaux.inria.fr")
                                  %default-substitute-urls))
                         (authorized-keys
                          (append (list
                                   (plain-file "nonguix-signing-key.pub"
                                               "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
                                   (plain-file "guix-science-signing-key.pub"
                                               "(public-key (ecc (curve Ed25519) (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))"))
                                  %default-authorized-guix-keys)))))))
