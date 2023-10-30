(define-module (system network)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu services dns)
  #:use-module (gnu services networking))

(define etc-resolv-conf
  (plain-file "etc-resolv-conf"
              "nameserver 127.0.0.1
options trust-ad
"))

(define-public services
  (list
   (simple-service 'resolv-service
                   etc-service-type
                   `(("resolv.conf" ,etc-resolv-conf)))
   (service network-manager-service-type
            (network-manager-configuration
             (dns "none")))
   (service dnsmasq-service-type
            (dnsmasq-configuration
             (no-resolv? #t)
             (query-servers-in-order? #t)
             (servers '("45.90.30.0"
                        "45.90.28.0"))
             (cpe-id "1dc65b")))))
