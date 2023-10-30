(define-module (home qutebrowser qutebrowser)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix git-download)

  #:use-module ((gnu packages web-browsers) #:select (qutebrowser))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define-public packages
  (list (replace-mesa qutebrowser)))

;; (define-public services
;;   (list
;;    (simple-service 'qutebrowser-config
;;                    home-files-service-type
;;                    `((".config/qutebrowser/config.py"    . "1")))))
