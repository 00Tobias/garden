;;; From https://git.sr.ht/~akagi/guixrc/tree/master/item/magi/home/services/notification.scm
;;; with my own changes

(define-module (home services dunst)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)

  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services-utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix import utils)

  #:use-module (gnu packages dunst)
  #:export (home-dunst-configuration
            home-dunst-service-type))

(define-configuration/no-serialization home-dunst-configuration
  (package
    (package dunst)
    "Dunst package to use")
  (config
   (ini-config '())
   ""))

(define (serialize-dunst-config config)

  (define (serialize-boolean val)
    (list (boolean->true-or-false val)))

  (define (serialize-list val)
    (interpose (map serialize-val val) ", "))

  (define (serialize-val val)
    (cond
     ((list? val) (serialize-list val))
     ((boolean? val) (serialize-boolean val))
     ((or (number? val) (symbol? val)) (list (maybe-object->string val)))
     (else (list val))))

  (define (serialize-field key val)
    (let ((val (serialize-val val))
          (key (symbol->string key)))
      `(,key "=" ,@val "\n")))

  (flatten (generic-serialize-ini-config
            #:combine-ini interpose
            #:combine-alist list
            #:combine-section-alist cons
            #:serialize-field serialize-field
            #:fields config)))

(define (home-dunst-shepherd-service _)
  (list
   (shepherd-service
    ;; (requirement '(dbus-home))
    (provision '(dunst))
    (auto-start? #f)
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append dunst "/bin/dunst")
                    (string-append
                     "-conf "
                     (getenv "XDG_CONFIG_HOME") "/dunst/dunstrc")))))))

(define (add-dunst-configuration config)
  (let ((cfg (home-dunst-configuration-config config)))
    `(("config/dunst/dunstrc"
       ,(apply mixed-text-file
               "config"
               (serialize-dunst-config cfg))))))

(define add-dunst-package
  (compose list home-dunst-configuration-package))

(define home-dunst-service-type
  (service-type
   (name 'home-dunst)
   (extensions
    (list (service-extension
           home-files-service-type
           add-dunst-configuration)
          ;; (service-extension
          ;;  home-environment-variables-service-type
          ;;  home-dunst-environment-variables-service)
          (service-extension
           home-shepherd-service-type
           home-dunst-shepherd-service)
          (service-extension
           home-profile-service-type
           add-dunst-package)))
   (compose identity)
   (default-value (home-dunst-configuration))
   (description "Configure the Dunst notification daemon")))
