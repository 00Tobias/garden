(define-module (home nyxt nyxt)
  #:use-module (guix gexp)

  #:use-module ((gnu packages web-browsers) #:select (nyxt))
  #:use-module ((gnu packages aspell) #:select (aspell
                                                aspell-dict-en
                                                aspell-dict-sv))
  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa)))

(define-public packages
  (let ((lst (list
              nyxt
              gstreamer
              aspell
              aspell-dict-en
              aspell-dict-sv)))
    (if (string= (gethostname) "okarthel")
        (map replace-mesa lst)
        lst)))

(define-public services
  (list
   (if (string= (gethostname) "okarthel")
       (simple-service 'nyxt-nvidia-env-vars
                       home-environment-variables-service-type
                       `(("WEBKIT_DISABLE_COMPOSITING_MODE" . "1"))))
   (simple-service 'nyxt-config
                   home-files-service-type
                   `((".config/nyxt/config.lisp" ,(local-file "config/config.lisp"))))))
