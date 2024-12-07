(define-module (home nyxt nyxt)
  #:use-module (guix gexp)

  #:use-module ((gnu packages web-browsers) #:select (nyxt))
  #:use-module ((gnu packages gstreamer) #:select (gstreamer))
  #:use-module ((gnu packages aspell) #:select (aspell
                                                aspell-dict-en
                                                aspell-dict-sv))
  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((trowel) #:select (replace-mesa)))

(define-public packages
  (let ((lst (list
              nyxt
              gstreamer
              aspell
              aspell-dict-en
              aspell-dict-sv)))
    (if (or (string= (gethostname) "okarthel")
            (string= (gethostname) "austrat"))
        (map replace-mesa lst)
        lst)))

(define-public services
  (append
   (list
    (simple-service 'nyxt-config
                    home-files-service-type
                    `((".config/nyxt/config.lisp" ,(local-file "config/config.lisp")))))
   (if (or (string= (gethostname) "okarthel")
           (string= (gethostname) "austrat"))
       (list
        (simple-service 'nyxt-nvidia-env-vars
                        home-environment-variables-service-type
                        `(("WEBKIT_DISABLE_COMPOSITING_MODE" . "1"))))
       '())))
