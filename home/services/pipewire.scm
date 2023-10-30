;; From https://git.sr.ht/~akagi/guixrc/tree/master/item/magi/home/services/pipewire.scm
(define-module (home services pipewire)
  #:use-module (guix gexp)
  #:use-module ((gnu packages linux) #:select (pipewire wireplumber))
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd))

(define (home-pipewire-files-service _)
  `(("alsa/asoundrc"
     ,(mixed-text-file
       "asoundrc"
       #~(string-append
          "<"
          #$(file-append pipewire "/share/alsa/alsa.conf.d/50-pipewire.conf")
          ">\n<"
          #$(file-append pipewire "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
          ">\n"
          "
pcm_type.pipewire {
  lib " #$(file-append pipewire "/lib/alsa-lib/libasound_module_pcm_pipewire.so") "
}

ctl_type.pipewire {
  lib " #$(file-append pipewire "/lib/alsa-lib/libasound_module_ctl_pipewire.so") "
}")))))

(define (home-pipewire-package-service _)
  (list pipewire wireplumber))

(define (home-pipewire-shepherd-service _)
  (list
   (shepherd-service
    (requirement '(dbus))
    (provision '(pipewire))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire "/bin/pipewire"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pipewire.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))
   (shepherd-service
    (requirement '(pipewire))
    (provision '(wireplumber))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append wireplumber "/bin/wireplumber"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/wireplumber.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))
   (shepherd-service
    (requirement '(pipewire))
    (provision '(pipewire-pulse))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire
                                   "/bin/pipewire-pulse"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pipewire-pulse.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))))

(define (home-pipewire-environment-variables-service _)
  '(("RTC_USE_PIPEWIRE" . "true")))

(define-public home-pipewire-service-type
  (service-type
   (name 'home-pipewire)
   (extensions
    (list (service-extension
           home-xdg-configuration-files-service-type
           home-pipewire-files-service)
          (service-extension
           home-profile-service-type
           home-pipewire-package-service)
          (service-extension
           home-shepherd-service-type
           home-pipewire-shepherd-service)
          (service-extension
           home-environment-variables-service-type
           home-pipewire-environment-variables-service)))
   (default-value #f)
   (description "Run pipewire.")))
