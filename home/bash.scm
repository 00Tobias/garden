(define-module (home bash)
  #:use-module (guix gexp)

  #:use-module (gnu services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services base))

(define-public services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (bash-profile '("export HISTFILE=$XDG_CACHE_HOME/.bash_history"
                             "export PATH=$PATH:~/.bin"
                             "export GTK_THEME=Adwaita:dark"))))))
