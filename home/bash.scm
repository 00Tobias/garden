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
             ;; (aliases
             ;;  '(("grep" . "grep --color=auto")
             ;;    ("la" . "ls -hpla")
             ;;    ("ls" . "ls -hp --color=auto")
             ;;    ("diff" . "diff --color=auto")
             ;;    ("ip" . "ip -color=auto")))
             (bash-profile '("shopt -s autocd checkwinsize"
                             "set -o noclobber"))))))
