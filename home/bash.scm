(define-module (home bash)
  #:use-module (guix gexp)

  #:use-module (gnu services)

  #:use-module (gnu home services shells))

(define-public services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (aliases
              '(("grep" . "grep --color=auto")
                ("la"   . "ls -hpla")
                ("ls"   . "ls -hp --color=auto")
                ("diff" . "diff --color=auto")
                ("ip"   . "ip -color=auto")
                ("gsr"  . "sudo guix system reconfigure -c $(nproc) -L ~/garden ~/garden/system/hosts/$HOSTNAME.scm")
                ("ghr"  . "guix home reconfigure -c $(nproc) -L ~/garden ~/garden/home/core.scm")))
             (bash-profile (list (plain-file "bash_profile"
                                             "shopt -s autocd checkwinsize
set -o noclobber")))))))
