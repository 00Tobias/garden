(define-module (home shell)
  #:use-module (guix gexp)

  #:use-module ((gnu packages terminals) #:select (fzf))

  #:use-module (gnu services)

  #:use-module (gnu home services shells))

(define-public services
  (list
   (service
    home-fish-service-type
    (home-fish-configuration
     (aliases
      '(("grep" . "grep --color=auto")
        ("la"   . "ls -hpla")
        ("ls"   . "ls -hp --color=auto")
        ("diff" . "diff --color=auto")
        ("ip"   . "ip -color=auto")))
     (abbreviations
      '(("gsr"  . "-- sudo guix system reconfigure -c $(nproc) -L ~/garden ~/garden/system/hosts/(hostname).scm")
        ("ghr"  . "-- guix home reconfigure -c $(nproc) -L ~/garden ~/garden/home/main.scm")))
     (environment-variables
      '(("LESS"  . "-R --use-color -Dd+r$Du+b")))
     (config (list (mixed-text-file "fish-config" "
set -g fish_greeting
source " fzf "/share/fish/vendor_functions.d/fzf_key_bindings.fish
")))))))
