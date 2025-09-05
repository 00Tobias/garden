(define-module (home shell)
  #:use-module (guix gexp)

  #:use-module ((gnu packages shellutils) #:select (direnv))
  #:use-module ((gnu packages rust-apps) #:select (tealdeer fd ripgrep))
  #:use-module ((gnu packages admin) #:select (du-dust))
  #:use-module ((gnu packages terminals) #:select (fzf))

  #:use-module (gnu services)

  #:use-module (gnu home services)
  #:use-module (gnu home services shells))

(define-public packages
  (list direnv
        tealdeer
        du-dust
        fd
        ripgrep))

(define-public services
  (list
   (simple-service 'shell-xdg-config home-xdg-configuration-files-service-type
                   `(("direnv/direnv.toml" ,(mixed-text-file
                                             "direnv-config"
                                             "
[whitelist]
prefix = [ \"/home/tobias/projects/\" ]
"))))
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
" direnv "/bin/direnv hook fish | source
")))))))
