(define-module (system impermanence)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)

  #:use-module (gnu system file-systems)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services guix))

;; Adapted from https://git.sr.ht/~ngraves/dotfiles/tree/main/item/make
(define get-btrfs-file-system
  (match-lambda
    ((subvol . mount-point)
     (file-system
       (type "btrfs")
       (device (file-system-label "system"))
       (mount-point mount-point)
       (create-mount-point? #t)
       (flags '(no-atime))
       (options (string-append "compress-force=zstd:1,discard=async,subvol=" (symbol->string subvol)))
       (needed-for-boot? (or (string=? "/gnu/store" mount-point)
                             (string=? "/var/guix"  mount-point)
                             (string=? "/boot"      mount-point)))))))

(define %root-subvolumes
  (map get-btrfs-file-system
       '((boot           . "/boot")
         (root           . "/root")
         (store          . "/gnu/store")
         (guix           . "/var/guix")
         (log            . "/var/log")
         (lib            . "/var/lib")
         (tmp            . "/var/tmp")
         (networkmanager . "/etc/NetworkManager")
         (ssh-etc        . "/etc/ssh"))))

(define %home-subvolumes
  (map get-btrfs-file-system
       '((ai                   . "/home/tobias/ai")
         (common-lisp          . "/home/tobias/common-lisp")
         (documents            . "/home/tobias/Documents")
         (garden               . "/home/tobias/garden")
         (git                  . "/home/tobias/git")
         (irthir               . "/home/tobias/irthir")
         (music                . "/home/tobias/Music")
         (pictures             . "/home/tobias/Pictures")
         (projects             . "/home/tobias/projects")

         (cabal                . "/home/tobias/.cabal")
         (cache                . "/home/tobias/.cache")
         (gnupg                . "/home/tobias/.gnupg")
         (librewolf            . "/home/tobias/.librewolf")
         (local-bin            . "/home/tobias/.local/bin")
         (local-lib            . "/home/tobias/.local/lib")
         (local-state          . "/home/tobias/.local/state")
         (m2                   . "/home/tobias/.m2")
         (mozilla              . "/home/tobias/.mozilla")
         (opam                 . "/home/tobias/.opam")
         (password-store       . "/home/tobias/.password-store")
         (ssh-home             . "/home/tobias/.ssh")
         (var-home             . "/home/tobias/.var")
         (wine                 . "/home/tobias/.wine")

         (blender-config       . "/home/tobias/.config/blender")
         (btop-config          . "/home/tobias/.config/btop")
         (gimp-config          . "/home/tobias/.config/GIMP")
         (godot-config         . "/home/tobias/.config/godot")
         (kdeconnect-config    . "/home/tobias/.config/kdeconnect")
         (nethack-config       . "/home/tobias/.config/nethack")
         (qt6ct-config         . "/home/tobias/.config/qt6ct")
         (strawberry-config    . "/home/tobias/.config/strawberry")
         (vesktop-config       . "/home/tobias/.config/vesktop")
         (wesnoth-config       . "/home/tobias/.config/wesnoth")

         (fish-local           . "/home/tobias/.local/share/fish")
         (flatpak-local        . "/home/tobias/.local/share/flatpak")
         (godot-local          . "/home/tobias/.local/share/godot")
         (nyxt-local           . "/home/tobias/.local/share/nyxt")
         (prismlauncher-local  . "/home/tobias/.local/share/PrismLauncher")
         (qutebrowser-local    . "/home/tobias/.local/share/qutebrowser")
         (steam-local          . "/home/tobias/.local/share/guix-sandbox-home")
         (strawberry-local     . "/home/tobias/.local/share/strawberry")
         (wesnoth-local        . "/home/tobias/.local/share/wesnoth")
         (zrythm-local         . "/home/tobias/.local/share/zrythm")

         (auto-save-list-emacs . "/home/tobias/.emacs.d/auto-save-list")
         (eln-cache-emacs      . "/home/tobias/.emacs.d/eln-cache")
         (eshell-emacs         . "/home/tobias/.emacs.d/eshell"))))

(define-public services
  (list
   (simple-service 'fixup-home activation-service-type
		   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))
                         (let* ((user (getpw "tobias"))
                                (directories '("/home/tobias"
                                               "/home/tobias/.config"
                                               "/home/tobias/.config/guix"
                                               "/home/tobias/.local"
                                               "/home/tobias/.local/share"
                                               "/home/tobias/.local/state"
                                               "/home/tobias/.emacs.d")))
                           (for-each mkdir-p directories)
                           (for-each (lambda (dir)
                                       (chown dir (passwd:uid user) (passwd:gid user)))
                                     directories)
                           (if (not (file-exists? "/home/tobias/.config/guix/current"))
                               (symlink "/var/guix/profiles/per-user/tobias/current-guix"
                                        "/home/tobias/.config/guix/current"))))))))

(define-public file-systems
  (append
   (list (file-system
           (mount-point "/")
           (type "tmpfs")
           (device "none")
           (needed-for-boot? #t)
           (check? #f)
           (flags '(no-atime))
           (options "size=12G")))
   %root-subvolumes
   %home-subvolumes))
