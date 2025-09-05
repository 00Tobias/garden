(define-module (system hosts austrat)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system pam)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages commencement) #:select (gcc-toolchain-15))
  #:use-module ((gnu packages shells) #:select (fish))
  #:use-module ((gnu packages selinux) #:select (libselinux))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages avahi) #:select (nss-mdns))
  #:use-module ((gnu packages base) #:select (glibc-utf8-locales))
  #:use-module ((gnu packages gnome) #:select (libsecret))
  #:use-module ((gnu packages glib) #:select (dbus-glib))
  #:use-module ((gnu packages file-systems) #:select (bees))

  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)

  #:use-module (nongnu packages linux)
  #:use-module ((nongnu packages nvidia) #:select (nvda nvidia-firmware nvidia-module))

  #:use-module (nongnu services nvidia)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((trowel) #:select (aggressively-optimize))
  #:use-module ((system common) #:prefix common:)
  #:use-module ((system udev) #:prefix udev:)
  #:use-module ((system network) #:prefix network:)
  #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system wayland) #:prefix wayland:)
  #:use-module ((home main) #:select (main-home)))

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
         (store          . "/gnu/store")
         (guix           . "/var/guix")
         (log            . "/var/log")
         (lib            . "/var/lib")
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

         (cache                . "/home/tobias/.cache")
         (gnupg                . "/home/tobias/.gnupg")
         (local-bin            . "/home/tobias/.local/bin")
         (local-lib            . "/home/tobias/.local/lib")
         (local-state          . "/home/tobias/.local/state")
         (m2                   . "/home/tobias/.m2")
         (mozilla              . "/home/tobias/.mozilla")
         (password-store       . "/home/tobias/.password-store")
         (ssh-home             . "/home/tobias/.ssh")
         (wine                 . "/home/tobias/.wine")

         (blender-config       . "/home/tobias/.config/blender")
         (btop-config          . "/home/tobias/.config/btop")
         (gimp-config          . "/home/tobias/.config/GIMP")
         (google-chrome-config . "/home/tobias/.config/google-chrome")
         (kdeconnect-config    . "/home/tobias/.config/kdeconnect")
         (nethack-config       . "/home/tobias/.config/nethack")
         (wesnoth-config       . "/home/tobias/.config/wesnoth")

         (nyxt-local           . "/home/tobias/.local/share/nyxt")
         (prismlauncher-local  . "/home/tobias/.local/share/PrismLauncher")
         (qutebrowser-local    . "/home/tobias/.local/share/qutebrowser")
         (steam-local          . "/home/tobias/.local/share/guix-sandbox-home")
         (wesnoth-local        . "/home/tobias/.local/share/wesnoth")
         (zrythm-local         . "/home/tobias/.local/share/zrythm")

         (auto-save-list-emacs . "/home/tobias/.emacs.d/auto-save-list")
         (eln-cache-emacs      . "/home/tobias/.emacs.d/eln-cache")
         (eshell-emacs         . "/home/tobias/.emacs.d/eshell"))))

(define etc-sudoers-config
  (plain-file "etc-sudoers-config"
              "Defaults  timestamp_timeout=480
root      ALL=(ALL) ALL
%wheel    ALL=(ALL) NOPASSWD:ALL
tobias    ALL=(ALL) NOPASSWD:/run/current-system/profile/bin/loginctl,/run/current-system/profile/bin/nmtui"))

(operating-system
  (kernel linux)
  (kernel-arguments (append '("mitigations=off" ; Live a little
                              "modprobe.blacklist=nouveau,pcspkr"
                              "nvidia.NVreg_DynamicPowerManagement=0x02"
                              "quiet")
                            %default-kernel-arguments))
  (initrd microcode-initrd)
  (firmware (list linux-firmware
                  intel-microcode
                  iwlwifi-firmware
                  ibt-hw-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "us" "altgr-intl" #:options '("ctrl:nocaps")))
  (host-name "austrat")
  (sudoers-file etc-sudoers-config)
  (users (cons* (user-account
                 (name "tobias")
                 (comment "Tobias")
                 (group "users")
                 (shell (file-append fish "/bin/fish"))
                 (home-directory "/home/tobias")
		 (password "")
                 (supplementary-groups
                  '("wheel"
                    "netdev"
                    "audio"
                    "video"
                    "dialout"
                    "plugdev"
                    "adbusers"
                    "kvm")))
                %base-user-accounts))

  (packages
   (append
    (list
     git
     openssh
     nss-mdns

     glibc-utf8-locales
     dbus-glib
     libsecret
     libselinux)
    %base-packages))

  (services
   (append
    common:services
    udev:services
    network:services
    syncthing:services
    wayland:services
    (list
     (service earlyoom-service-type
              (earlyoom-configuration
               (minimum-available-memory 5)
               (minimum-free-swap 100)))
     (service fstrim-service-type)
     (service upower-service-type)
     (service tlp-service-type)
     (service pcscd-service-type)
     (service nvidia-service-type)

     fontconfig-file-system-service
     (service udisks-service-type)
     (service polkit-service-type)
     (service dbus-root-service-type)
     (service pam-limits-service-type
              (list (pam-limits-entry "*" 'both 'memlock 'unlimited)))

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
                                          "/home/tobias/.config/guix/current"))))))

     (service guix-home-service-type
     	      `(("tobias" ,main-home)))

     ;; (simple-service 'bees-etc-service etc-service-type
     ;;                 (list `("bees/beesd.conf"
     ;;                         ,(plain-file "beesd.conf"
     ;;                                      "UUID=7d0ea08e-b8ea-44b3-9d0e-3d48165cae9e\n"))))
     ;; (simple-service 'beesd-shepherd-service shepherd-root-service-type
     ;;                 (list
     ;;                  (shepherd-service
     ;;                   (documentation "beesd")
     ;;                   (requirement '(file-systems))
     ;;                   (provision '(beesd))
     ;;                   (start
     ;;                    #~(make-forkexec-constructor
     ;;                       (list #$(file-append bees "/sbin/beesd")
     ;;                             "--no-timestamps"
     ;;                             "--strip-paths"
     ;;                             "--verbose=0"
     ;;                             "7d0ea08e-b8ea-44b3-9d0e-3d48165cae9e")
     ;;                       #:environment-variables
     ;;                       (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
     ;;                   (stop #~(make-kill-destructor)))))
     )))

  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout keyboard-layout)
    (theme (grub-theme
            (gfxmode '("auto"))
            (color-normal    '((fg . white) (bg . black)))
            (color-highlight '((fg . black) (bg . white)))
            (image (local-file "wallpapers/grub-austrat.png"))))))

  (file-systems
   (append
    (list
     (file-system
       (mount-point "/")
       (type "tmpfs")
       (device "none")
       (needed-for-boot? #t)
       (check? #f)
       (flags '(no-atime))
       (options "size=12G"))

     (file-system
       (mount-point "/boot/efi")
       (device (uuid "" 'fat32))
       (type "vfat"))

     %root-subvolumes
     %home-subvolumes

     %base-file-systems))))
