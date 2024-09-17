(define-module (system hosts okarthel)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system pam)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages linux) #:select (linux-pam))
  #:use-module ((gnu packages selinux) #:select (libselinux))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  #:use-module ((gnu packages display-managers) #:select (slim))
  #:use-module ((gnu packages xorg) #:select (xorg-server))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages base) #:select (glibc-utf8-locales))
  #:use-module ((gnu packages gnome) #:select (libsecret))
  #:use-module ((gnu packages glib) #:select (dbus-glib))
  #:use-module ((gnu packages file-systems) #:select (bees))

  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)

  #:use-module (nongnu services nvidia)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((system common) #:prefix common:)
  #:use-module ((system udev) #:prefix udev:)
  #:use-module ((system network) #:prefix network:)
  #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system xorg) #:prefix xorg:)
  #:use-module ((system wayland) #:prefix wayland:)
  #:use-module ((home main) #:select (main-home)))

(define %leetmouse-config
  (plain-file "config.h"
              "#define BUFFER_SIZE 8

#define SCROLLS_PER_TICK 3.0f

#define SENSITIVITY 0.4f
#define ACCELERATION 0.7f
#define SENS_CAP 4.0f
#define OFFSET 0.0f
#define POST_SCALE_X 0.4f
#define POST_SCALE_Y 0.4f
#define SPEED_CAP 0.0f

#define PRE_SCALE_X 0.25f
#define PRE_SCALE_Y 0.25f"))

(define leetmouse-module
  (package
    (name "leetmouse-module")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/systemofapwne/leetmouse")
             (commit "fdea8f70ba6eaa4c885d2f3023202e86726c2aeb")))
       (sha256 (base32 "03zqqkzm69fqdxhqv1g7d98kkbp86k5cllvkqw4bh2hzz5kradlk"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f
       #:source-directory "driver"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'inject-config-header
           (lambda _
             (copy-file ,%leetmouse-config "driver/config.h"))))))
    (home-page "https://github.com/systemofapwne/leetmouse")
    (synopsis "A fork of the Linux mouse driver with acceleration")
    (description "A fork of the Linux mouse driver with acceleration")
    (license license:gpl2+)))

(define leetmouse-udev-rules
  (package
    (name "leetmouse-udev-rules")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/systemofapwne/leetmouse")
             (commit "fdea8f70ba6eaa4c885d2f3023202e86726c2aeb")))
       (sha256 (base32 "03zqqkzm69fqdxhqv1g7d98kkbp86k5cllvkqw4bh2hzz5kradlk"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("install_files/udev/99-leetmouse.rules" "lib/udev/rules.d/")
         ("install_files/udev/leetmouse_bind"     "lib/udev/rules.d/")
         ("install_files/udev/leetmouse_manage"   "lib/udev/rules.d/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "install_files/udev/leetmouse_bind"
                 (("PATH='/sbin:/bin:/usr/sbin:/usr/bin'")
                  ""))
               (substitute* "install_files/udev/99-leetmouse.rules"
                 (("leetmouse_bind")
                  (string-append out "/lib/udev/rules.d/leetmouse_bind")))))))))
    (home-page "https://github.com/systemofapwne/etmouse")
    (synopsis "Udev rules for leetmouse")
    (description "This contains the udev rules for leetmouse.")
    (license license:gpl2+)))

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
  (kernel linux-xanmod)
  (kernel-arguments (append '("mitigations=off" ; Live a little
                              "modprobe.blacklist=nouveau,pcspkr"
                              "nvidia_drm.modeset=1"
                              "quiet")
                            %default-kernel-arguments))
  ;; (kernel-loadable-modules (list leetmouse-module))
  (initrd microcode-initrd)
  (firmware (list linux-firmware amd-microcode))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "se" "nodeadkeys" #:options '("ctrl:nocaps")))
  (host-name "okarthel")
  (sudoers-file etc-sudoers-config)
  (users (cons* (user-account
                 (name "tobias")
                 (comment "Tobias")
                 (group "users")
                 (home-directory "/home/tobias")
		 (password "$6$2iVqBpcHrXOuNbJc$CRvuQT7LX0ArLQXV.JMhsCuEaOaBIcWbyol9ugcHFSZdriINStBP/iwXqaFau.DR09x2P4f.ew.j7yeelsH5m/")
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
    (map replace-mesa
         (list
          git
          openssh

          glibc-utf8-locales
          dbus-glib
          libsecret
          libselinux))
    %base-packages))

  (services
   (append
    common:services
    udev:services
    network:services
    syncthing:services
    (modify-services xorg:services
      (slim-service-type
       config =>
       (slim-configuration
        (inherit config)
        (slim (replace-mesa slim))
        (xorg-configuration
         (xorg-configuration
          (keyboard-layout keyboard-layout)
          (modules (cons* nvda %default-xorg-modules))
          (server (replace-mesa xorg-server))
          (drivers '("nvidia"))
          (extra-config (list xorg:%libinput-config
                              xorg:%nvidia-config)))))))
    (list
     (service earlyoom-service-type
              (earlyoom-configuration
               (minimum-available-memory 5)
               (minimum-free-swap 100)))
     (service pcscd-service-type)
     (service nvidia-service-type)
     ;; (udev-rules-service 'leetmouse leetmouse-udev-rules)

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
     (simple-service 'bees-etc-service etc-service-type
                     (list `("bees/beesd.conf"
                             ,(plain-file "beesd.conf"
                                          "UUID=7d0ea08e-b8ea-44b3-9d0e-3d48165cae9e\n"))))
     (simple-service 'beesd-shepherd-service shepherd-root-service-type
                     (list
                      (shepherd-service
                       (documentation "beesd")
                       (requirement '(file-systems))
                       (provision '(beesd))
                       (start
                        #~(make-forkexec-constructor
                           (list #$(file-append bees "/sbin/beesd")
                                 "--no-timestamps"
                                 "--strip-paths"
                                 "--verbose=0"
                                 "7d0ea08e-b8ea-44b3-9d0e-3d48165cae9e")
                           #:environment-variables
                           (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
                       (stop #~(make-kill-destructor))))))))

  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout keyboard-layout)
    (theme (grub-theme
            (gfxmode '("auto"))
            (color-normal    '((fg . white) (bg . black)))
            (color-highlight '((fg . black) (bg . white)))
            (image (local-file "wallpapers/grub-okarthel.png"))))))

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
       (device (uuid "C1E3-56B8" 'fat32))
       (type "vfat"))

     (file-system
       (mount-point "/bulk/")
       (device (uuid "2a2dd272-52f8-42b3-ba30-46aee21d75f4" 'ext4))
       (flags '(no-atime))
       (type "ext4")))

    %root-subvolumes
    %home-subvolumes

    %base-file-systems)))
