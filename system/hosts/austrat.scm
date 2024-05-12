(define-module (system hosts austrat)
  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system pam)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages base) #:select (glibc-utf8-locales))
  #:use-module ((gnu packages selinux) #:select (libselinux))
  #:use-module ((gnu packages gnome) #:select (libsecret))
  #:use-module ((gnu packages glib) #:select (dbus-glib))

  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services dbus)
  #:use-module ((gnu services desktop) #:select (upower-service-type))
  #:use-module ((gnu services pm) #:select (tlp-service-type))
  #:use-module ((gnu services security-token) #:select (pcscd-service-type))

  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)

  #:use-module (nongnu services nvidia)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((system udev) #:prefix udev:)
  #:use-module ((system network) #:prefix network:)
  #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system xorg) #:prefix xorg:)
  #:use-module ((home main) #:select (main-home)))

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
                              "quiet")
                            %default-kernel-arguments))
  (initrd microcode-initrd)
  (firmware (list
             linux-firmware
             i915-firmware
             intel-microcode
             iwlwifi-firmware
             ibt-hw-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Stockholm")
  (keyboard-layout (keyboard-layout "se" "nodeadkeys" #:options '("ctrl:nocaps")))
  (host-name "austrat")
  (sudoers-file etc-sudoers-config)
  (users (cons* (user-account
                 (name "tobias")
                 (comment "Tobias")
                 (group "users")
                 (home-directory "/home/tobias")
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

     glibc-utf8-locales
     dbus-glib
     libsecret
     libselinux)
    %base-packages))

  (services
   (append
    udev:services
    network:services
    syncthing:services
    (modify-services xorg:services
      (slim-service-type
       config =>
       (slim-configuration
        (inherit config)
        (xorg-configuration
         (xorg-configuration
          (keyboard-layout keyboard-layout)
          (modules (cons* nvda %default-xorg-modules))
          (extra-config (list xorg:%libinput-config)))))))
    (list
     (service earlyoom-service-type)
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

     (service guix-home-service-type
     	      `(("tobias" ,main-home))))
    (modify-services %base-services
      (guix-service-type config =>
			 (guix-configuration
                          (inherit config)
                          (substitute-urls
                           (append (list "https://substitutes.nonguix.org" "https://guix.bordeaux.inria.fr")
                                   %default-substitute-urls))
                          (authorized-keys
                           (append (list
                                    (plain-file "nonguix-signing-key.pub"
                                                "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
                                    (plain-file "guix-science-signing-key.pub"
                                                "(public-key (ecc (curve Ed25519) (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))"))
                                   %default-authorized-guix-keys)))))))

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

  (mapped-devices (list (mapped-device
                         (source (uuid "e79d1abc-35de-4eb8-959b-8d657e35566b"))
                         (target "cryptroot")
                         (type luks-device-mapping))))

  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "7E96-D0AD" 'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices))
                       %base-file-systems)))
