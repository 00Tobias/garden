(define-module (system hosts genera)
  #:use-module (guix gexp)

  #:use-module (gnu system)
  #:use-module (gnu system shadow) ;; user-account
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages linux) #:select (efibootmgr))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  #:use-module ((gnu packages syncthing) #:select (syncthing))
  #:use-module (gnu packages certs)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)

  #:use-module (nongnu packages linux)
  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system xorg) #:prefix xorg:))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list
            i915-firmware
            intel-microcode
            iwlwifi-firmware
            ibt-hw-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Stockholm")
 (keyboard-layout (keyboard-layout "se"))
 (host-name "genera")
 (users (cons* (user-account
                (name "tobias")
                (comment "Tobias")
                (group "users")
                (home-directory "/home/tobias")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "dialout")))
               %base-user-accounts))
 (packages
  (append
   (list nss-certs
         ;; syncthing
         )
   %base-packages))

 (services
  (append (list (service openssh-service-type))
          xorg:services
          syncthing:services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)))

 (mapped-devices (list (mapped-device
                        (source (uuid "759320e1-34f8-4bd0-8fe6-64148ddd2676"))
                        (target "cryptroot")
                        (type luks-device-mapping))))

 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "BCE4-B121" 'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device "/dev/mapper/cryptroot")
                       (type "ext4")
                       (dependencies mapped-devices))
                      %base-file-systems)))
