(define-module (system hosts haven)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1) ;; 'remove' keyword

  #:use-module (gnu system)
  #:use-module (gnu system shadow) ;; user-account
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module (gnu packages linux)       ;; efibootmgr
  #:use-module (gnu packages bootloaders) ;; GRUB
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages version-control)

  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sddm)
  #:use-module (gnu services desktop)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)

  #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system xorg) #:prefix xorg:))

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list
            i915-firmware     ;; Intel graphics firmware
            intel-microcode   ;; Unsure if needed
            iwlwifi-firmware  ;; Intel wifi firmware
            ibt-hw-firmware)) ;; Intel bluetooth firmware
 (locale "en_US.utf8")
 (timezone "Europe/Stockholm")
 (keyboard-layout (keyboard-layout "se"))
 (host-name "haven")
 (users (cons* (user-account
                (name "toxic")
                (comment "Toxic")
                (group "users")
                (home-directory "/home/toxic")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "dialout")))
               %base-user-accounts))
 (packages
  (append
   xorg:packages
   (list
    xf86-input-libinput ;; More modern input method
    os-prober           ;; Detect other operating systems
    efibootmgr          ;; rEFInd
    git
    ;; HTTPS access
    nss-certs)
   %base-packages))

 (services (append
            ;; Modify xorg services with host specific keyboard-layout
            (modify-services xorg:services
                             (xorg-server-service-type config =>
                                                       (xorg-configuration
                                                        (inherit config)
                                                        (keyboard-layout keyboard-layout))))

            syncthing:services))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)))

 (mapped-devices
  (list (mapped-device
         (source (uuid "b409c038-44fc-488a-85c3-2be96b03d9c6"))
         (target "enigma")
         (type luks-device-mapping))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/mapper/enigma")
          (type "ext4")
          (dependencies mapped-devices))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "E2DD-71C1" 'fat32))
          (type "vfat"))
         %base-file-systems)))
