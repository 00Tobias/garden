(define-module (system hosts genera)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu system)
  #:use-module (gnu system shadow) ;; user-account
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages linux) #:select (efibootmgr))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  ;; #:use-module ((gnu packages xorg) #:select (xorg-server))
  ;; #:use-module ((gnu packages syncthing) #:select (syncthing))
  #:use-module (gnu packages certs)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  ;; #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)

  #:use-module (nongnu services nvidia)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  ;; #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system xorg) #:prefix xorg:))

;; (define transform
;;   (options->transformation
;;    '((with-graft . "mesa=nvda"))))

(operating-system
 (kernel linux-lts)
 ;; (kernel-arguments (append '("modprobe.blacklist=nouveau")
 ;;                           %default-kernel-arguments))
 ;; (kernel-loadable-modules (list nvidia-module))
 (initrd microcode-initrd)
 (firmware (list
            i915-firmware
            intel-microcode
            iwlwifi-firmware
            ibt-hw-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Stockholm")
 (keyboard-layout (keyboard-layout "se" "nodeadkeys"))
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
  (append (list (service openssh-service-type)
                ;; (service nvidia-service-type)
                ;; (simple-service
                ;;  'custom-udev-rules udev-service-type
                ;;  (list nvidia-driver))
                ;; (service kernel-module-loader-service-type
                ;;          '("ipmi_devintf"
                ;;            "nvidia"
                ;;            "nvidia_modeset"
                ;;            "nvidia_uvm"))
                ;; (set-xorg-configuration
                ;;  (xorg-configuration
                ;;   (modules (cons* nvidia-driver %default-xorg-modules))
                ;;   (server (transform xorg-server))
                ;;   (drivers '("nvidia"))))
                )
          xorg:services
          ;; syncthing:services
          ))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)))

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
