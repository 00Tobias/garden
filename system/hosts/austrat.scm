(define-module (system hosts austrat)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:use-module (gnu system)
  #:use-module (gnu system shadow) ;; user-account
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module ((gnu packages freedesktop) #:select (flatpak-xdg-utils
                                                     xdg-desktop-portal-gtk))
  #:use-module ((gnu packages glib) #:select (xdg-dbus-proxy))

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module ((gnu services desktop) #:select (upower-service-type))
  #:use-module ((gnu services pm) #:select (tlp-service-type))
  #:use-module ((gnu services security-token) #:select (pcscd-service-type))

  #:use-module (nongnu packages linux)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((system udev) #:prefix udev:)
  #:use-module ((system network) #:prefix network:)
  #:use-module ((system xorg) #:prefix xorg:))

(define etc-sudoers-config
  (plain-file "etc-sudoers-config"
              "Defaults  timestamp_timeout=480
root      ALL=(ALL) ALL
%wheel    ALL=(ALL) NOPASSWD:ALL
tobias    ALL=(ALL) NOPASSWD:/run/current-system/profile/bin/chvt,/run/current-system/profile/bin/loginctl"))

(operating-system
 (kernel linux)
 (kernel-arguments (append '("quiet") %default-kernel-arguments))
 (initrd microcode-initrd)
 (firmware (list
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
                 '("wheel" "netdev" "audio" "video" "dialout")))
               %base-user-accounts))

 (packages
  (append
   (list
    nss-certs
    git
    openssh

    flatpak
    flatpak-xdg-utils
    xdg-desktop-portal-gtk
    xdg-dbus-proxy)
   %base-packages))

 (services
  (append
   udev:services
   network:services
   (list
    (service upower-service-type)
    (service tlp-service-type)
    (service pcscd-service-type))
   (modify-services xorg:services
	                  (guix-service-type config =>
					                             (guix-configuration
					                              (inherit config)
					                              (substitute-urls
						                             (append (list "https://substitutes.nonguix.org")
							                                   %default-substitute-urls))
					                              (authorized-keys
						                             (append (list (local-file "../../nonguix-signing-key.pub"))
							                                   %default-authorized-guix-keys))))
                    (slim-service-type config =>
                                       (slim-configuration
                                        (inherit config)
                                        (xorg-configuration
                                         (xorg-configuration
                                          (keyboard-layout keyboard-layout)
                                          (extra-config (list xorg:%libinput-config)))))))))

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
