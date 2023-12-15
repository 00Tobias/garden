(define-module (system hosts okarthel)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (guix packages)

  #:use-module (gnu system)
  #:use-module (gnu system shadow) ;; user-account
  #:use-module (gnu system setuid)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages linux) #:select (efibootmgr
                                               ntfs-3g))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  #:use-module ((gnu packages xorg) #:select (xorg-server))
  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module ((gnu packages freedesktop) #:select (flatpak-xdg-utils
                                                     xdg-desktop-portal-gtk))
  #:use-module ((gnu packages glib) #:select (xdg-dbus-proxy))
  #:use-module ((gnu packages base) #:select (glibc-utf8-locales))
  #:use-module ((gnu packages libusb) #:select (libmtp))
  #:use-module ((gnu packages nfs) #:select (nfs-utils))
  #:use-module ((gnu packages gnome) #:select (network-manager-applet))

  ;; TEMP
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages selinux)

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services lightdm)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)

  #:use-module (nongnu services nvidia)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((system udev) #:prefix udev:)
  #:use-module ((system network) #:prefix network:)
  #:use-module ((system syncthing) #:prefix syncthing:)
  #:use-module ((system xorg) #:prefix xorg:)
  #:use-module ((system wayland) #:prefix wayland:))

(define etc-sudoers-config
  (plain-file "etc-sudoers-config"
              "Defaults  timestamp_timeout=480
root      ALL=(ALL) ALL
%wheel    ALL=(ALL) NOPASSWD:ALL
tobias    ALL=(ALL) NOPASSWD:/run/current-system/profile/bin/chvt,/run/current-system/profile/bin/loginctl"))

(operating-system
 (kernel linux-xanmod-lts)
 (kernel-arguments (append '("video=DP-1:3440x1440@144"
                             "mitigations=off" ; Live a little
                             "modprobe.blacklist=nouveau,pcspkr"
                             "nvidia_drm.modeset=1"
                             "quiet")
                           %default-kernel-arguments))
 (kernel-loadable-modules (list nvidia-module))
 (initrd microcode-initrd)
 (firmware (list linux-firmware amd-microcode nvidia-firmware))
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
                (supplementary-groups
                 '("wheel"
                   "netdev"
                   "audio"
                   "video"
                   "dialout"
                   "adbusers"
                   "kvm")))
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
    xdg-dbus-proxy

    glibc-utf8-locales
    ;; gtk+
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
                        (modules (cons* nvidia-driver %default-xorg-modules))
                        (server (replace-mesa xorg-server))
                        (drivers '("nvidia"))
                        (extra-config (list xorg:%libinput-config
                                            xorg:%nvidia-config)))))))
   (list
    (service pcscd-service-type)
    (service nvidia-service-type)

    fontconfig-file-system-service

    ;; (simple-service 'network-manager-applet
    ;;                 profile-service-type
    ;;                 (list network-manager-applet))
    (service udisks-service-type)
    (service polkit-service-type)
    (service dbus-root-service-type))
   (modify-services %base-services
	                  (guix-service-type config =>
					                             (guix-configuration
					                              (inherit config)
					                              (substitute-urls
						                             (append (list "https://substitutes.nonguix.org")
							                                   %default-substitute-urls))
					                              (authorized-keys
						                             (append (list (plain-file "nonguix.pub"
                                                                   "
(public-key (ecc (curve Ed25519)
                 (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))
"))
							                                   %default-authorized-guix-keys)))))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)
   (theme (grub-theme
           (gfxmode '("auto"))
           (color-normal '((fg . white) (bg . black)))
           (color-highlight '((fg . black) (bg . white)))
           (image (local-file "wallpapers/grub-okarthel.png"))))))

 (swap-devices (list (swap-space
                      (target (uuid "e58f0e61-1094-4cfe-9ec3-faa3da768de0")))))

 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "C1E3-56B8"
                                     'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (uuid
                                "6879250f-7b25-4bc5-a686-9595f5669d73"
                                'ext4))
                       (type "ext4"))
                      (file-system
                       (mount-point "/bulk/")
                       (device (uuid
                                "2a2dd272-52f8-42b3-ba30-46aee21d75f4"
                                'ext4))
                       (type "ext4"))
                      %base-file-systems)))
