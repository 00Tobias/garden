(define-module (system hosts okarthel)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix transformations)
  #:use-module (guix packages)

  #:use-module (gnu system)
  #:use-module (gnu system shadow) ;; user-account
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)

  #:use-module ((gnu packages linux) #:select (efibootmgr))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  #:use-module ((gnu packages xorg) #:select (xinit xorg-server xf86-input-libinput))
  #:use-module ((gnu packages certs) #:select (nss-certs))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages gl) #:select (libglvnd))
  #:use-module ((gnu packages vulkan) #:select (vulkan-loader))
  #:use-module ((gnu packages xdisorg) #:select (xclip))
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module ((gnu packages freedesktop) #:select (flatpak-xdg-utils
                                                     xdg-desktop-portal-gtk))
  #:use-module ((gnu packages glib) #:select (xdg-dbus-proxy))
  #:use-module ((gnu packages android) #:select (android-udev-rules))

  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services security-token)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services cups)
  #:use-module (gnu services ssh)

  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)

  #:use-module (nongnu services nvidia)

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

;; (define nvidia-module-open-xanmod
;;   (let ((base nvidia-module-open))
;;     (package
;;      (inherit base)
;;      (arguments
;;       (substitute-keyword-arguments (package-arguments base)
;;                                     ((#:linux linux-lts) linux-xanmod-lts)
;;                                     ;; ((#:make-flags flags)
;;                                     ;;  #~(append #$flags (list "CC=clang" "LLVM=1")))
;;                                     )))))

(operating-system
 (kernel linux-xanmod-lts)
 (kernel-arguments (append '("video=DP-1:3440x1440@144"
                             "modprobe.blacklist=nouveau,pcspkr"
                             "nvidia_drm.modeset=1"
                             "quiet")
                           %default-kernel-arguments))
 (kernel-loadable-modules (list nvidia-module))
 (initrd microcode-initrd)
 (firmware (list linux-firmware amd-microcode nvidia-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Stockholm")
 (keyboard-layout (keyboard-layout "se" #:options '("ctrl:nocaps")))
 (host-name "okarthel")
 (sudoers-file etc-sudoers-config)
 (users (cons* (user-account
                (name "tobias")
                (comment "Tobias")
                (group "users")
                (home-directory "/home/tobias")
                (supplementary-groups
                 '("adbusers" "wheel" "netdev" "audio" "video" "dialout" "kvm"))) ; "libvirt"
               %base-user-accounts))
 (packages
  (append
   (list
    nss-certs
    git
    openssh
    libglvnd
    vulkan-loader

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
    (service pcscd-service-type)
    (udev-rules-service 'android android-udev-rules
                        #:groups '("adbusers"))
    ;; (service cups-service-type
    ;;          (cups-configuration
    ;;           (web-interface? #t)))
    (service nvidia-service-type))
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
                                          (modules (cons* nvidia-driver %default-xorg-modules))
                                          (server (replace-mesa xorg-server))
                                          (drivers '("nvidia"))
                                          (extra-config (list xorg:%libinput-config
                                                              xorg:%nvidia-config)))))))))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout keyboard-layout)
   ;; (theme (grub-theme
   ;;         (gfxmode '("1920x1080" "auto"))))
   ))

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
