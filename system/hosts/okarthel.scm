(define-module (system hosts okarthel)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
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

  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages shells) #:select (fish))
  #:use-module ((gnu packages selinux) #:select (libselinux))
  #:use-module ((gnu packages bootloaders) #:select (grub))
  #:use-module ((gnu packages version-control) #:select (git))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages avahi) #:select (nss-mdns))
  #:use-module ((gnu packages package-management) #:select (flatpak))
  #:use-module ((gnu packages base) #:select (patch glibc-utf8-locales))
  #:use-module ((gnu packages compression) #:select (zstd lz4))
  #:use-module ((gnu packages gnome) #:select (libsecret))
  #:use-module ((gnu packages glib) #:select (dbus-glib))
  #:use-module ((gnu packages file-systems) #:select (bees))
  #:use-module ((gnu packages linux) #:select (libinih v4l2loopback-linux-module customize-linux linux-libre-6.15))

  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu services base)
  #:use-module (gnu services security-token)
  #:use-module (gnu services linux)
  #:use-module (gnu services desktop)
  #:use-module (gnu services avahi)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)

  #:use-module (nongnu packages linux)
  #:use-module ((nongnu packages nvidia) #:select (replace-mesa nvidia-module))

  #:use-module (nongnu services nvidia)

  #:use-module ((nongnu system linux-initrd) #:select (microcode-initrd))

  #:use-module ((trowel) #:select (aggressively-optimize))
  #:use-module ((system common) #:prefix common:)
  #:use-module ((system impermanence) #:prefix impermanence:)
  #:use-module ((system udev) #:prefix udev:)
  #:use-module ((system network) #:prefix network:)
  #:use-module ((system syncthing) #:prefix syncthing:)
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


(define etc-sudoers-config
  (plain-file "etc-sudoers-config"
              "Defaults  timestamp_timeout=480
root      ALL=(ALL) ALL
%wheel    ALL=(ALL) NOPASSWD:ALL
tobias    ALL=(ALL) NOPASSWD:/run/current-system/profile/bin/loginctl,/run/current-system/profile/bin/nmtui"))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define kernel-version "6.15.6")
(define kernel-source (%upstream-linux-source kernel-version (base32 "1z5l0b59q56qj6s56cxzv43lhfx9z9sp4vfziw60fz97ak4qdd9b")))

(operating-system
  (kernel
   (package-with-c-toolchain
    (let ((p (customize-linux
              #:name "linux-zen"
              #:extra-version "zen-okarthel"
              #:linux (package
                        (inherit linux-libre-6.15)
                        (native-inputs
                         (modify-inputs (package-native-inputs linux-libre-6.15)
                           (prepend lz4))))
              #:defconfig (local-file "defconfigs/okarthel")
              #:source (origin
                         (inherit kernel-source)
                         (modules '((guix build utils)))
                         (snippet
                          #~(begin
                              (let* ((zen-patch-origin #+(origin
                                                           (method url-fetch)
                                                           (uri (string-append
                                                                 "https://github.com/zen-kernel/zen-kernel/releases/download/v"
                                                                 kernel-version "-zen1/linux-v" kernel-version "-zen1.patch.zst"))
                                                           (sha256 "02vb9g7z5gqk692a0jmwjdx50n96j3wnyq4xb24qnmrpcrmbznf4"))))
                                (copy-file zen-patch-origin (basename zen-patch-origin))
                                (invoke #+(file-append zstd "/bin/zstd") "-d" (basename zen-patch-origin))
                                (invoke #+(file-append patch "/bin/patch")
                                        "--force" "--no-backup-if-mismatch"
                                        #+@(origin-patch-flags kernel-source)
                                        "--input" (basename zen-patch-origin ".zst"))
                                (delete-file (basename zen-patch-origin ".zst")))))))))
      (package
        (inherit p)
        (arguments
         (substitute-keyword-arguments (package-arguments p)
           ((#:make-flags flags #~'())
            #~(append
               #$flags
               (let ((f "-pipe -O3 -march=native -fgraphite-identity -floop-nest-optimize -fno-semantic-interposition"))
                 (list
                  (string-append "KCFLAGS=" f)
                  (string-append "KCPPFLAGS=" f)))))))))
    `(("gcc-toolchain" ,gcc-toolchain))))
  (kernel-arguments (append '("mitigations=off" ; Live a little
                              "modprobe.blacklist=nouveau,pcspkr"
                              "nvidia_drm.modeset=1"
                              "quiet")
                            %default-kernel-arguments))
  (kernel-loadable-modules (list v4l2loopback-linux-module))
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
                 (shell (file-append fish "/bin/fish"))
                 (home-directory "/home/tobias")
		 (password (call-with-input-file "/home/tobias/irthir/irthos/okarthel" get-line))
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
          nss-mdns
          flatpak

          glibc-utf8-locales
          dbus-glib
          libsecret
          libselinux))
    %base-packages))

  (services
   (append
    common:services
    impermanence:services
    udev:services
    network:services
    syncthing:services
    wayland:services
    (list
     (simple-service 'v4l2loopback-module kernel-module-loader-service-type '("v4l2loopback")) ;; FIXME: Dumb but fixes error: service 'nvidia' requires 'kernel-module-loader', which is not provided by any service
     (service earlyoom-service-type
              (earlyoom-configuration
               (minimum-available-memory 1)
               (minimum-free-swap 100)))
     (service pcscd-service-type)
     (service nvidia-service-type
              ;; (nvidia-configuration
              ;;  (module (package
              ;;            (inherit nvidia-module)
              ;;            (arguments
              ;;             (substitute-keyword-arguments (package-arguments nvidia-module)
              ;;               ((#:make-flags flags #~'())
              ;;                #~(append #$flags (list "IGNORE_PREEMPT_RT_PRESENCE=1"))))))))
              )
     ;; (udev-rules-service 'leetmouse leetmouse-udev-rules)

     fontconfig-file-system-service
     (service udisks-service-type)
     (service avahi-service-type
              (avahi-configuration
               (wide-area? #t)))
     (service cups-service-type
              (cups-configuration
               (web-interface? #t)))
     (service polkit-service-type)
     (service dbus-root-service-type)
     (service pam-limits-service-type
              (list (pam-limits-entry "*" 'both 'memlock 'unlimited)))


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
    impermanence:file-systems
    (list
     (file-system
       (mount-point "/boot/efi")
       (device (uuid "C1E3-56B8" 'fat32))
       (type "vfat"))

     (file-system
       (mount-point "/bulk/")
       (device (uuid "2a2dd272-52f8-42b3-ba30-46aee21d75f4" 'ext4))
       (flags '(no-atime))
       (type "ext4")))

    %base-file-systems)))
