(define-module (home main)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module ((gnu packages compression) #:select (atool
                                                     unrar-free
                                                     zip
                                                     unzip
                                                     p7zip))
  #:use-module ((gnu packages pciutils) #:select (pciutils))
  #:use-module ((gnu packages gl) #:select (mesa-utils))
  #:use-module ((gnu packages vpn) #:select (wireguard-tools))
  #:use-module ((gnu packages rust-apps) #:select (tealdeer))
  #:use-module ((gnu packages admin) #:select (btop))
  #:use-module ((gnu packages gnupg) #:select (pinentry))
  #:use-module ((gnu packages password-utils) #:select (password-store keepassxc))
  #:use-module ((gnu packages pdf) #:select (zathura zathura-pdf-mupdf))
  #:use-module ((gnu packages wine) #:select (wine64-staging))
  #:use-module ((gnu packages librewolf) #:select (librewolf))
  #:use-module ((gnu packages llvm) #:select (make-lld-wrapper lld-18))

  #:use-module ((gnu packages fonts) #:select (font-google-noto
                                               font-google-noto-sans-cjk
                                               font-google-noto-emoji))

  #:use-module ((nongnu packages mozilla) #:select (firefox))
  #:use-module ((nongnu packages wine) #:select (winetricks))
  #:use-module ((nongnu packages nvidia) #:select (nvdb nvidia-driver-beta))

  #:use-module (gnu services)

  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type))
  #:use-module ((gnu home services sound) #:select (home-pipewire-service-type))
  #:use-module (rde home services desktop)

  #:use-module ((trowel) #:select (replace-mesa aggressively-optimize))
  #:use-module ((home shell) #:prefix shell:)
  #:use-module ((home gtk) #:prefix gtk:)
  #:use-module ((home xorg xresources) #:prefix xresources:)
  #:use-module ((home xorg i3) #:prefix i3:)
  #:use-module ((home xorg dunst) #:prefix dunst:)
  #:use-module ((home wayland sway) #:prefix sway:)
  #:use-module ((home emacs emacs) #:prefix emacs:)
  #:use-module ((home qutebrowser qutebrowser) #:prefix qutebrowser:)
  #:use-module ((home mpv mpv) #:prefix mpv:)
  #:use-module ((home creative) #:prefix creative:)
  #:use-module ((home ai) #:prefix ai:)
  #:use-module ((home games) #:prefix games:)
  #:use-module ((home vile) #:prefix vile:))

(define btop-nvidia
  (package
    (inherit btop)
    (inputs
     (modify-inputs (package-inputs btop)
       (prepend nvidia-driver-beta)))
    (arguments
     (substitute-keyword-arguments (package-arguments btop)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'fix-libnvidia
              (lambda _
                (substitute* "src/linux/btop_collect.cpp"
                  (("libnvidia-ml.so.1")
                   (string-append #$(this-package-input "nvidia-driver-beta")
                                  "/lib/libnvidia-ml.so.1")))))))
       ((#:make-flags flags #~'())
        #~(append #$flags (list "GPU_SUPPORT=true")))))))

(define legacyfox
  (package
    (name "legacyfox")
    (version "0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.gir.st/LegacyFox.git")
             (commit "312a791ae03bddd725dee063344801f959cfe44d")))
       (sha256 (base32 "05ppc2053lacvrlab4fspxmmjmkryvvc6ndrzhyk06ivmm2nlyyx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'install 'mkdir
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p (string-append out))
                #t)))
          (add-before 'install 'rename-config-librewolf-cfg
            (lambda _
              (substitute* "defaults/pref/config-prefs.js"
                (("(config\\.js)")
                 "librewolf.cfg")))))
      #:make-flags
      #~(list (string-append "DESTDIR=" #$output))))
    (home-page "https://git.gir.st/LegacyFox.git")
    (synopsis "Legacy bootstrapped extensions for Firefox 65 and beyond")
    (description "Legacy bootstrapped extensions for Firefox 65 and beyond.")
    (license license:mpl2.0)))

(define-public main-home
  (home-environment
   (packages (append
              shell:packages
              gtk:packages
              sway:packages
              emacs:packages
              qutebrowser:packages
              mpv:packages
              creative:packages
              ai:packages
              games:packages
              vile:packages
              (let ((lst (list
                          atool
                          unrar-free
                          zip
                          unzip
                          p7zip

                          pinentry
                          password-store
                          keepassxc
                          wireguard-tools
                          tealdeer
                          zathura
                          zathura-pdf-mupdf
                          wine64-staging
                          winetricks
                          (package
                            (inherit mesa-utils)
                            (inputs
                             (modify-inputs (package-inputs mesa-utils)
                               (prepend pciutils))))
                          firefox

                          ;; Librewolf with Legacyfox, optimizations from Mercury, and Mozilla addons repo
                          (package
                            (inherit librewolf)
                            (arguments
                             (substitute-keyword-arguments (package-arguments librewolf)
                               ((#:configure-flags flags #~'())
                                #~(append (list "--enable-clang-plugin"
                                                "--disable-debug-symbols"
                                                "--disable-debug-js-modules"
                                                "--enable-optimize=\"-O3 -march=native\""
                                                "--enable-eme=widevine")
                                          (fold delete #$flags '("--enable-optimize"
                                                                 "--disable-eme"))))
                               ((#:phases phases)
                                #~(modify-phases #$phases
                                    (delete 'fix-addons-placeholder)
                                    (delete 'patch-config)
                                    (add-after 'install 'add-legacyfox
                                      (lambda* (#:key inputs outputs #:allow-other-keys)
                                        (let* ((out (assoc-ref outputs "out"))
                                               (librewolf-dir (string-append out "/lib/librewolf/"))
                                               (legacyfox (assoc-ref inputs "legacyfox")))
                                          (mkdir-p (string-append librewolf-dir "defaults/pref"))

                                          (copy-file (string-append legacyfox "/defaults/pref/config-prefs.js")
                                                     (string-append librewolf-dir "defaults/pref/config-prefs.js"))

                                          (copy-recursively (string-append legacyfox "/legacy")
                                                            (string-append librewolf-dir "legacy"))

                                          (copy-file (string-append legacyfox "/legacy.manifest")
                                                     (string-append librewolf-dir "legacy.manifest"))

                                          (let ((port (open-file (string-append librewolf-dir "librewolf.cfg") "a")))
                                            (display (call-with-input-file
                                                         (string-append legacyfox "/config.js")
                                                       get-string-all)
                                                     port)
                                            (close-port port)))))
                                    (add-before 'configure 'add-envvars
                                      (lambda* (#:key inputs outputs #:allow-other-keys)
                                        (let* ((flags "-O3 -ffp-contract=fast -march=native"))
                                          (setenv "MOZ_OPTIMIZE" "1")
                                          (setenv "OPT_LEVEL" "3")
                                          (setenv "RUSTC_OPT_LEVEL" "3")
                                          (setenv "CFLAGS" flags)
                                          (setenv "CPPFLAGS" flags)
                                          (setenv "CXXFLAGS" flags)
                                          (setenv "LDFLAGS" "-Wl,-O3")
                                          (setenv "RUSTFLAGS" "-C target-cpu=native -C codegen-units=1"))))))))
                            (native-inputs
                             (modify-inputs (package-native-inputs librewolf)
                               (prepend
                                (make-lld-wrapper lld-18 #:lld-as-ld? #t) ; Same version as clang in inputs
                                legacyfox))))
                          (if (or (string= (gethostname) "okarthel")
                                  (string= (gethostname) "austrat"))
                              (aggressively-optimize btop-nvidia)
                              btop)

                          ;; Fonts
                          font-google-noto
                          font-google-noto-sans-cjk
                          font-google-noto-emoji)))
                (if (string= (gethostname) "okarthel")
                    (map replace-mesa lst)
                    lst))))

   (services
    (append
     shell:services
     gtk:services
     sway:services
     dunst:services
     emacs:services
     qutebrowser:services
     mpv:services
     vile:services
     (list
      (service home-dbus-service-type)
      (service home-pipewire-service-type)
      (simple-service 'git-config home-files-service-type
                      `((".gitconfig"  ,(plain-file "gitconfig" "
[user]
	email = tobias@nights.rest
	name = tobias
[filter.lfs]
	required = true
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	clean = git-lfs clean -- %f
"))))
      (simple-service 'channels home-channels-service-type
                      (list
                       (channel
                        (name 'nonguix)
                        (url "https://gitlab.com/nonguix/nonguix")
                        (introduction
                         (make-channel-introduction
                          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
                          (openpgp-fingerprint
                           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
                       (channel
                        (name 'guix-science-nonfree)
                        (url "https://codeberg.org/guix-science/guix-science-nonfree.git")
                        (introduction
                         (make-channel-introduction
                          "58661b110325fd5d9b40e6f0177cc486a615817e"
                          (openpgp-fingerprint
                           "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
                       (channel
                        (name 'saayix)
                        (url "https://codeberg.org/look/saayix.git")
                        (branch "entropy")
                        (introduction
                         (make-channel-introduction
                          "12540f593092e9a177eb8a974a57bb4892327752"
                          (openpgp-fingerprint
                           "3FFA 7335 973E 0A49 47FC  0A8C 38D5 96BE 07D3 34AB"))))
                       (channel
                        (name 'rde)
                        (url "https://git.sr.ht/~abcdw/rde")
                        (introduction
                         (make-channel-introduction
                          "257cebd587b66e4d865b3537a9a88cccd7107c95"
                          (openpgp-fingerprint
                           "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))))
      (simple-service 'main-env-vars
                      home-environment-variables-service-type
                      `(("PATH" . "$HOME/.cabal/bin/:$PATH:$HOME/.local/bin/:$HOME/.local/lib/cargo/bin/:$HOME/.local/lib/npm/bin/")
                        ("HISTCONTROL" . "ignoredups:ignorespace")
                        ("HISTSIZE" . "10000")
                        ("HISTFILE" . "$HOME/.local/state/shell/history")
                        ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                        ("TERM" . "xterm-256color"))))
     (if (string= (gethostname) "okarthel")
         (list
          (simple-service 'okarthel-env-vars
                          home-environment-variables-service-type
                          '(("XDG_SESSION_TYPE" . "xcb"))))
         '())
     (if (string= (gethostname) "austrat")
         (list
          (simple-service
           'austrat-env-vars
           home-environment-variables-service-type
           `(("QT_QPA_PLATFORM" . "wayland-egl")
             ("XDG_SESSION_TYPE" . "wayland")
             ("XDG_CURRENT_DESKTOP" . "sway")
             ("MOZ_ENABLE_WAYLAND" . "1")
             ("__EGL_VENDOR_LIBRARY_FILENAMES" . ,(file-append nvdb "/share/glvnd/egl_vendor.d/50_mesa.x86_64.json"))
             ("__GLX_VENDOR_LIBRARY_NAME" . "mesa"))))
         '())))))

main-home
