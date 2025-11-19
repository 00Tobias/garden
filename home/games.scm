(define-module (home games)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)

  #:use-module ((gnu packages xorg) #:select (libxcb))
  #:use-module ((gnu packages qt) #:select (qtwayland))
  #:use-module ((gnu packages libusb) #:select (libusb))

  #:use-module ((gnu packages games) #:select (nethack))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module ((saayix packages minecraft) #:select (prismlauncher)))

(define prismlauncher-fixed
  (package
    (inherit prismlauncher)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out            (assoc-ref outputs "out"))
                    (bin            (string-append out "/bin/prismlauncher"))
                    (xrandr         (assoc-ref inputs "xrandr"))
                    (qtwayland      (assoc-ref inputs "qtwayland")))
               (wrap-program bin
                 `("PATH" ":" prefix (,(string-append xrandr "/bin")))
                 `("QT_PLUGIN_PATH" ":" prefix (,(string-append qtwayland "/lib/qt5/plugins")))
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,@(map (lambda (dep)
                             (string-append (assoc-ref inputs dep) "/lib"))
                           '("libx11" "libxext" "libxcursor"
                             "libxrandr" "libxxf86vm" "pulseaudio"
                             "wayland" "libxkbcommon"
                             "mesa" "libxcb")))))
               #t))))))
    (inputs
     (modify-inputs (package-inputs prismlauncher)
       (prepend libusb libxcb)))))

(define-public packages
  (let ((lst (list
              nethack
              prismlauncher-fixed)))
    (if (string= (gethostname) "okarthel")
        (map replace-mesa lst)
        lst)))
