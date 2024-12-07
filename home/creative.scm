(define-module (home creative)
  #:use-module (guix gexp)

  ;; For Sunvox package definition
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module ((nonguix licenses) #:prefix license:)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (ice-9 match)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((gnu packages video) #:select (obs obs-pipewire-audio-capture))
  #:use-module ((gnu packages music) #:select (zrythm zplugins))
  #:use-module ((gnu packages graphics) #:select (blender))
  #:use-module ((gnu packages python-xyz) #:select (python-numpy)) ; Blender python libraries
  #:use-module ((gnu packages gimp) #:select (gimp-next))
  #:use-module ((gnu packages inkscape) #:select (inkscape))

  #:use-module ((trowel) #:select (replace-mesa)))

;; FIXME: From https://gitlab.com/nonguix/nonguix/-/merge_requests/323, remove when merged
(define-public sunvox
  (package
   (name "sunvox")
   (version "2.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://warmplace.ru/soft/sunvox/sunvox-" version "c.zip"))
     (sha256
      (base32 "0g6avkc2azp44d0g4m1m23r38gnmg7v9a1fbp2yv6ma0v5p5rxf8"))))
   (build-system binary-build-system)
   (arguments
    `(#:patchelf-plan '((,(string-append "sunvox/"
                                         (match (%current-system)
                                           ("x86_64-linux" "linux_x86_64")
                                           ("i686-linux" "linux_x86")
                                           ("aarch64-linux" "linux_arm64")
                                           ("armhf-linux" "linux_arm"))
                                         "/sunvox")
                         ("libc" "gcc" "alsa-lib" "sdl2")))
      #:install-plan '(("curves" "share/curves")
                       ("docs" "share/docs")
                       ("effects" "share/effects")
                       ("examples" "share/examples")
                       ("instruments" "share/instruments")
                       (,(string-append "sunvox/"
                                        (match (%current-system)
                                          ("x86_64-linux" "linux_x86_64")
                                          ("i686-linux" "linux_x86")
                                          ("aarch64-linux" "linux_arm64")
                                          ("armhf-linux" "linux_arm"))
                                        "/sunvox")
                        "share/sunvox/linux/sunvox"))
      #:validate-runpath? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'install 'make-wrapper
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (real (string-append out "/share/sunvox/linux/sunvox"))
                                         (wrapper (string-append out "/bin/sunvox")))
                                    (mkdir-p (dirname wrapper))
                                    (symlink real wrapper))
                                  #t)))))
   (inputs
    (list
     alsa-lib
     sdl2
     unzip
     `(,gcc "lib")))
   (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"
                        "armhf-linux"))
   (home-page "https://warmplace.ru/soft/sunvox/")
   (synopsis "Modular synthesizer and tracker.")
   (description
    "SunVox is a small, fast and powerful modular synthesizer with
pattern-based sequencer (tracker). Features:
@itemize
@item Support for variety of systems and audio formats.
@item Real-time sample/MIDI/audio signal playback and recording.
@item Generative music utilities.
@item Lots of built-in modules.
@end itemize")
   (license (license:nonfree "file:///share/docs/license/sunvox.txt"))))

(define-public packages
  (let ((lst (list
              obs
              obs-pipewire-audio-capture
              sunvox
              zrythm
              zplugins
              blender
              python-numpy
              gimp-next
              inkscape)))
    (if (or (string= (gethostname) "okarthel")
            (string= (gethostname) "austrat"))
        (map replace-mesa lst)
        lst)))
