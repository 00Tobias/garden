(define-module (home creative)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((gnu packages video) #:select (obs obs-pipewire-audio-capture))
  #:use-module ((gnu packages music) #:select (zrythm zplugins))
  #:use-module ((gnu packages graphics) #:select (blender))
  #:use-module ((gnu packages python-xyz) #:select (python-numpy)) ; Blender python libraries
  #:use-module ((gnu packages game-development) #:select (godot))
  #:use-module ((gnu packages gimp) #:select (gimp-next))
  #:use-module ((gnu packages inkscape) #:select (inkscape))
  #:use-module ((gnu packages kde) #:select (krita))
  #:use-module ((gnu packages libreoffice) #:select (libreoffice))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module ((incognita packages music) #:select (sunvox))

  #:use-module ((trowel) #:select (aggressively-optimize)))

(define-public packages
  (let ((lst (list
              obs
              obs-pipewire-audio-capture
              sunvox
              zrythm
              zplugins
              blender
              python-numpy
              godot
              gimp-next
              inkscape
              krita
              libreoffice)))
    (if (or (string= (gethostname) "okarthel")
            (string= (gethostname) "austrat"))
        (map replace-mesa lst)
        lst)))
