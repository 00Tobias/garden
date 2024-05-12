(define-module (home mpv mpv)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)

  #:use-module ((gnu packages video) #:select (ffmpeg mpv yt-dlp))
  #:use-module ((gnu packages curl) #:select (curl))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa)))

(define ffmpeg-git
  (package
    (inherit ffmpeg)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.ffmpeg.org/ffmpeg.git")
             (commit "575efc0406864542a65aa3f31ebe8eb0bbef5087")))
       (sha256 (base32 "05cfn3z0267v7c0d2yvcmjbcf42hha9s17fr8fr1in2h4q0f4lhk"))))))

(define mpv-git
  (package
    (inherit mpv)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mpv-player/mpv")
             (commit "17be6e1990a92e9b43f5f49cca9c4dd3da24a1e8")))
       (sha256 (base32 "1pr698qw5l6awfrc61j5hxcw2l56sxfq6ymd36ccpxhpnlzr3sgd"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs mpv)
       (replace "ffmpeg" ffmpeg-git)))))

(define-public packages
  (let ((lst (list
              mpv-git
              yt-dlp
              curl                      ; sponsorblock_minimal.lua
              )))
    (if (or (string= (gethostname) "okarthel")
            (string= (gethostname) "austrat"))
        (map replace-mesa lst)
        lst)))

(define-public services
  (list
   (simple-service 'mpv-env-vars-service
                   home-environment-variables-service-type
                   `(("WHISPER_CMD" . "1")))
   (simple-service 'mpv-config
                   home-xdg-configuration-files-service-type
                   `(("mpv/mpv.conf"                ,(local-file "files/mpv.conf"))
                     ("mpv/input.conf"              ,(local-file "files/input.conf"))
                     ("mpv/FSRCNNX_x2_8-0-4-1.glsl" ,(local-file "files/FSRCNNX_x2_8-0-4-1.glsl"))
                     ("mpv/scripts"                 ,(local-file "files/scripts" #:recursive? #t))))))
