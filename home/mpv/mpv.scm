(define-module (home mpv mpv)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix git-download)

  #:use-module ((gnu packages video) #:select (ffmpeg mpv))
  #:use-module ((gnu packages xorg) #:select (libxfixes))
  #:use-module ((gnu packages vulkan) #:select (vulkan-loader))
  #:use-module ((gnu packages video) #:select (libvdpau libvdpau-va-gl))
  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (nongnu packages nvidia))

(define transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

(define ffmpeg-fruc
  (package
   (inherit ffmpeg)
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/philipl/FFmpeg")
           (commit "d84becd3da465cdf3d8a6400bdba71646800898b")))
     ;; (file-name (git-file-name name version))
     (sha256 (base32 "1a9fsk2hqy1a5g7dbl3yg60yh6r441mizzpf68jmcccynk1a04xg"))))))

(define-public eglexternalplatform
  (let ((base (@ (gnu packages xorg) eglexternalplatform)))
    (package
     (inherit base)
     (native-search-paths
      (list (search-path-specification
             (variable "__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS")
             (files '("share/egl/egl_external_platform.d"))))))))

(define-public libglvnd
  (let ((base (@ (gnu packages gl) libglvnd)))
    (package
     (inherit base)
     (native-search-paths
      (list (search-path-specification
             (variable "__EGL_VENDOR_LIBRARY_DIRS")
             (files '("share/glvnd/egl_vendor.d"))))))))

;; (define-public mpv-nvidia
;;   (let ((base (@ (gnu packages video) mpv)))
;;     (package/inherit base
;;      (name "mpv-nvidia")
;;      (native-search-paths
;;       (list (search-path-specification
;;              (variable "VK_ICD_PATH")
;;              (files '("share/vulkan/icd.d")))
;;             (search-path-specification
;;              (variable "VK_ILAYER_PATH")
;;              (files '("share/vulkan/implicit_layer.d"))))))))

(define mpv-nvidia
  (package
   (inherit mpv)
   (inputs
    (modify-inputs (package-inputs mpv)
                   (prepend vulkan-loader eglexternalplatform libglvnd libvdpau libvdpau-va-gl libxfixes)
                   (replace "ffmpeg" ffmpeg-fruc)
                   (replace "mesa" nvda)))
   (native-search-paths
    (list (search-path-specification
           (variable "VK_ICD_PATH")
           (files '("share/vulkan/icd.d")))
          (search-path-specification
           (variable "VK_ILAYER_PATH")
           (files '("share/vulkan/implicit_layer.d")))))))

(define-public packages
  (list (if (string= (gethostname) "okarthel")
            (replace-mesa mpv)
            mpv)))

(define-public services
  (list
   (simple-service 'mpv-config
                   home-xdg-configuration-files-service-type
                   `(("mpv/mpv.conf"   ,(local-file "files/mpv.conf"))
                     ;; ("mpv/input.conf"   ,(local-file "files/input.conf"))
                     ("mpv/FSRCNNX_x2_8-0-4-1.glsl"   ,(local-file "files/FSRCNNX_x2_8-0-4-1.glsl"))))))
