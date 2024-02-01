(define-module (system udev)
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix packages) #:select (origin base32))
  #:use-module ((gnu packages android) #:select (android-udev-rules))
  #:use-module ((gnu packages security-token) #:select (libfido2))

  #:use-module (gnu services)
  #:use-module (gnu services base))

(define %steam-input-udev-rules
  (file->udev-rule
    "60-steam-input.rules"
    (let ((commit "13443480a64fe8f10676606bd57da6de89f8ccb1"))
      (origin (method url-fetch)
       (uri (string-append "https://raw.githubusercontent.com/ValveSoftware/steam-devices/"
                           commit "/60-steam-input.rules"))
       (sha256 (base32 "0k92pjn2yx09wqya4mgy3xrqg2g77zpsgzgayfg77r6ljl88b81j"))))))

(define %steam-vr-udev-rules
  (file->udev-rule
    "60-steam-vr.rules"
    (let ((commit "13443480a64fe8f10676606bd57da6de89f8ccb1"))
      (origin (method url-fetch)
       (uri (string-append "https://raw.githubusercontent.com/ValveSoftware/steam-devices/"
                           commit "/60-steam-vr.rules"))
       (sha256 (base32 "0a4s3f7rcx9kyrbh45dkyj0x8zfqg5nl0d8n9w9pm0g8f1ashcrj"))))))

(define-public services
  (list
   (udev-rules-service 'steam-input %steam-input-udev-rules)
   (udev-rules-service 'steam-vr %steam-vr-udev-rules)
   (udev-rules-service 'android android-udev-rules #:groups '("adbusers"))
   (udev-rules-service 'fido2 libfido2 #:groups '("plugdev"))))
