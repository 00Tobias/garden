(define-module (home theme)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

(define make-nerd-font-package
  (@@ (saayix packages fonts) make-nerd-font-package))

(define font-nerd-iosevka
  (make-nerd-font-package
   "Iosevka"
   "font-nerd-iosevka"
   "10w24pir4flr0zhm0n6v6kblgmcx7dpnqv2xkp8d0rgh3rnlwpm5"))

(define-public font-nerd-atkynson-next
  (package
    (name "font-nerd-atkynson-next")
    (version "1.0.0")
    (source (local-file "fonts/atkinson-hyperlegible-next" #:recursive? #t))
    (build-system font-build-system)
    (home-page "https://github.com/googlefonts/atkinson-hyperlegible-next")
    (synopsis "New (2024) second version of the Atkinson Hyperlegible fonts")
    (description
     "Atkinson Hyperlegible Next is an improvement of the typeface Atkinson Hyperlegible, including
 new characters, improved glyphs, and improved kerning. Additionally, the two previous weights has
increased to six, all in upright and italic, allowing for greater flexibility in use.")
    (license license:silofl1.1)))

(define-public font-package font-nerd-atkynson-next)
(define-public font-package-mono font-nerd-iosevka)
(define-public font "AtkynsonNext Nerd Font Propo")
(define-public font-mono "Iosevka Nerd Font")
(define-public font-size "11")
(define-public fg "#ffffff")
(define-public bg "#000000")
(define-public accent "#1c1c1c")
(define-public highlight "#ff0000")
