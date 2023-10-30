(define-module (home nyxt nyxt)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)

  #:use-module ((gnu packages web-browsers) #:select (nyxt))
  #:use-module (gnu packages crates-io)
  #:use-module ((gnu packages gstreamer) #:select (gstreamer
                                                   gst-libav
                                                   gst-plugins-base
                                                   gst-plugins-good
                                                   gst-plugins-bad
                                                   gst-plugins-ugly))

  #:use-module (gnu services)
  #:use-module (gnu home services))

(define transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

(define-public rust-attohttpc
  (package
   (name "rust-attohttpc")
   (version "0.26.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "attohttpc" version))
     (file-name
      (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "1p4hj2psdcxm9v19frwpaa1aqs1f2vax53bjppxpj28vj91x4xqg"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-base64" ,rust-base64-0.21)
       ("rust-encoding-rs" ,rust-encoding-rs-0.8)
       ("rust-encoding-rs-io" ,rust-encoding-rs-io-0.1)
       ("rust-flate2" ,rust-flate2-1)
       ("rust-http" ,rust-http-0.2)
       ("rust-log" ,rust-log-0.4)
       ("rust-mime" ,rust-mime-0.3)
       ("rust-multipart" ,rust-multipart-0.18)
       ("rust-native-tls" ,rust-native-tls-0.2)
       ("rust-rustls" ,rust-rustls-0.21)
       ("rust-rustls-native-certs" ,rust-rustls-native-certs-0.6)
       ("rust-serde" ,rust-serde-1)
       ("rust-serde-json" ,rust-serde-json-1)
       ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
       ("rust-url" ,rust-url-2)
       ("rust-webpki-roots" ,rust-webpki-roots-0.25))
      #:cargo-development-inputs
      (("rust-anyhow" ,rust-anyhow-1)
       ("rust-env-logger" ,rust-env-logger-0.10)
       ("rust-futures" ,rust-futures-0.3)
       ("rust-futures-util" ,rust-futures-util-0.3)
       ("rust-hyper" ,rust-hyper-0.14)
       ("rust-lazy-static" ,rust-lazy-static-1)
       ("rust-rustls-pemfile" ,rust-rustls-pemfile-1)
       ("rust-tokio" ,rust-tokio-1)
       ("rust-tokio-rustls" ,rust-tokio-rustls-0.23)
       ("rust-tokio-stream" ,rust-tokio-stream-0.1)
       ("rust-warp" ,rust-warp-0.2))))
   (home-page "https://github.com/sbstp/attohttpc")
   (synopsis "Small and lightweight HTTP client")
   (description "Small and lightweight HTTP client")
   (license mpl2.0)))

(define-public adblock-rust-server
  (package
   (name "adblock-rust-server")
   (version "0.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (crate-uri "adblock-rust-server" version))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256 (base32 "04s0c4xg4gj416fybrmxyq1bczkf3r51xfy3cqnw613zywmigj9q"))))
   (build-system cargo-build-system)
   (arguments
    `(#:cargo-inputs
      (("rust-adblock" ,rust-adblock-0.7)
       ("rust-attohttpc" ,rust-attohttpc))))
   ;; (inputs (list adblock-rust-server))
   (synopsis "Hello, GNU world: An example GNU package")
   (description "Guess what GNU Hello prints!")
   (home-page "https://www.gnu.org/software/hello/")
   (license gpl3+)))

(define-public blockit
  (package
   (name "blockit")
   (version "2.10")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/dudik/blockit")
           (commit "be185b454798afd5542f08cf51774b72ee7b01d4")))
     (sha256 (base32 "1a9fsk2hqy1a5g7dbl3yg60yh6r441mizzpf68jmcccynk1a04xg"))))
   (build-system gnu-build-system)
   (inputs (list adblock-rust-server))
   (synopsis "Hello, GNU world: An example GNU package")
   (description "Guess what GNU Hello prints!")
   (home-page "https://www.gnu.org/software/hello/")
   (license gpl3+)))

(define-public packages
  (list
   (transform nyxt)
   ;; blockit
   ;; adblock-rust-server
   gstreamer
   gst-libav
   gst-plugins-base
   gst-plugins-good
   gst-plugins-bad
   gst-plugins-ugly))

(define-public services
  (list
   (simple-service 'nyxt-config
                   home-files-service-type
                   `((".config/nyxt/config.lisp" ,(local-file "config/config.lisp"))))))
