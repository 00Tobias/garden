(define-module (home librewolf)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module ((gnu packages librewolf) #:select (librewolf))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)

  #:use-module (gnu home services)

  #:use-module ((trowel) #:select (wrap-package)))

(define legacyfox-for-librewolf
  (package
    (name "legacyfox-for-librewolf")
    (version "5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.gir.st/LegacyFox.git")
              (commit "7c2fc6e40e8e84d9cecde02e9630318ccca605ab")))
       (sha256 (base32 "1bshld1nxavcg5jkc78fxfk2j6hy02xpgzrj47nkmlh5izswr9ai"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'install 'chdir
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (librewolf-dir (string-append out "/lib/librewolf/")))
                (mkdir-p librewolf-dir)
                #t)))
          (add-before 'install 'rename-config-librewolf-cfg
            (lambda _
              (substitute* "defaults/pref/config-prefs.js"
                (("(config\\.js)")
                 "librewolf.cfg")))))
      #:make-flags
      #~(list (string-append "DESTDIR=" (string-append #$output "/lib/librewolf")))))
    (home-page "https://git.gir.st/LegacyFox.git")
    (synopsis "Legacy bootstrapped extensions for Firefox 65 and beyond")
    (description "Legacy bootstrapped extensions for Firefox 65 and beyond.")
    (license license:mpl2.0)))

(define librewolf+legacyfox-config
  (package
    (name "librewolf+legacyfox-config")
    (version "0")
    (source
     (mixed-text-file
      "librewolf.cfg"
      #~(call-with-input-file
            #$(origin
                (uri "https://codeberg.org/librewolf/settings/raw/commit/f14d8586601bd3f13a32986d4a6a305a27a0806c/librewolf.cfg")
                (method url-fetch)
                (sha256 (base32 "057dlx91h5z59bzg4wjqvbvdmbqwlnyg3c64lwnzfxf9204pdpjb")))
          (@ (ice-9 textual-ports) get-string-all))
      #~(call-with-input-file
            #$(origin
                (uri "https://git.gir.st/LegacyFox.git/blob_plain/f732e438a6d8e75ce22c28c43878ca5e3effcadd:/config.js")
                (method url-fetch)
                (sha256 (base32 "1g9jbhpjgklyjcwbc0ixyq00b1zbxi87895rpwh2kv0cyhz6l6jg")))
          (@ (ice-9 textual-ports) get-string-all))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("librewolf.cfg" "lib/librewolf/librewolf.cfg"))))
    (home-page "https://codeberg.org/librewolf/settings")
    (synopsis "Default LibreWolf config file")
    (description "Default LibreWolf config file as a package. Useful for union-builds.")
    (license license:mpl2.0)))

(define-public packages
  (let ((lst (list
              ;; FIXME: Legacyfox still seems broken, unsure exactly why. Maybe due to symlinks?
              (wrap-package librewolf
                            #:unite (list legacyfox-for-librewolf
                                          librewolf+legacyfox-config)))))
    (if (string= (gethostname) "okarthel")
        (map replace-mesa lst)
        lst)))
