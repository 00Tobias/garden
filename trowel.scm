(define-module (trowel)
  #:use-module (ice-9 threads)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)

  #:use-module ((gnu packages mold) #:select (mold-as-ld-wrapper))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain-15))
  #:use-module ((gnu packages bash) #:select (bash))

  #:export (wrap-package aggressively-optimize))

(define* (wrap-package base #:key (flags '()) (env '()) (binaries '()) (unite '()))
  "Wrap the given BASE package with the operations passed.
FLAGS is a list of strings.
ENV is an associative list of environment variables and values.
BINARIES is a list of binary names to wrap with either FLAGS or ENV.
UNITE is a list of packages to union-build together."
  (package
    (name (string-append (package-name base) "-wrapped"))
    (version (package-version base))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build union)
                  (guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build union)
                       (guix build utils))
          (let ((flags? (not (null? '#$flags)))
                (env? (not (null? '#$env))))
            (union-build #$output (list #$@unite #$base) #:create-all-directories? #t)
            (when (or flags? env?)
              (for-each
               (lambda (name)
                 (let ((name-real (string-append name "-real")))
                   (rename-file name name-real)
                   (call-with-output-file name
                     (lambda (port)
                       (display
                        (string-append
                         (if env? (string-append
                                   (string-join
                                    (map (lambda (var)
                                           (string-append "export " (car var) "=" (cdr var)))
                                         '#$env)
                                    "\n")
                                   "\n")
                             "")
                         "#!" #$bash "/bin/bash\n"
                         "exec " name-real " \"$@\" "
                         (if flags? (string-join '#$flags " ") ""))
                        port)))
                   (chmod name #o555)))
               (map (lambda (bin)
                      (string-append #$output "/bin/" bin))
                    '#$binaries)))))))
    (native-search-paths (package-native-search-paths base))
    (search-paths (package-search-paths base))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 (append (list bash) (list base) (if (not (null? unite)) unite '()))))
    (synopsis (package-synopsis base))
    (description (package-description base))
    (home-page (package-home-page base))
    (license (package-license base))))

(define* (aggressively-optimize p #:key (toolchain gcc-toolchain-15))
  (package-with-c-toolchain
   (package
     (inherit p)
     (native-inputs
      (modify-inputs (package-native-inputs p)
        (prepend mold-as-ld-wrapper))))
   `(("gcc-toolchain-optimized"
      ,(wrap-package toolchain
                     #:binaries (list "gcc" "g++" "cpp" "c++")
                     #:flags (list "-O3"
                                   "-march=native"
                                   "-pipe"

                                   "-fuse-linker-plugin"
                                   (string-append "-flto=" (number->string (total-processor-count)))
                                   "-fno-plt"

                                   "-fgraphite-identity"
                                   "-floop-nest-optimize"

                                   "-fipa-pta"
                                   "-fno-semantic-interposition"
                                   "-fdevirtualize-at-ltrans"

                                   "-fno-signed-zeros"
                                   "-fno-trapping-math"
                                   "-fassociative-math"
                                   "-freciprocal-math"))))))
