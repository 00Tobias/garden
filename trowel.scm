(define-module (trowel)
  #:use-module (ice-9 threads)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)

  #:use-module ((gnu packages mold) #:select (mold-as-ld-wrapper))
  #:use-module ((gnu packages commencement) #:select (make-gcc-toolchain))
  #:use-module ((gnu packages gcc) #:select (gcc-13))
  #:use-module ((gnu packages bash) #:select (bash))

  #:export (aggressively-optimize))

(define (aggressively-optimize p)
  (package-with-c-toolchain
   (package
     (inherit p)
     (native-inputs
      (modify-inputs (package-native-inputs p)
        (prepend mold-as-ld-wrapper))))
   `(("gcc-toolchain-optimized"
      ,(make-gcc-toolchain
        (package
          (inherit gcc-13)
          (inputs
           (modify-inputs (package-inputs gcc-13)
             (prepend bash)))
          (arguments
           (substitute-keyword-arguments (package-arguments gcc-13)
             ((#:phases phases)
              #~(modify-phases #$phases
                  (add-after 'install 'wrap-gcc
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bash (string-append (assoc-ref inputs "bash") "/bin/bash"))
                             (gcc (string-append out "/bin/gcc"))
                             (gcc-real (string-append out "/bin/.gcc-real")))
                        (rename-file gcc gcc-real)
                        (call-with-output-file gcc
                          (lambda (port)
                            (format port
                                    "#!~a~%exec \"~a\" \"$@\" ~a~%"
                                    bash
                                    gcc-real
                                    #$(string-append
                                       "-O3 "
                                       "-march=native "
                                       "-pipe "

                                       "-fuse-linker-plugin "
                                       "-flto=" (number->string (total-processor-count))
                                       " -fno-plt "

                                       "-fgraphite-identity "
                                       "-floop-nest-optimize "

                                       "-fipa-pta "
                                       "-fno-semantic-interposition "
                                       "-fdevirtualize-at-ltrans "

                                       "-fno-signed-zeros "
                                       "-fno-trapping-math "
                                       "-fassociative-math "
                                       "-freciprocal-math"))))
                        (chmod gcc #o555))))))))))))))
