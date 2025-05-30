(define-module (trowel)
  #:use-module (ice-9 threads)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)

  #:use-module (nonguix utils)

  #:use-module ((gnu packages gl) #:select (mesa))
  #:use-module ((gnu packages mold) #:select (mold-as-ld-wrapper))
  #:use-module ((gnu packages gcc) #:select (gcc-14))
  #:use-module ((gnu packages bash) #:select (bash))

  #:use-module ((nongnu packages nvidia) #:select (nvdb))

  #:export (replace-mesa
            aggressively-optimize))

(define replace-mesa
  (package-input-grafting `((,mesa . ,nvdb))))

(define (aggressively-optimize p)
  (package-with-c-toolchain
   (package
     (inherit p)
     (native-inputs
      (modify-inputs (package-native-inputs p)
        (prepend mold-as-ld-wrapper))))
   `(("gcc-toolchain-optimized"
      ,((@@ (gnu packages commencement) make-gcc-toolchain)
        (package
          (inherit gcc-14)
          (inputs
           (modify-inputs (package-inputs gcc-14)
             (prepend bash)))
          (arguments
           (substitute-keyword-arguments (package-arguments gcc-14)
             ((#:phases phases)
              #~(modify-phases #$phases
                  (add-after 'install 'wrap-executables
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bash (string-append (assoc-ref inputs "bash") "/bin/bash")))
                        (for-each
                         (lambda (name)
                           (let ((name-real (string-append name "-real")))
                             (rename-file name name-real)
                             (call-with-output-file name
                               (lambda (port)
                                 (format port
                                         "#!~a~%exec \"~a\" \"$@\" ~a~%"
                                         bash
                                         name-real
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
                             (chmod name #o555)))
                         `(,(string-append out "/bin/gcc")
                           ,(string-append out "/bin/g++")
                           ,(string-append out "/bin/cpp")
                           ,(string-append out "/bin/c++"))))))))))))))))
