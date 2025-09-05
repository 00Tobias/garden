(define-module (home ai)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module ((gnu packages version-control) #:select (git-lfs))
  #:use-module ((gnu packages machine-learning) #:select (llama-cpp))
  #:use-module ((gnu packages gcc) #:select (gcc))

  #:use-module ((nongnu packages nvidia) #:select (replace-mesa))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((trowel) #:select (aggressively-optimize)))

(define whisper-cpp
  (package
    (name "whisper-cpp")
    (version "1.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggerganov/whisper.cpp")
             (commit "ffef323c4cfa8596cb91cf92d6f791f01a44335e")))
       (sha256 (base32 "0j9ksbkvpq8mviyc0xm1v84h7pbgfwyf8c1agvns10r0b3hw6bxs"))))
    (build-system cmake-build-system)
    ;; TODO: Split outputs
    (arguments
     '(#:tests? #f
       #:configure-flags
       `("-DWHISPER_STANDALONE=ON"
         "-DWHISPER_BUILD_EXAMPLES=ON"
         "-DWHISPER_BUILD_TESTS=OFF"
         ,(string-append "-DCMAKE_EXE_LINKER_FLAGS=-Wl,-rpath="
                         (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/")))
               ;; TODO: Rename
               (mkdir-p bin)
               (copy-file "bin/main" (string-append bin "/whisper"))))))))
    (home-page "https://github.com/ggerganov/whisper.cpp")
    (synopsis "Port of OpenAI's Whisper model in C/C++")
    (description "Port of OpenAI's Whisper model in C/C++.")
    (license license:x11)))

(define whisper-cpp-nvidia
  (package
    (inherit whisper-cpp)
    (inputs
     (modify-inputs (package-inputs whisper-cpp)
       (prepend nvidia-driver-beta cuda)))
    (arguments
     (substitute-keyword-arguments (package-arguments whisper-cpp)
       ((#:configure-flags flags #~'())
        #~(append #$flags (list "-DWHISPER_CUDA=ON")))))))

(define llama-cpp-server
  (package
    (inherit llama-cpp)
    (arguments
     (substitute-keyword-arguments (package-arguments llama-cpp)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-server
              (lambda _
                (with-directory-excursion (string-append #$output "/bin")
                  (symlink "server" "llama-server"))))))
       ((#:configure-flags flags)
        #~(list "-DLLAMA_BUILD_SERVER=ON"))))))

(define llama-cpp-server-nvidia
  (package
    (inherit llama-cpp-server)
    (inputs
     (modify-inputs (package-inputs llama-cpp)
       (prepend nvidia-driver cuda)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments llama-cpp-server)
       ((#:configure-flags flags #~'())
        #~(append #$flags (list "-DLLAMA_CUBLAS=ON")))))))

(define-public packages
  (list
   git-lfs
   ;; TODO: aggressively-optimize (cuda gcc max version)
   (if (string= (gethostname) "okarthel")
       whisper-cpp-nvidia
       whisper-cpp)
   (if (string= (gethostname) "okarthel")
       llama-cpp-server-nvidia
       llama-cpp-server)))
