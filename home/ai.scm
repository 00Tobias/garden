(define-module (home ai)
  #:use-module (srfi srfi-1)

  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)

  #:use-module ((gnu packages version-control) #:select (git-lfs))
  #:use-module ((gnu packages machine-learning) #:select (llama-cpp whisper-cpp))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain-14))

  #:use-module ((nongnu packages nvidia) #:select (nvidia-driver replace-mesa))

  #:use-module ((guix-science-nonfree packages cuda) #:select (cuda))

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((trowel) #:select (aggressively-optimize)))

(define llama-cpp-latest
  (let ((tag "b7032"))
    (package
      (inherit llama-cpp)
      (name "llama-cpp")
      (version tag)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/ggml-org/llama.cpp")
                (commit tag)))
         (file-name (git-file-name name tag))
         (sha256
          (base32 "01mw4a1n2ly8nyl005ii3w5713x9sfiwin171gn9np68cb5kgls3")))))))

(define llama-cpp-nvidia
  (package
    (inherit llama-cpp-latest)
    (arguments
     (substitute-keyword-arguments (package-arguments llama-cpp-latest)
       ((#:configure-flags flags #~'())
        #~(append (list "-DGGML_CUDA=ON")
                  ((@ (srfi srfi-1) fold) delete #$flags '("-DGGML_NATIVE=OFF"
                                                           "-DGGML_FMA=OFF"
                                                           "-DGGML_AVX2=OFF"
                                                           "-DGGML_AVX512=OFF"
                                                           "-DGGML_AVX512_VBMI=OFF"
                                                           "-DGGML_AVX512_VNNI=OFF"))))))
    (inputs
     (modify-inputs (package-inputs llama-cpp-latest)
       (prepend nvidia-driver cuda)))))

(define whisper-cpp-nvidia
  (package
    (inherit whisper-cpp)
    (arguments
     (substitute-keyword-arguments (package-arguments whisper-cpp)
       ((#:configure-flags flags #~'())
        #~(append (list "-DWHISPER_CUDA=ON")
                  ((@ (srfi srfi-1) fold) delete #$flags '("-DGGML_NATIVE=OFF"
                                                           "-DGGML_FMA=OFF"
                                                           "-DGGML_AVX2=OFF"
                                                           "-DGGML_AVX512=OFF"
                                                           "-DGGML_AVX512_VBMI=OFF"
                                                           "-DGGML_AVX512_VNNI=OFF"))))))
    (inputs
     (modify-inputs (package-inputs whisper-cpp)
       (prepend nvidia-driver cuda)))))

(define-public packages
  (list
   git-lfs
   (if (or (string= (gethostname) "okarthel")
           (string= (gethostname) "austrat"))
       ;; FIXME: Maximum supported nvcc gcc version, can be overwritten with the nvcc flag '-allow-unsupported-compiler'
       ;; (aggressively-optimize llama-cpp-nvidia #:toolchain gcc-toolchain-14)
       llama-cpp-nvidia
       llama-cpp-latest)
   (if (or (string= (gethostname) "okarthel")
           (string= (gethostname) "austrat"))
       (aggressively-optimize whisper-cpp-nvidia #:toolchain gcc-toolchain-14)
       whisper-cpp)))
