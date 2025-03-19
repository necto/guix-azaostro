(define-module (upstreaming gnu packages cpp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages pkg-config))

;; Updated version
(define-public c++-gsl
  (package
    (name "c++-gsl")
    (version "4.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/microsoft/GSL.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "08w3ppd43wx9vq641ljw5izjd7p5w7drynw13ll9shwy41ydif9n"))))
    (build-system cmake-build-system)
    (native-inputs
     (list googletest pkg-config))
    (synopsis "Guidelines Support Library")
    (description "c++-gsl contains functions and types that are suggested for
use by the C++ Core Guidelines maintained by the Standard C++ Foundation.")
    (home-page "https://github.com/microsoft/GSL/")
    (license license:expat)))
