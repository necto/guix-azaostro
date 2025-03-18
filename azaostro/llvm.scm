(define-module (llvm)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (guix utils)
  #:use-module (guix gexp))

(define (package-with-configure-flags p flags)
  "Return P with FLAGS as additional 'configure' flags."
  (package/inherit p
    (arguments
     (substitute-keyword-arguments (package-arguments p)
       ((#:configure-flags original-flags #~(list))
        #~(append #$original-flags #$flags))))))

;;; Use LLD as the default linker instead of BFD used by clang-18.
;;; LLD has the advantage of being insensitive to the order in which
;;; it has libraries listed, which is useful when compiling our work project
;;; since it injects the home-built standard library to the CMAKE_CXX_FLAGS
;;; which is expanded in the beginning of a compiler invocation.
;;; Hold off on using clang-19 as it is not yet supported by the work project z3
;;; contains a bug in template code that breaks clang-19 compilation.
(define-public clang-with-lld-18
  (package
    (inherit (package-with-configure-flags clang-18 #~(list "-DCLANG_DEFAULT_LINKER=lld")))
    (inputs (modify-inputs (package-inputs clang-18) (replace "gcc" "gcc-13")))
    (name "clang-with-lld")))

(define-public clang-toolchain-with-lld-18
  (make-clang-toolchain clang-with-lld-18 libomp-18))
