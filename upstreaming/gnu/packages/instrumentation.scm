(define-module (upstreaming gnu packages instrumentation)
  #:use-module (gnu packages)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils))


(define-public uftrace-0.17
  (package
    (name "uftrace")
    (version "0.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/namhyung/uftrace")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0p1iy70hc4vl3j16j9vvlh5amvk06l3m35iic2crpavm240dw7y7"))
              (patches (search-patches "uftrace-fix-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      `((ice-9 match)
        ,@%default-gnu-modules)
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target)))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs target #:allow-other-keys)
              (let ((arch #$(platform-linux-architecture
                             (lookup-platform-by-target-or-system
                              (or (%current-target-system)
                                  (%current-system))))))
                (setenv "ARCH"
                        (match arch
                          ("arm64" "aarch64")
                          (_ arch)))
                (when target
                  (setenv "CROSS_COMPILE" (string-append target "-"))))
              (setenv "SHELL" (which "sh"))
              (let* ((python #$(this-package-input "python"))
                     (luajit #$(this-package-input "luajit"))
                     (libs (cond
                             ((and python luajit)
                              (list "-Wl,-rpath="
                                    python
                                    "/lib"
                                    ":"
                                    luajit
                                    "/lib"))
                             (python (list "-Wl,-rpath=" python "/lib"))
                             (luajit (list "-Wl,-rpath=" luajit "/lib"))
                             (#t #f))))
                (when libs
                  (setenv "LDFLAGS"
                          (apply string-append libs))))
              (invoke "./configure"
                      (string-append "--prefix="
                                     #$output))))
          (add-before 'check 'fix-shebang
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tests/t220_trace_script.py"
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))))
          (add-before 'check 'delete-network-tests
            (lambda _
              ;; These tests require network capability (localhost)
              (for-each delete-file
                        '("tests/t141_recv_basic.py"
                          "tests/t142_recv_multi.py"
                          "tests/t143_recv_kernel.py"
                          "tests/t150_recv_event.py"
                          "tests/t151_recv_runcmd.py"
                          "tests/t167_recv_sched.py")))))))
    (inputs (list capstone
                  elfutils
                  libunwind
                  python ;(optional) libpython3.10.so for python scripting
                  luajit ;(optional) libluajit-5.1.so for lua scripting
                  ncurses))
    (native-inputs (list luajit pandoc pkg-config python-wrapper))
    (home-page "https://github.com/namhyung/uftrace")
    (synopsis "Function graph tracer for C/C++/Rust")
    (description
     "uftrace is a tool for tracing and analyzing the execution of
programs written in C/C++.  It is heavily inspired by the ftrace framework of
the Linux kernel, while supporting userspace programs.  It supports various
kind of commands and filters to help analysis of the program execution and
performance.  It provides the command @command{uftrace}.  By default, it is
bundled with python-3 and luajit that you can delete in a package variant.")
    (license license:gpl2)))
