(define-module (azaostro llvm)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  ;; for optimized-clang-with-lld
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages build-tools)   ; ninja
  #:use-module (gnu packages python))

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
;;; This should be superceded by clang-with-lld-20
(define-public clang-with-lld-18
  (package
    (inherit (package-with-configure-flags clang-18 #~(list "-DCLANG_DEFAULT_LINKER=lld")))
    (inputs (modify-inputs (package-inputs clang-18) (replace "gcc" "gcc-13")))
    (name "old-clang-with-lld")))

(define-public clang-toolchain-with-lld-18
  (make-clang-toolchain clang-with-lld-18 libomp-18))

;;; Use LLD as the default linker instead of BFD used by clang-20.
;;; LLD has the advantage of being insensitive to the order in which
;;; it has libraries listed, which is useful when compiling our work project
;;; since it injects the home-built standard library to the CMAKE_CXX_FLAGS
;;; which is expanded in the beginning of a compiler invocation.
(define-public clang-with-lld-20
  (package
    (inherit (package-with-configure-flags clang-20 #~(list "-DCLANG_DEFAULT_LINKER=lld")))
    (inputs (modify-inputs (package-inputs clang-20) (replace "gcc" "gcc-13")))
    (name "clang-with-lld")))

(define-public clang-toolchain-with-lld-20
  (make-clang-toolchain clang-with-lld-20 libomp-20))

;;; ---------------------------------------------------------------------------
;;; PGO + ThinLTO + jemalloc optimized Clang.
;;;
;;; Ported from ~/proj/optimized-clang-script/guix/optimized-clang.scm, which
;;; in turn replicates the 4-stage pipeline of build-optimized-clang.sh.
;;; Benchmarked against clang-with-lld-20 on a full sonar-cpp `asserts' build
;;; and found faster, hence promotion into this channel.
;;;
;;; Pinned to the same LLVM release as clang-with-lld-20 (20.1.8) on purpose:
;;; with the version held constant the difference between the two packages is
;;; the optimization pipeline alone, not a year of upstream Clang changes.
;;;
;;; Like clang-with-lld-20 this is coupled with LLD, and more tightly so: LLD
;;; is not merely the default linker (-DCLANG_DEFAULT_LINKER=lld), it is built
;;; as part of stage 4 and installed into the same output, so bin/ld.lld always
;;; matches the clang that drives it.  Nothing else needs to supply a linker.
;;;
;;; Caveats:
;;;   * -march=native: the output is specific to the CPU that built it and is
;;;     not reproducible elsewhere.  Build locally (--no-substitutes) and do
;;;     not expect a substitute server to have it.
;;;   * The build runs four LLVM builds back to back, including a full
;;;     instrumented training build.  Budget hours and tens of GB of scratch
;;;     space; intermediates are deleted before the output is finalised.
;;;   * Header/crt search paths are baked into bin/clang.cfg rather than taken
;;;     from the profile.  Config-file arguments are *prepended* to the command
;;;     line, so a project that brings its own C++ standard library must pass
;;;     --no-default-config (and re-supply the link flags) to keep the cfg's
;;;     -isystem entries from shadowing its own headers.
;;; ---------------------------------------------------------------------------

;;; Source and version track Guix's own llvm-20 (20.1.8) so the hash and the
;;; driver patches (clang-18.0-libc-search-path, clang-17.0-link-dsymutil-latomic)
;;; stay in sync with the channel we are grafted onto.  Retarget by changing
;;; this binding and the matching bootstrap/libomp inputs below.
(define %optimized-llvm llvm-20)

(define-public optimized-clang-with-lld
  (package
    (name "optimized-clang-with-lld")
    (version (package-version %optimized-llvm))
    (source (package-source %optimized-llvm))

    (build-system gnu-build-system)

    ;; Bootstrap compiler and build tools (build-time only)
    (native-inputs
     (list cmake-minimal
           ninja
           python-wrapper
           clang-20          ; bootstrap C/C++ compiler for stage 1
           lld-20))          ; bootstrap linker for stage 1

    ;; Runtime libraries embedded in or referenced by the final clang binary.
    ;; Use old-style alist spec so the key "gcc-lib" is explicit and unambiguous;
    ;; the implicit GCC from gnu-build-system keeps key "gcc" (used for C++ headers).
    (inputs
     `(("gcc-lib"       ,gcc "lib")          ; key "gcc-lib": libgcc_s.so, crtbegin.o
       ("jemalloc"      ,jemalloc)           ; key "jemalloc": libjemalloc_pic.a (static)
       ("zlib"          ,zlib)
       ("linux-headers" ,linux-libre-headers)))

    (arguments
     (list
      #:tests? #f
      #:phases
      #~(begin
          (use-modules (ice-9 ftw) (srfi srfi-1))
          (let*
            ;; ── helpers available to all phases ──────────────────────────────
            ((jobs (number->string (parallel-job-count)))

             ;; Write clang.cfg and clang++.cfg into BIN-DIR so the installed
             ;; clang can find Guix store paths for glibc headers/crt and GCC
             ;; C++ headers.  Mirrors the .cfg generation in build-optimized-clang.sh.
             (write-clang-cfg
              (lambda (bin-dir libc gcc gcc-lib linux-headers)
                (let* ((cxx-base  (string-append gcc "/include/c++"))
                       ;; Guix GCC uses a flat layout: include/c++/<header> with
                       ;; the arch dir (x86_64-unknown-linux-gnu) directly inside.
                       ;; Some GCCs use a versioned layout: include/c++/<ver>/<hdr>.
                       ;; Detect by looking for an arch dir in include/c++ itself.
                       (flat-arch   (let ((es (scandir
                                                cxx-base
                                                (lambda (f)
                                                  (string-contains f "-linux-")))))
                                      (and (pair? es) (car es))))
                       ;; Flat layout: cxx-dir IS cxx-base.
                       ;; Versioned layout: cxx-dir is the first subdir of cxx-base.
                       (cxx-dir     (if flat-arch
                                        cxx-base
                                        (let ((subs (scandir
                                                      cxx-base
                                                      (lambda (f)
                                                        (not (member f '("." "..")))))))
                                          (and (pair? subs)
                                               (string-append cxx-base "/"
                                                              (car subs))))))
                       (arch-entry  (or flat-arch
                                        (and cxx-dir
                                             (let ((es (scandir
                                                         cxx-dir
                                                         (lambda (f)
                                                           (string-contains f "-linux-")))))
                                               (and (pair? es) (car es))))))
                       (cfg (string-append
                             "--gcc-toolchain=" gcc-lib "\n"
                             "-L" gcc-lib "/lib\n"
                             "-L" libc "/lib\n"
                             ;; GCC C++ headers BEFORE glibc: required for
                             ;; #include_next <stdlib.h> inside <cstdlib>.
                             (if cxx-dir
                                 (string-append "-isystem " cxx-dir "\n")
                                 "")
                             (if (and cxx-dir arch-entry)
                                 (string-append "-isystem " cxx-dir "/"
                                                arch-entry "\n")
                                 "")
                             (if cxx-dir
                                 (string-append "-isystem " cxx-dir "/backward\n")
                                 "")
                             "-isystem " libc "/include\n"
                             "-B" libc "/lib\n"
                             ;; Linux kernel headers (linux/limits.h etc.).
                             ;; Guix glibc does not bundle kernel headers; they
                             ;; live in linux-libre-headers.  Must be a narrow
                             ;; store path — a broad dir like /usr/include can
                             ;; contain stale system packages (e.g. libz3-dev)
                             ;; that shadow FetchContent headers during builds.
                             (if linux-headers
                                 (string-append "-isystem " linux-headers "/include\n")
                                 ""))))
                  (for-each
                   (lambda (name)
                     (call-with-output-file (string-append bin-dir "/" name)
                       (lambda (port) (display cfg port))))
                   '("clang.cfg" "clang++.cfg")))))

             ;; cmake flags shared across all four stages.
             ;; Each stage adds its own -DCMAKE_EXE_LINKER_FLAGS (overriding
             ;; the one below) plus project/target specific flags.
             ;; zlib is included in BUILD_RPATH because generated host tools
             ;; (llvm-min-tblgen, clang-tidy-confusable-chars-gen, …) link
             ;; libz.so.1 and are executed immediately after being compiled.
             (common-cmake-flags
              (lambda (libc gcc-lib zlib install-rpath)
                (list
                 "-GNinja"
                 "-DCMAKE_BUILD_TYPE=Release"
                 "-DLLVM_ENABLE_ASSERTIONS=OFF"
                 "-DLLVM_ENABLE_ZSTD=OFF"
                 "-DLLVM_ENABLE_ZLIB=ON"
                 "-DLLVM_ENABLE_LIBXML2=OFF"
                 "-DLLVM_TARGETS_TO_BUILD=X86"
                 "-DCMAKE_CXX_STANDARD=17"
                 "-DLLVM_OPTIMIZED_TABLEGEN=ON"
                 "-DLLVM_INCLUDE_TESTS=OFF"
                 "-DLLVM_INCLUDE_EXAMPLES=OFF"
                 "-DLLVM_INCLUDE_BENCHMARKS=OFF"
                 "-DLLVM_ENABLE_DUMP=ON"
                 (string-append "-DCMAKE_EXE_LINKER_FLAGS=-L" libc "/lib")
                 (string-append "-DCMAKE_SHARED_LINKER_FLAGS=-L" libc "/lib")
                 (string-append "-DCMAKE_BUILD_RPATH="
                                gcc-lib "/lib:" zlib "/lib")
                 (string-append "-DCMAKE_INSTALL_RPATH=" install-rpath))))

             ;; Flags for PGO/optimized stages: -O3 -march=native + anti-interposition
             (perf-flags
              "-O3 -march=native -fno-semantic-interposition -fno-plt -fno-pie")

             ;; Linker flags that embed jemalloc and preserve relocs for BOLT.
             ;; --whole-archive forces malloc/free override symbols into the binary.
             ;; --icf=none disables LLD's Identical Code Folding.  With ICF enabled,
             ;; LLD can merge identical function sections while leaving stale entries
             ;; in .rela.text and .eh_frame that point to the eliminated copies.
             ;; BOLT then encounters .rela.text offsets with no covering function,
             ;; triggering a null dereference in handleRelocation.  Disabling ICF
             ;; ensures every code region retains its own section, symbol, and
             ;; relocation entries in a consistent state.
             (jemalloc-linker-flags
              (lambda (libc jemalloc)
                (string-append
                 "-L" libc "/lib"
                 " -fuse-ld=lld -Wl,--emit-relocs -Wl,--no-pie -Wl,--icf=none -Wl,-z,now"
                 " -Wl,--whole-archive " jemalloc "/lib/libjemalloc_pic.a"
                 " -Wl,--no-whole-archive"
                 " -Wl,--push-state,--no-as-needed"
                 " -ldl -lstdc++ -Wl,--pop-state"))))

          ;; ── phase list ───────────────────────────────────────────────────
          (modify-phases %standard-phases
            ;; Skip phases that are slow on a large monorepo or don't apply
            (delete 'patch-source-shebangs)
            (delete 'patch-usr-bin-file)
            (delete 'configure)
            (delete 'build)
            (delete 'check)

            ;; ── Stage 0a: hide libstdc++ and glibc from CPLUS_INCLUDE_PATH ──
            ;; Guix puts both GCC's include/c++ and glibc's include on
            ;; CPLUS_INCLUDE_PATH.  Stage 1 builds compiler-rt, which compiles
            ;; with -nostdinc++; that switches off clang's own libstdc++
            ;; detection but NOT CPLUS_INCLUDE_PATH, which clang honours
            ;; regardless.  So compiler-rt TUs end up including libstdc++
            ;; headers they were explicitly compiled to avoid, and break in two
            ;; different ways:
            ;;
            ;;   * #include <stdlib.h> hits the C-compatibility wrapper
            ;;     include/c++/stdlib.h -> <cstdlib> -> fatal error:
            ;;     'bits/c++config.h' file not found (that header lives in the
            ;;     arch subdir include/c++/<triple>, which clang would normally
            ;;     have added itself).
            ;;   * nsan.h's #include <math.h> hits include/c++/math.h ->
            ;;     <cmath> -> bits/stl_pair.h, which declares `array' -- a name
            ;;     compiler-rt's sanitizer_redefine_builtins.h deliberately
            ;;     poisons: error: redefinition of 'array' as different kind of
            ;;     symbol.
            ;;
            ;; Removing both directories fixes the whole class rather than
            ;; individual headers: with libstdc++ off the path, -nostdinc++ TUs
            ;; see no wrapper at all, which is what the flag asks for.  glibc has
            ;; to go too -- dropping only include/c++ leaves glibc's include
            ;; ahead of the libstdc++ directory clang locates on its own, which
            ;; breaks `#include_next <stdlib.h>' inside <cstdlib> for every
            ;; ordinary C++ TU.  With neither present, normal C++ TUs get
            ;; libstdc++ and glibc from clang's own search logic, in the right
            ;; order.
            ;;
            ;; Guix's own clang-runtime carries exactly this workaround, for the
            ;; same reason, in its hide-glibc phase (gnu/packages/llvm.scm);
            ;; see also <https://issues.guix.info/issue/36882>.
            ;;
            ;; Stages 2-4 are unaffected: they build no compiler-rt, and they
            ;; run the stage-1 clang with the generated bin/clang.cfg, whose
            ;; -isystem list covers include/c++, its arch subdir, backward and
            ;; glibc's include explicitly.  Only C++ is touched; C_INCLUDE_PATH
            ;; is left alone, as C has no wrapper-header problem.
            (add-after 'set-paths 'hide-libstdc++-and-glibc-from-cxx-path
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((libc-include (let ((libc (assoc-ref inputs "libc")))
                                       (and libc
                                            (string-append libc "/include"))))
                       (path (getenv "CPLUS_INCLUDE_PATH")))
                  (when path
                    (setenv
                     "CPLUS_INCLUDE_PATH"
                     (string-join
                      (filter (lambda (dir)
                                (and (not (string-suffix? "/include/c++" dir))
                                     (not (and libc-include
                                               (string=? dir libc-include)))))
                              (string-split path #\:))
                      ":"))))))

            ;; ── Stage 0: patch source for Guix store paths ─────────────────
            ;; Mirrors clang-from-llvm's add-missing-triplets and
            ;; set-glibc-file-names phases from gnu/packages/llvm.scm.
            ;; The clang-18.0-libc-search-path.patch (applied during unpack)
            ;; already inserted the @GLIBC_LIBDIR@ placeholder; we substitute
            ;; the actual store path here.
            (add-after 'unpack 'patch-source-for-guix
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((libc (assoc-ref inputs "libc"))
                      (gcc  (assoc-ref inputs "gcc")))
                  ;; Teach clang about Guix-specific GNU triplets
                  (substitute* "clang/lib/Driver/ToolChains/Gnu.cpp"
                    (("\"aarch64-linux-gnu\"," all)
                     (string-append "\"aarch64-unknown-linux-gnu\", " all))
                    (("\"arm-linux-gnueabihf\"," all)
                     (string-append all " \"arm-unknown-linux-gnueabihf\","))
                    (("\"i686-pc-linux-gnu\"," all)
                     (string-append "\"i686-unknown-linux-gnu\", " all)))
                  ;; Point clang at Guix's glibc and GCC C++ headers
                  (substitute* "clang/lib/Driver/ToolChains/Linux.cpp"
                    (("(^[[:blank:]]+LibDir = ).*" _ decl)
                     (string-append decl "\"" libc "/lib\";\n"))
                    (("LibStdCXXIncludePathCandidates\\[\\] = \\{")
                     (string-append
                      "LibStdCXXIncludePathCandidates[] = { \""
                      gcc "/include/c++\","))
                    (("@GLIBC_LIBDIR@")
                     (string-append libc "/lib"))))))

            ;; ── Stage 1: bootstrap build ────────────────────────────────────
            ;; Builds clang+lld using the native-input clang-20.
            ;; Output: ../stage1-install/ with working clang + .cfg files.
            (add-after 'patch-source-for-guix 'stage1-build
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((srcdir     (getcwd))
                       (root       (dirname srcdir))
                       (s1-build   (string-append root "/stage1-build"))
                       (s1-install (string-append root "/stage1-install"))
                       (libc         (assoc-ref inputs "libc"))
                       (gcc          (assoc-ref inputs "gcc"))
                       (gcc-lib      (assoc-ref inputs "gcc-lib"))
                       (zlib         (assoc-ref inputs "zlib"))
                       (linux-headers (assoc-ref inputs "linux-headers"))
                       ;; native-inputs are merged into inputs in gnu-build-system phases
                       (bootstrap  (assoc-ref inputs "clang"))
                       (s1-clang   (string-append bootstrap "/bin/clang"))
                       (s1-clangxx (string-append bootstrap "/bin/clang++"))
                       (s1-lld     (string-append (assoc-ref inputs "lld")
                                                  "/bin/ld.lld")))
                  (mkdir-p s1-build)
                  (apply invoke
                         (append
                          (list "cmake")
                          (common-cmake-flags libc gcc-lib zlib
                                              ;; Include zlib so the installed clang
                                              ;; binary can find libz.so.1 at runtime
                                              ;; (clang links libz when ZLIB=ON).
                                              ;;
                                              ;; glibc is here for the compiler-rt
                                              ;; *shared* runtimes this stage builds --
                                              ;; libclang_rt.{hwasan,hwasan_aliases,nsan,
                                              ;; scudo_standalone,ubsan_standalone}.so --
                                              ;; which install-compiler-rt copies into
                                              ;; $out.  They list ld-linux-x86-64.so.2
                                              ;; (and libm) as DT_NEEDED, so without
                                              ;; glibc/lib on their RUNPATH the
                                              ;; validate-runpath phase rejects them after
                                              ;; the whole build has already succeeded:
                                              ;;
                                              ;;   libclang_rt.hwasan.so: error: depends
                                              ;;   on 'ld-linux-x86-64.so.2', which cannot
                                              ;;   be found in RUNPATH (...)
                                              ;;
                                              ;; Guix's own clang-runtime ships these
                                              ;; libraries with glibc/lib in RUNPATH too.
                                              (string-append gcc-lib "/lib:" zlib "/lib:"
                                                             libc "/lib"))
                          (list
                           (string-append "-DCMAKE_C_COMPILER=" s1-clang)
                           (string-append "-DCMAKE_CXX_COMPILER=" s1-clangxx)
                           (string-append "-DCMAKE_INSTALL_PREFIX=" s1-install)
                           (string-append "-DLLVM_USE_LINKER=" s1-lld)
                           "-DLLVM_ENABLE_LTO=OFF"
                           ;; Stage1 needs clang + lld + llvm-profdata, and it
                           ;; is also where the *shipped* compiler-rt comes from
                           ;; (see the install-compiler-rt phase).  Building the
                           ;; runtimes here rather than in stage 4 keeps them out
                           ;; of the ThinLTO build: a libclang_rt.*.a full of
                           ;; bitcode members would force every user link that
                           ;; enables a sanitizer through LTO.
                           ;;
                           ;; Excluding clang-tools-extra avoids clang-tidy-
                           ;; confusable-chars-gen needing libz.so.1 at build
                           ;; time (rpath only covers gcc-lib, not zlib);
                           ;; stage 4 builds it, where the rpath does cover zlib.
                           "-DLLVM_ENABLE_PROJECTS=clang;lld;compiler-rt"
                           (string-append srcdir "/llvm")
                           "-B" s1-build)))
                  (invoke "cmake" "--build" s1-build "-j" jobs)
                  (invoke "cmake" "--build" s1-build "--target" "install"
                          "-j" jobs)
                  ;; Generate .cfg so stage1 clang can find Guix headers/crt
                  (write-clang-cfg (string-append s1-install "/bin")
                                   libc gcc gcc-lib linux-headers))))

            ;; ── Stage 2: PGO-instrumented build ────────────────────────────
            ;; Builds an IR-instrumented clang using the stage1 compiler.
            ;; The instrumented binary writes .profraw files at runtime.
            (add-after 'stage1-build 'stage2-build
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((srcdir      (getcwd))
                       (root        (dirname srcdir))
                       (s1-install  (string-append root "/stage1-install"))
                       (s2-build    (string-append root "/stage2-build"))
                       (pgo-dir     (string-append root "/pgo-profiles"))
                       (libc        (assoc-ref inputs "libc"))
                       (gcc-lib     (assoc-ref inputs "gcc-lib"))
                       (zlib        (assoc-ref inputs "zlib"))
                       (jemalloc    (assoc-ref inputs "jemalloc"))
                       (s1-clang    (string-append s1-install "/bin/clang"))
                       (s1-clangxx  (string-append s1-install "/bin/clang++"))
                       (s1-lld      (string-append s1-install "/bin/ld.lld"))
                       (vp          "12"))
                  (mkdir-p (string-append pgo-dir "/raw"))
                  (mkdir-p s2-build)
                  ;; CFLAGS/CXXFLAGS: must match stage4 so profiles reflect
                  ;; the same code layout as the final binary.
                  (setenv "CFLAGS"
                          (string-append perf-flags
                                         " -mllvm -vp-counters-per-site=" vp))
                  (setenv "CXXFLAGS"
                          (string-append perf-flags
                                         " -mllvm -vp-counters-per-site=" vp))
                  (apply invoke
                         (append
                          (list "cmake")
                          (common-cmake-flags libc gcc-lib zlib
                                              (string-append gcc-lib "/lib"))
                          (list
                           (string-append "-DCMAKE_C_COMPILER=" s1-clang)
                           (string-append "-DCMAKE_CXX_COMPILER=" s1-clangxx)
                           (string-append "-DLLVM_USE_LINKER=" s1-lld)
                           "-DLLVM_BUILD_INSTRUMENTED=IR"
                           (string-append "-DLLVM_VP_COUNTERS_PER_SITE=" vp)
                           "-DLLVM_ENABLE_LTO=Thin"
                           (string-append "-DLLVM_PROFILE_DATA_DIR=" pgo-dir "/raw")
                           "-DLLVM_ENABLE_PROJECTS=clang"
                           (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                          (jemalloc-linker-flags libc jemalloc))
                           (string-append srcdir "/llvm")
                           "-B" s2-build)))
                  (unsetenv "CFLAGS")
                  (unsetenv "CXXFLAGS")
                  ;; Build only the clang target (no install needed)
                  (invoke "cmake" "--build" s2-build
                          "--target" "clang" "-j" jobs)
                  ;; Give stage2 clang the .cfg so it can find headers/crt
                  ;; during the training workload (stage 3).
                  (for-each
                   (lambda (name)
                     (copy-file
                      (string-append s1-install "/bin/clang.cfg")
                      (string-append s2-build "/bin/" name)))
                   '("clang.cfg" "clang++.cfg"))
                  ;; Nothing to symlink: stage1 has no compiler-rt install.
                )))

            ;; ── Stage 3: profile collection ─────────────────────────────────
            ;; Runs a realistic training workload (build clang from source)
            ;; using the instrumented compiler, then merges the raw profiles.
            (add-after 'stage2-build 'stage3-collect-profiles
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((srcdir      (getcwd))
                       (root        (dirname srcdir))
                       (s1-install  (string-append root "/stage1-install"))
                       (s2-build    (string-append root "/stage2-build"))
                       (pgo-dir     (string-append root "/pgo-profiles"))
                       (train-build (string-append root "/pgo-training"))
                       (libc        (assoc-ref inputs "libc"))
                       (gcc-lib     (assoc-ref inputs "gcc-lib"))
                       (zlib        (assoc-ref inputs "zlib"))
                       (s2-clang    (string-append s2-build "/bin/clang"))
                       (s2-clangxx  (string-append s2-build "/bin/clang++"))
                       (profdata    (string-append s1-install "/bin/llvm-profdata")))
                  ;; Each clang invocation writes a uniquely named .profraw
                  (setenv "LLVM_PROFILE_FILE"
                          (string-append pgo-dir "/raw/clang-%m-%p.profraw"))
                  ;; Instrumented binaries need to find libstdc++.so at runtime
                  (setenv "LD_LIBRARY_PATH" (string-append gcc-lib "/lib"))
                  (mkdir-p train-build)
                  ;; Configure training build with the instrumented compiler
                  (apply invoke
                         (list "cmake"
                               (string-append "-DCMAKE_C_COMPILER=" s2-clang)
                               (string-append "-DCMAKE_CXX_COMPILER=" s2-clangxx)
                               "-GNinja"
                               "-DCMAKE_BUILD_TYPE=Release"
                               "-DLLVM_TARGETS_TO_BUILD=X86"
                               "-DLLVM_ENABLE_PROJECTS=clang;lld"
                               "-DLLVM_ENABLE_LTO=OFF"
                               "-DLLVM_INCLUDE_TESTS=OFF"
                               "-DLLVM_INCLUDE_EXAMPLES=OFF"
                               "-DLLVM_INCLUDE_BENCHMARKS=OFF"
                               "-DLLVM_OPTIMIZED_TABLEGEN=ON"
                               "-DCMAKE_CXX_STANDARD=17"
                               (string-append "-DCMAKE_EXE_LINKER_FLAGS=-L"
                                              libc "/lib")
                               (string-append "-DCMAKE_BUILD_RPATH="
                                              gcc-lib "/lib:" zlib "/lib")
                               (string-append srcdir "/llvm")
                               "-B" train-build))
                  ;; Partial build is intentional: even failed TUs yield valid
                  ;; profile data for the TUs that did compile.
                  (system* "cmake" "--build" train-build
                           "--target" "clang" "-j" jobs)
                  (unsetenv "LLVM_PROFILE_FILE")
                  (unsetenv "LD_LIBRARY_PATH")
                  ;; Merge all raw profiles into a single indexed .profdata
                  (let ((profraw-files
                         (filter (lambda (f) (string-suffix? ".profraw" f))
                                 (scandir (string-append pgo-dir "/raw/")
                                          (lambda (f)
                                            (not (member f '("." ".."))))))))
                    (apply invoke
                           (append
                            (list profdata "merge"
                                  "--sparse"
                                  (string-append "--output="
                                                 pgo-dir "/clang.profdata"))
                            (map (lambda (f)
                                   (string-append pgo-dir "/raw/" f))
                                 profraw-files))))
                  ;; Free ~15-20 GB: delete the instrumented build and training tree
                  (delete-file-recursively s2-build)
                  (delete-file-recursively train-build)
                  (delete-file-recursively (string-append pgo-dir "/raw")))))

            ;; ── Stage 4: final PGO + ThinLTO optimized build ────────────────
            ;; Uses stage1 compiler + PGO profile data + ThinLTO + O3 + native.
            (add-after 'stage3-collect-profiles 'stage4-build
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out        (assoc-ref outputs "out"))
                       (srcdir     (getcwd))
                       (root       (dirname srcdir))
                       (s1-install (string-append root "/stage1-install"))
                       (s4-build   (string-append root "/stage4-build"))
                       (pgo-dir    (string-append root "/pgo-profiles"))
                       (libc       (assoc-ref inputs "libc"))
                       (gcc-lib    (assoc-ref inputs "gcc-lib"))
                       (zlib       (assoc-ref inputs "zlib"))
                       (jemalloc   (assoc-ref inputs "jemalloc"))
                       (s1-clang   (string-append s1-install "/bin/clang"))
                       (s1-clangxx (string-append s1-install "/bin/clang++"))
                       (s1-lld     (string-append s1-install "/bin/ld.lld"))
                       ;; RPATH for installed binaries: own lib dir (libclang.so etc),
                       ;; libgcc_s, libz, ld-linux
                       (install-rpath
                        (string-join (list (string-append out "/lib")
                                           (string-append gcc-lib "/lib")
                                           (string-append zlib "/lib")
                                           (string-append libc "/lib"))
                                     ":")))
                  (mkdir-p s4-build)
                  (setenv "CFLAGS" perf-flags)
                  (setenv "CXXFLAGS" perf-flags)
                  (apply invoke
                         (append
                          (list "cmake")
                          (common-cmake-flags libc gcc-lib zlib install-rpath)
                          (list
                           (string-append "-DCMAKE_C_COMPILER=" s1-clang)
                           (string-append "-DCMAKE_CXX_COMPILER=" s1-clangxx)
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           (string-append "-DLLVM_USE_LINKER=" s1-lld)
                           "-DLLVM_ENABLE_LTO=Thin"
                           (string-append "-DLLVM_PROFDATA_FILE="
                                          pgo-dir "/clang.profdata")
                           "-DLLVM_USE_SPLIT_DWARF=ON"
                           "-DLLVM_PARALLEL_LINK_JOBS=2"
                           ;; lld is built and installed alongside clang, so
                           ;; CLANG_DEFAULT_LINKER=lld below always resolves to
                           ;; a matching ld.lld in this very output.
                           ;;
                           ;; clang-tools-extra is what makes this a drop-in
                           ;; replacement for clang-toolchain-with-lld-20:
                           ;; without it there is no clangd, clang-tidy,
                           ;; clang-query, run-clang-tidy, …
                           "-DLLVM_ENABLE_PROJECTS=clang;clang-tools-extra;lld;polly;bolt"
                           ;; FileCheck, count, not, split-file, yaml2obj — Guix's
                           ;; own llvm sets this too, and running the LLVM test
                           ;; suite needs them.  Unit tests stay off
                           ;; (LLVM_INCLUDE_TESTS in the common flags); these are
                           ;; utils, which are built independently of that.
                           "-DLLVM_INSTALL_UTILS=ON"
                           "-DLLVM_ENABLE_BINDINGS=OFF"
                           "-DCLANG_DEFAULT_LINKER=lld"
                           ;; Override the CMAKE_EXE_LINKER_FLAGS from common flags
                           (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                          (jemalloc-linker-flags libc jemalloc))
                           (string-append srcdir "/llvm")
                           "-B" s4-build)))
                  (unsetenv "CFLAGS")
                  (unsetenv "CXXFLAGS"))))

            ;; Install stage4 build to $out
            (replace 'install
              (lambda _
                (let* ((root    (dirname (getcwd)))
                       (s4-build (string-append root "/stage4-build")))
                  (invoke "cmake" "--build" s4-build
                          "--target" "install" "-j" jobs))))

            ;; ── compiler-rt ────────────────────────────────────────────────
            ;; Stage 4 does not build compiler-rt (see stage1-build), so the
            ;; freshly installed clang has a resource directory with headers but
            ;; no lib/, and every -fsanitize=… / -fprofile-generate link fails on
            ;; a missing libclang_rt.*.a.  clang looks for those relative to its
            ;; own binary (/proc/self/exe), so they have to live in $out; unlike
            ;; Guix's clang, whose driver is patched to reach into a separate
            ;; clang-runtime package, this one is self-contained.
            ;;
            ;; Both stages come from the same source tree and the same version,
            ;; so stage 1's runtimes drop straight into stage 4's resource dir.
            (add-after 'install 'install-compiler-rt
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out        (assoc-ref outputs "out"))
                       (root       (dirname (getcwd)))
                       (s1-install (string-append root "/stage1-install"))
                       ;; Resource dirs are lib/clang/<major>; find them rather
                       ;; than deriving the name from the version string.
                       (resource-dirs
                        (lambda (prefix)
                          (let ((base (string-append prefix "/lib/clang")))
                            (if (file-exists? base)
                                (scandir base
                                         (lambda (f)
                                           (not (member f '("." "..")))))
                                '())))))
                  (for-each
                   (lambda (ver)
                     (let ((src (string-append s1-install "/lib/clang/" ver "/lib"))
                           (dst (string-append out "/lib/clang/" ver "/lib")))
                       (when (file-exists? src)
                         (mkdir-p dst)
                         (copy-recursively src dst))))
                   (resource-dirs s1-install))
                  ;; Fail loudly rather than shipping a compiler that cannot
                  ;; link a sanitized binary.
                  (unless (any (lambda (ver)
                                 (file-exists?
                                  (string-append out "/lib/clang/" ver "/lib")))
                               (resource-dirs out))
                    (error "no compiler-rt libraries were installed")))))

            ;; Write .cfg files so the installed clang finds Guix store paths
            (add-after 'install-compiler-rt 'generate-cfg
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out          (assoc-ref outputs "out"))
                       (libc         (assoc-ref inputs "libc"))
                       (gcc          (assoc-ref inputs "gcc"))
                       (gcc-lib      (assoc-ref inputs "gcc-lib"))
                       (linux-headers (assoc-ref inputs "linux-headers"))
                       (bin-dir      (string-append out "/bin")))
                  (write-clang-cfg bin-dir libc gcc gcc-lib linux-headers)
                  ;; Also write cfg for the versioned clang-NN binary
                  (for-each
                   (lambda (name)
                     (when (and (string-prefix? "clang-" name)
                                (not (string-suffix? ".cfg" name))
                                (< 6 (string-length name))
                                (char-numeric? (string-ref name 6)))
                       (copy-file (string-append bin-dir "/clang.cfg")
                                  (string-append bin-dir "/" name ".cfg"))))
                   (scandir bin-dir (lambda (f)
                                      (not (member f '("." "..")))))))))

            ;; Delete all intermediate build artifacts to keep the store lean.
            ;; Only the stage4 install (now in $out) is kept.
            (add-after 'generate-cfg 'cleanup-intermediates
              (lambda _
                (let ((root (dirname (getcwd))))
                  (for-each
                   (lambda (dir)
                     (let ((path (string-append root "/" dir)))
                       (when (file-exists? path)
                         (delete-file-recursively path))))
                   '("stage1-build" "stage1-install"
                     "stage4-build"  "pgo-profiles"))))))))))

    ;; Taken from clang-20 rather than spelled out, so a drop-in replacement
    ;; stays a drop-in replacement as upstream Guix edits them.  These are what
    ;; make a profile containing this package define C_INCLUDE_PATH,
    ;; CPLUS_INCLUDE_PATH, OBJC*_INCLUDE_PATH and LIBRARY_PATH; without them,
    ;; headers of other packages in the same profile are not found implicitly.
    (native-search-paths (package-native-search-paths clang-20))
    (search-paths (package-search-paths clang-20))

    (home-page "https://llvm.org")
    (synopsis "PGO+ThinLTO+jemalloc optimized Clang with LLD")
    (description
     "A Clang compiler built via a 4-stage PGO pipeline: bootstrap,
PGO-instrumented build, profile collection (building Clang itself as the
training workload), and final PGO+ThinLTO optimized build.  The final
binary includes statically linked jemalloc for fast allocation,
@code{-O3 -march=native} for full CPU-specific optimization, and preserves
relocations for optional BOLT post-link optimization.

LLD is built into the same output and is the default linker, so
@command{ld.lld} always matches the @command{clang} that invokes it.

This package is machine-specific (uses @code{-march=native}) and is not
reproducible across different CPUs.")
    (license license:asl2.0)
    ;; Inherit clang-20's properties so anything keying off them behaves the
    ;; same.  Deliberately *not* 'tunable?': -march=native already specialises
    ;; the build for this CPU, and marking it tunable only adds another way for
    ;; a package transformation to trigger the whole multi-hour rebuild.
    (properties (package-properties clang-20))))

;;; Profile-ready variant, and a drop-in replacement for
;;; clang-toolchain-with-lld-20: same bin/ commands (clangd, clang-tidy,
;;; clang-format, the llvm-* tools, FileCheck/not/count), same compiler-rt
;;; runtimes, same C_INCLUDE_PATH / CPLUS_INCLUDE_PATH / LIBRARY_PATH search
;;; paths, same binutils + glibc + libomp union.
;;;
;;; Two deliberate differences remain, both load-bearing for the performance
;;; this package exists for — change either and you give that up:
;;;
;;;   * X86 only (LLVM_TARGETS_TO_BUILD in common-cmake-flags).  Guix's llvm
;;;     builds every target; here --target=aarch64-… will not work and neither
;;;     will building an out-of-tree LLVM tool for another architecture.
;;;   * No libLLVM-20.so / no LLVM_LINK_LLVM_DYLIB.  Everything is statically
;;;     linked, which is a large part of why it is fast; `llvm-config
;;;     --link-shared' and anything expecting to link libLLVM.so will fail.
(define-public optimized-clang-toolchain-with-lld
  (make-clang-toolchain optimized-clang-with-lld libomp-20))
