(define-module (upstreaming gnu packages telegram)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages video)
  #:use-module (gnu packages image)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages language)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cpp)
  #:use-module (upstreaming gnu packages cpp) ; patched c++-gsl
  #:use-module (gnu packages linux)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages glib)
  #:use-module (upstreaming gnu packages glib) ; patched cppgir-for-telegram-desktop
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages telegram))

;; Make sure patches in this repository are visible,
;; but keep other dirs for patches of the dependencies
(%patch-path (append (if (current-filename)
                         (list (string-append (dirname (current-filename)) "/patches/"))
                         '())
                     (map (lambda (directory)
                            (string-append directory "/upstreaming/gnu/packages/patches"))
                          %load-path)
                     (%patch-path)))

(define %telegram-version "5.12.3")

(define cmake-helpers-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/cmake_helpers.git")
          (commit "90e6d73100a9fd2dc4c30a270c3bbc1d35924f32")))
    (file-name
     (git-file-name "cmake-helpers-for-telegram-desktop" %telegram-version))
    (patches
     ;; https://github.com/desktop-app/cmake_helpers/pull/305
     (search-patches "telegram-desktop-unbundle-cppgir.patch"))
    (sha256
     (base32
      "0mpz0adsyzsr5crxcjfr96x133yl4j55nm5f3gv5w1q1g1vk283r"))))

(define codegen-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/codegen.git")
          (commit "4155b9ae2d4c5a37b9738afa8ef9fa20d8fdcb44")))
    (file-name
     (git-file-name "codegen-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1h45rsi4nrkr3j312ji8qlkbzsb948nszmnylwimh5v65n90p21a"))))

(define lib-base-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_base.git")
          (commit "b28088164b7a46c70ae2cfd9daf865f6425610b2")))
    (file-name
     (git-file-name "lib-base-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1dwqdnasn3igr7i14hkx1glxj0gn6rd852bj0w3k1ai9j295wnfz"))))

(define lib-crl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_crl.git")
          (commit "c1d6b0273653095b10b4d0f4f7c30b614b690fd5")))
    (file-name
     (git-file-name "lib-crl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1sxn3qccsfbx1289z0fdrb4cggs16a8r75ic6wi81c6lnkrdi3wl"))))

(define lib-lottie-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_lottie.git")
          (commit "3eb4a97f1dd038bc4b6bd2884262242382a37e79")))
    (file-name
     (git-file-name "lib-lottie-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "04sgbspp0wngpr5w2wjfl1hwk1kiy8kwk2sz841f1yj231s7v6xw"))))

(define lib-qr-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_qr.git")
          (commit "6fdf60461444ba150e13ac36009c0ffce72c4c83")))
    (file-name
     (git-file-name "lib-qr-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1i5n3qvjkf4nv8k75cc0a71xyvpklc4nzg4k2zsfr2pxk0cy7hkw"))))

(define lib-rpl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_rpl.git")
          (commit "9a3ce435f4054e6cbd45e1c6e3e27cfff515c829")))
    (file-name
     (git-file-name "lib-rpl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "02g84i1d1hb5kqnhfr90fnw8nq1khqky95x52v2kx8zz05i1r8vs"))))

(define lib-spellcheck-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_spellcheck.git")
          (commit "8809cc72d07087ec61a1e8569de4da95aac45474")))
    (file-name
     (git-file-name "lib-spellcheck-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0zxk7vxr29f8scdi2ymvvz4zh9zkln8r57y1n65x0vfi8vdihn1a"))))

(define lib-storage-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_storage.git")
          (commit "ccdc72548a5065b5991b4e06e610d76bc4f6023e")))
    (file-name
     (git-file-name "lib-storage-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0b11ix95dzpkz335q0a6b5yg8qhj33s4fgj9ppl37pszcqq1j3wi"))))

(define lib-tl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_tl.git")
          (commit "237cbeb9d1c637759f89a508c1d854caf16e1984")))
    (file-name
     (git-file-name "lib-tl-for-telegram-desktop" %telegram-version))
    (patches
     (search-patches "lib-tl-for-telegram-memcpy.patch"))
    (sha256
     (base32
      "1ji3gypy4yf9knsgylnyz5gc2kii7lls5ymj1rkf0daixdz931cm"))))

(define lib-ui-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_ui.git")
          (commit "ba969667301ae4d8da2c2f6c4528bea63443f607")))
    (file-name
     (git-file-name "lib-ui-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "17h6awwna9qn98a0zk85xhh8ibgh3g7665khpgd752pya4jg27jw"))))

(define lib-webrtc-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webrtc.git")
          (commit "169ba6b1d5e58e9d1cfa7b7d5c85c119e6c6e2db")))
    (file-name
     (git-file-name "lib-webrtc-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0xh24qdy82j9mricja4ahzrsw9bgiklqy2mc0r891cblmmm2d90j"))))

(define lib-webview-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webview.git")
          (commit "f546969919a5946d49a504f8159041fa5b55c3df")))
    (file-name
     (git-file-name "lib-webview-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "178mf6lvgj4y5lscb68pc0yn3jcn66g04zszj74hpya18zjbmavw"))))

(define tgcalls-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TelegramMessenger/tgcalls.git")
          (commit "9bf4065ea00cbed5e63cec348457ed13143459d0")))
    (file-name
     (git-file-name "tgcalls-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1p563a11w8jrid96xf03dg6j39ciz28n5f4r6g28lxhiphbqzfym"))))

(define-public rlottie-for-telegram-desktop
  (let ((commit "8c69fc20cf2e150db304311f1233a4b55a8892d7")
        (revision "678"))
    (hidden-package
     (package
       (inherit rlottie)
       (version
        (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/rlottie.git")
            (commit commit)))
          (file-name
           (git-file-name "rlottie-for-telegram-desktop" version))
          (sha256
           (base32 "14gwg3sn6xdx9ymnx5r0vfm4pk8dwk92s10a1wdvfbjpyrxll64i"))
          (modules '((guix build utils)))
          (snippet
           #~(begin
               (substitute* "meson.build"
                 (("werror=true") "werror=false"))))))))))

(define cld3-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/google/cld3.git")
         (commit "b48dc46512566f5a2d41118c8c1116c4f96dc661")))
   (file-name
    (git-file-name "cld3-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0ayrrhfdwrf4260h9fsirkhhfrcvc3qqnh6h9wj3ixij2lq0wwqb"))))

(define libprisma-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/desktop-app/libprisma")
         (commit "23b0d70f9709da9b38561d5706891a134d18df76")))
   (file-name
    (git-file-name "libprisma-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0fg4x4ikj7f3706bmfvkwq4smxc98qr3cgpm25w48n4ys6wfgadg"))))

(define-public telegram-desktop
  (package
    (name "telegram-desktop")
    (version %telegram-version)
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/telegramdesktop/tdesktop.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "16dfk36xfsizrxmxcid9kwj2dvxfp42382hqcan9rsrgjlqm6ymy"))
       (patches
        (search-patches
         ;; https://github.com/telegramdesktop/tdesktop/pull/24126
         "telegram-desktop-allow-disable-libtgvoip.patch"
         ;; Make it compatible with GCC 11.
         "telegram-desktop-qguiapp.patch"
         "telegram-desktop-hashmap-incomplete-value.patch"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        #~(begin
            (let ((keep
                   '(;; Not available in Guix.
                     "tgcalls" "cld3")))
              (with-directory-excursion "Telegram/ThirdParty"
                (for-each delete-file-recursively
                          (lset-difference string=?
                                           (scandir ".")
                                           (cons* "." ".." keep)))))))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                      ; No target
           #:imported-modules
           `(,@%qt-build-system-modules
             (guix build glib-or-gtk-build-system))
           #:modules
           '((guix build qt-build-system)
             ((guix build glib-or-gtk-build-system)
              #:prefix glib-or-gtk:)
             (guix build utils)
             (ice-9 match))
           #:configure-flags
           #~(list
              ;; Do not generate the debug symbols to reduce link time memory
              ;; requirements from 25 GiB to 1.3 GiB.  This also nearly halves
              ;; the build time.
              "-DCMAKE_BUILD_TYPE=Release"
              ;; Client applications must provide their own API-ID and API-HASH,
              ;; see also <https://core.telegram.org/api/obtaining_api_id>.
              ;; Here, we snarf the keys from the official Snaps, which are
              ;; also stored in <#$source/snap/snapcraft.yaml>.
              "-DTDESKTOP_API_ID=611335"
              "-DTDESKTOP_API_HASH=d524b414d21f4d37f08684c1df41ac9c"
              "-DTDESKTOP_DISABLE_LEGACY_TGVOIP=ON"
              "-DDESKTOP_APP_DISABLE_CRASH_REPORTS=ON"
              "-DDESKTOP_APP_DISABLE_AUTOUPDATE=ON"
              "-DDESKTOP_APP_USE_PACKAGED_RLOTTIE=ON"
              ;; Enabling jemalloc causes SIGSEGV.  This probably happened
              ;; after upgrading to glibc 2.39.
              "-DDESKTOP_APP_DISABLE_JEMALLOC=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-additional-sources
                 (lambda _
                   (for-each make-file-writable (find-files "."))
                   (for-each
                    (match-lambda
                      ((dst src)
                       (copy-recursively src dst)
                       (for-each make-file-writable (find-files dst))))
                    '(("cmake" #$cmake-helpers-for-telegram-desktop)
                      ("Telegram/codegen" #$codegen-for-telegram-desktop)
                      ("Telegram/lib_base" #$lib-base-for-telegram-desktop)
                      ("Telegram/lib_crl" #$lib-crl-for-telegram-desktop)
                      ("Telegram/lib_lottie" #$lib-lottie-for-telegram-desktop)
                      ("Telegram/lib_qr" #$lib-qr-for-telegram-desktop)
                      ("Telegram/lib_rpl" #$lib-rpl-for-telegram-desktop)
                      ("Telegram/lib_spellcheck" #$lib-spellcheck-for-telegram-desktop)
                      ("Telegram/lib_storage" #$lib-storage-for-telegram-desktop)
                      ("Telegram/lib_tl" #$lib-tl-for-telegram-desktop)
                      ("Telegram/lib_ui" #$lib-ui-for-telegram-desktop)
                      ("Telegram/lib_webrtc" #$lib-webrtc-for-telegram-desktop)
                      ("Telegram/lib_webview" #$lib-webview-for-telegram-desktop)
                      ("Telegram/ThirdParty/cld3" #$cld3-for-telegram-desktop)
                      ("Telegram/ThirdParty/libprisma" #$libprisma-for-telegram-desktop)
                      ("Telegram/ThirdParty/tgcalls" #$tgcalls-for-telegram-desktop)))))
               (add-after 'unpack-additional-sources 'patch-gir-ignore-paths
                 (lambda _
                   (substitute* "cmake/external/glib/generate_cppgir.cmake"
                     (("\\$\\{cmake_helpers_loc\\}/external/glib/cppgir/data")
                      (string-append #$(this-package-input "cppgir") "/share/cppgir")))))
               (add-after 'unpack-additional-sources 'use-system-xdg-desktop-portal
                 (lambda _
                   (substitute* (list "Telegram/CMakeLists.txt"
                                      "Telegram/lib_base/CMakeLists.txt")
                     (("\\$\\{third_party_loc\\}/xdg-desktop-portal/data")
                      (string-append #$(this-package-native-input "xdg-desktop-portal")
                                     "/share/dbus-1/interfaces")))))
               ;; Remove a problematic 'constexpr' keyword, otherwise
               ;; compilation fails with GCC 11.
               (add-after 'use-system-xdg-desktop-portal 'patch-libwebview
                 (lambda _
                   (substitute* "Telegram/lib_webview/webview/webview_interface.h"
                     (("constexpr ") ""))))
               (add-after 'install 'glib-or-gtk-compile-schemas
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
               (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list cpp-ada-url-parser
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           pkg-config
           python-wrapper
           xdg-desktop-portal))
    (inputs
     (list abseil-cpp-cxxstd17
           alsa-lib
           boost
           c++-gsl
           cppgir-for-telegram-desktop
           crc32c
           expected-lite
           fcitx-qt5
           fcitx5-qt
           ffmpeg
           glib
           glibmm-2.76
           gtk+
           hime
           hunspell
           kcoreaddons-5
           kimageformats-5
           libdispatch
           libexpected
           libjpeg-turbo
           libvpx
           libxcb
           lz4
           minizip
           nimf
           openal
           openssl
           opus
           plasma-wayland-protocols
           pulseaudio
           protobuf
           qrcodegen-cpp
           qtbase-5
           qtdeclarative-5
           qtimageformats-5
           qtsvg-5
           qtwayland-5
           range-v3
           rlottie-for-telegram-desktop
           rnnoise
           wayland
           wayland-protocols
           webkitgtk-for-gtk3
           webrtc-for-telegram-desktop
           xcb-util-keysyms
           xxhash
           zlib))
    (synopsis "Telegram Desktop")
    (description "Telegram desktop is the official desktop version of the
Telegram instant messenger.")
    (home-page "https://desktop.telegram.org/")
    (license
     (list
      ;; ThirdParty
      license:lgpl3
      ;; Others
      license:gpl3+))))
