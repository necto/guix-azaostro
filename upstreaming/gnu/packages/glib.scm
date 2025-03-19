(define-module (upstreaming gnu packages glib)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages glib))

;; Updated version
(define-public cppgir-for-telegram-desktop
  (let ((commit "2e96cab8ed40df326815b87b1e4b449e0c1a5947")
        (revision "0"))
    (package
      (inherit cppgir)
      (name "cppgir-for-telegram-desktop")
      (version (git-version "2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://gitlab.com/mnauw/cppgir")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0rdcgnriw8s5fqyx2v4218ii647l4fl1s9crnm9ihzf9bpl2p5p9")))))))
