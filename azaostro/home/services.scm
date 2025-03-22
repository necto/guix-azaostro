(define-module (azaostro home services)
  #:use-module (ice-9 ftw)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:export (home-direct-symlink-service-type
            path-to-local))

(define (create-direct-symlink-plan originals-dir)
  (define (relative-path path)
    (string-drop path (string-length originals-dir)))
  (let ((links '())
        (dirs '()))
    (define (leaf path stat _)
      (let ((relative-path (relative-path path)))
        (format #t "Will symlink /guix/store/...~a -> ~a\n" relative-path path )
        (set! links (cons (cons path relative-path) links))))
    (define (down path stat _)
      (unless (string=? path originals-dir)
        (set! dirs (cons (relative-path path) dirs))))
    (file-system-fold
     (const #t) ;enter
     leaf
     down
     (const #t) ;up
     (const #t) ;skip
     (const #t) ;error
     #t         ;init
     originals-dir)
    (cons dirs links)))


(define-syntax path-to-local
  (syntax-rules ()
    ((path-to-local path)
     (string-append (current-source-directory) "/" path))))

(define (home-direct-symlink-files original)
  (let ((orig-dir-name (basename original)))
    (unless (string-prefix? "/" original)
      (error 'wront-path
             "home-direct-symlink-service-type: ORIGINAL must be"
             "an absolute path to a directory. Use (path-to-local "
             original ")"))
    (let ((type (stat:type (lstat original))))
      (unless (eq? 'directory type)
        (error 'wrong-file "home-direct-symlink-service-type: ORIGINAL must be a directory and not a" type)))
    (let* ((dirs-links (create-direct-symlink-plan original))
           (dirs (car dirs-links))
           (links (cdr dirs-links)))
      `((,orig-dir-name
         ,(computed-file
           orig-dir-name
           #~(begin
               (define (make-dirs dirs)
                 (for-each (lambda (dir)
                             (let ((full-dir (string-append #$output "/" dir)))
                               (format #t "Creating directory ~a\n" full-dir)
                               (mkdir full-dir)))
                           dirs))
               (define (make-symlinks links)
                 (for-each (lambda (link)
                             (let ((orig (car link))
                                   (linkpath (string-append #$output "/" (cdr link))))
                               (format #t "Symlinking ~a -> ~a\n" orig linkpath)
                               (symlink orig linkpath)))
                           links))
               (mkdir #$output)
               (make-dirs '#$dirs)
               (make-symlinks '#$links))))))))

;; TODO: make it extensible to enable multipe instances with "simple-service"
(define home-direct-symlink-service-type
  (service-type
   (name 'my-hello)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             home-direct-symlink-files)))
   (description "Escape guix-home strict reproducibility and create
a symlink in your .config directly to the source files rather than
to a /gnu/store snapshot of it.  Provide the name of the directory
with the files you want to symlink.")
   (default-value '())))
