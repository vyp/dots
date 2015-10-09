(define-module (sxhkd)
  #:use-module (gnu packages asciidoc)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public sxhkd
  (package
   (name "sxhkd")
   (version "0.5.5")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://github.com/baskerville/sxhkd/archive/"
           version ".tar.gz"))
     (sha256
      (base32
       "04s3y2bq9502gw72jj3y2zsh96yj3qg2av3zsa8ahd2farvrysg6"))))
   (build-system gnu-build-system)
   (inputs
    `(("asciidoc" ,asciidoc)
      ("libxcb" ,libxcb)
      ("xcb-util" ,xcb-util)
      ("xcb-util-keysyms" ,xcb-util-keysyms)
      ("xcb-util-wm" ,xcb-util-wm)))
   (arguments
    '(#:phases (alist-delete 'configure %standard-phases)
      #:tests? #f
      #:make-flags
      (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))))
   (home-page "https://github.com/baskerville/sxhkd")
   (synopsis "Simple X hotkey daemon")
   (description "Simple X hotkey daemon.")
   (license bsd-2)))
