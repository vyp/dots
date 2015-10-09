(define-module (my-packages bspwm)
  #:use-module (my-packages sxhkd)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public bspwm
  (package
   (name "bspwm")
   (version "0.9")
   (source
    (origin
     (file-name (string-append name "-" version ".tar.gz"))
     (method url-fetch)
     (uri (string-append
           "https://github.com/baskerville/bspwm/archive/"
           version ".tar.gz"))
     (sha256
      (base32
       "1pig0h2jk8wipyz90j69c4bk37bfyq60asnn0v0bqld2p2vjvyqy"))))
   (build-system gnu-build-system)
   (inputs
    `(("libxcb" ,libxcb)
      ("libxinerama" ,libxinerama)
      ("sxhkd" ,sxhkd)
      ("xcb-util" ,xcb-util)
      ("xcb-util-keysyms" ,xcb-util-keysyms)
      ("xcb-util-wm" ,xcb-util-wm)))
   (arguments
    '(#:phases (alist-delete 'configure %standard-phases)
      #:tests? #f ;; No tests.
      #:make-flags
      (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))))
   (home-page "https://github.com/baskerville/bspwm")
   (synopsis "Tiling window manager based on binary space partitioning")
   (description "bspwm is a tiling window manager that represents windows as the
leaves of a full binary tree.")
   (license bsd-2)))
