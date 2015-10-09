(define-module (python-qutebrowser)
  #:use-module (gnu packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (guix licenses))

(define-public python-colorama
  (package
   (name "python-colorama")
   (version "0.3.3")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/c/colorama/colorama-"
           version ".tar.gz"))
     (sha256
      (base32
       "1716z9pq1r5ys3nkg7wdrb3h2f9rmd0zdxpxzmx3bgwgf6xg48gb"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)))
   (home-page "https://pypi.python.org/pypi/colorama")
   (synopsis "Cross-platform colored terminal text.")
   (description "Cross-platform colored terminal text.")
   (license bsd-3)))

(define-public python-colorlog
  (package
   (name "python-colorlog")
   (version "2.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/c/colorlog/colorlog-"
           version
           ".tar.gz"))
     (sha256
      (base32
       "1s8z9zr4r18igr4rri71nba01arnpppifrkaxhi2xb51500sw0qg"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)))
   (home-page
    "https://github.com/borntyping/python-colorlog")
   (synopsis "Log formatting with colors!")
   (description "Log formatting with colors!")
   (license expat)))

(define-public python-jinja2
  (package
   (name "python-jinja2")
   (version "2.8")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/J/Jinja2/Jinja2-"
           version ".tar.gz"))
     (sha256
      (base32
       "1x0v41lp5m1pjix3l46zx02b7lqp2hflgpnxwkywxynvi3zz47xw"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)
      ("python-markupsafe" ,python-markupsafe)))
   (home-page "http://jinja.pocoo.org/")
   (synopsis "Python template engine.")
   (description
    "A small but fast and easy to use stand-alone template engine written in
pure python.")
   (license bsd-3)))

(define-public python-pygments
  (package
   (name "python-pygments")
   (version "2.0.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/P/Pygments/Pygments-"
           version ".tar.gz"))
     (sha256
      (base32
       "0lagrwifsgn0s8bzqahpr87p7gd38xja8f06akscinp6hj89283k"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)))
   (home-page "http://pygments.org/")
   (synopsis "Syntax highlighting.")
   (description "Pygments is a syntax highlighting package written in Python.")
   (license bsd-3)))

(define-public python-pypeg2
  (package
   (name "python-pypeg2")
   (version "2.15.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/p/pyPEG2/pyPEG2-"
           version ".tar.gz"))
     (sha256
      (base32
       "0v8ziaam2r637v94ra4dbjw6jzxz99gs5x4i585kgag1v204yb9b"))))
   (build-system python-build-system)
   (inputs
    `(("python-setuptools" ,python-setuptools)))
   (arguments '(#:test-target "check"))
   (home-page "http://fdik.org/pyPEG2")
   (synopsis "An intrinsic PEG Parser-Interpreter for Python")
   (description "An intrinsic PEG Parser-Interpreter for Python")
   (license gpl2)))

(define-public python-qutebrowser
  (package
   (name "python-qutebrowser")
   (version "0.4.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://pypi.python.org/packages/source/q/qutebrowser/qutebrowser-"
           version ".tar.gz"))
     (sha256
      (base32
       "1mry18lndkjxvdkd44g9zq2q7nxs24j3sr7sisyhkza9ngj2443f"))))
   (build-system python-build-system)
   (inputs
    `(("python-colorama==0.3.3" ,python-colorama)
      ("python-colorlog==2.6.0" ,python-colorlog)
      ("python-jinja2==2.8.0" ,python-jinja2)
      ("python-markupsafe==0.23" ,python-markupsafe)
      ("python-pygments==2.0.2" ,python-pygments)
      ("python-pypeg2==2.15.1" ,python-pypeg2)
      ("python-pyyaml==3.11" ,python-pyyaml)
      ("python-setuptools" ,python-setuptools)))
    ;; `(("python-colorama==0.3.3" ,python-colorama==0.3.3)
    ;;   ("python-colorlog==2.6.0" ,python-colorlog==2.6.0)
    ;;   ("python-jinja2==2.8.0" ,python-jinja2==2.8.0)
    ;;   ("python-markupsafe==0.23" ,python-markupsafe==0.23)
    ;;   ("python-pygments==2.0.2" ,python-pygments==2.0.2)
    ;;   ("python-pypeg2==2.15.1" ,python-pypeg2==2.15.1)
    ;;   ("python-pyyaml==3.11" ,python-pyyaml==3.11)
    ;;   ("python-setuptools" ,python-setuptools)))
   ;; TODO: Figure out how to run tests.
   (arguments `(#:tests? #f))
   (home-page "http://www.qutebrowser.org/")
   (synopsis
    "A keyboard-driven, vim-like browser based on PyQt5 and QtWebKit.")
   (description
    "A keyboard-driven, vim-like browser based on PyQt5 and QtWebKit.")
   (license gpl3)))
