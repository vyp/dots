(define-module (personal scripting)
  #:use-module (personal string)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs sorting)
  #:use-module (srfi srfi-1))

(define-public (bell)
  (system* "echo" "-e" "\a"))

(define-public (close-ports ports)
  (map close-port ports))

(define-public (delete-files . files)
  (map (lambda (file)
         (when (file-exists? file)
           (delete-file file))) files))

(define-public (edit . files)
  (system (string-append (getenv "EDITOR") " " (string-join files))))

(define-public (mkdir-p . dirs)
  (map (lambda (dir)
         (unless (file-exists? dir)
           (mkdir dir))) dirs))

(define-public (enter-dir dir)
  (mkdir-p dir)
  (chdir dir))

(define-public (file-append file text)
  (let ((port (open-file file "a")))
    (display text port)
    (close-port port)))

(define-public (flocks mode . files)
  (let ((ports (map (lambda (file) (open-file file mode)) files)))
    (map (lambda (port) (flock port LOCK_EX)) ports)
    ports))

(define remove-stat-from-file-system-tree
  (match-lambda
    ((name stat)
     name)
    ((name stat children ...)
     (list name (map remove-stat-from-file-system-tree children)))))

(define-public (fs-tree path)
  (remove-stat-from-file-system-tree (file-system-tree path)))

(define-public (get-file ext)
  (let ((results
         (scandir "." (lambda (name)
                        (string-suffix? (string-append "." ext) name)))))
    (if (null? results)
        ""
        (car results))))

(define-public (home-path path)
  (string-append (getenv "HOME") "/" path))

(define-public (read-file file)
  (let ((lst '()))
    (with-input-from-file file
      (lambda ()
        (do ((line (read-line) (read-line)))
            ((eof-object? line))
          (set! lst (cons line lst)))
        (reverse lst)))))

(define-public (read-lines port)
  (let ((lst '()))
    (do ((line (read-line port) (read-line port)))
        ((eof-object? line))
      (set! lst (cons line lst)))
    (reverse lst)))

(define-public (sort-file file)
  (let ((contents (read-lines file)))
    (call-with-output-file file
      (lambda (out)
        (display (string-join
                  (list-sort string<? contents) "\n" 'suffix) out)))))

(define-public (system-output command)
  (let* ((port (open-input-pipe command))
         (output (read-delimited "" port)))
    (close-pipe port)
    (if (eof-object? output)
        ""
        output)))

(define-public (touch . files)
  (map (lambda (file)
         (unless (file-exists? file)
           (let ((port (open-file file "w")))
             (display "" port)
             (close-port port)))) files))
