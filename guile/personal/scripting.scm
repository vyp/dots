(define-module (personal scripting)
  #:use-module (personal string)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs sorting)
  #:use-module (srfi srfi-1))

(define-public (append-to-file path text)
  (let ((file (open-file path "a")))
    (display text file)
    (close-output-port file)))

(define-public (bell)
  (system* "echo" "-e" "\a"))

(define-public (delete-files . files)
  (map (lambda (file)
         (when (file-exists? file)
           (delete-file file))) files))

(define-public (edit . files)
  (system (string-append (getenv "EDITOR") " " (string-join files))))

(define-public (mkdir-p dir)
  (unless (file-exists? dir)
    (mkdir dir)))

(define-public (enter-dir dir)
  (mkdir-p dir)
  (chdir dir))

(define (fold-line word acc)
  (let* ((len (string-length word))
         (next-position (+ len (cdr acc))))
    (if (> next-position 80)
        (cons (string-append (car acc) "\n" word) len)
        (cons (string-append (car acc)
                             (if (string=? (car acc) "") "" " ")
                             word)
              next-position))))

(define (fold-lines line acc)
  (let ((maxlen (- 80 (cdr acc))))
    (if (<= (string-length line) maxlen)
        (cons (string-append (car acc)
                             (if (string-index (substring-from (car acc) 1)
                                               #\newline) "" " ") line "\n") 0)
        (let ((folded-line (fold fold-line (cons "" 0)
                                 (string-split line #\space))))
          (cons (string-append (car acc) (car folded-line))
                (cdr folded-line))))))

(define-public (fold-file file)
  ;; NOTE: Currently only a failed attempt.
  (let ((contents (read-lines file)))
    (display (car (fold fold-lines (cons "" 0) contents)))))

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

(define-public (read-lines file)
  (let ((lst '()))
    (with-input-from-file file
      (lambda ()
        (do ((line (read-line) (read-line)))
            ((eof-object? line))
          (set! lst (cons line lst)))
        (reverse lst)))))

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
