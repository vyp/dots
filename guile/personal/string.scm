(define-module (personal string))

(define-public (first-words contents)
  (map (lambda (line)
         (let ((multiple-words? (string-index line #\space)))
           (if multiple-words?
               (substring line 0 multiple-words?)
               line))) contents))

(define-public (substring-end str n)
  (substring str 0 (- (string-length str) n)))

(define-public (substring-from str n)
  (let ((len (string-length str)))
    (if (= len 0)
        ""
        (substring str (- len n) len))))

(define-public (substring-length str start end)
  (substring str start (- (string-length str) end)))
