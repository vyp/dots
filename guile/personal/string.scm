(define-module (personal string))

(define-public (first-word str)
  (let ((multiple-words? (string-index str #\space)))
    (if multiple-words?
        (substring str 0 multiple-words?)
        str)))

(define-public (first-words contents)
  (map first-word contents))

(define-public (substring-end str n)
  (substring str 0 (- (string-length str) n)))

(define-public (substring-from str n)
  (let ((len (string-length str)))
    (if (= len 0)
        ""
        (substring str (- len n) len))))

(define-public (substring-length str start end)
  (substring str start (- (string-length str) end)))
