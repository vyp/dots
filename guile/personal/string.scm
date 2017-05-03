(define-module (personal string))

(define-public (substring-end str n)
  (substring str 0 (- (string-length str) n)))

(define-public (substring-from str n)
  (let ((len (string-length str)))
    (if (= len 0)
        ""
        (substring str (- len n) len))))

(define-public (substring-length str start end)
  (substring str start (- (string-length str) end)))
