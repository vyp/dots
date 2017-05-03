(define-module (personal string))

(define-public (substring-from str n)
  (let ((len (string-length str)))
    (if (= len 0)
        ""
        (substring str (- len n) len))))

(define-public (substring-to str n)
  (substring str 0 (- (string-length str) n)))

(define-public (substring-from-to str start end)
  (substring str start (- (string-length str) end)))
