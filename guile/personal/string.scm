(define-module (personal string))

(define-public (substring-end str n)
  (substring str 0 (- (string-length str) n)))

(define-public (substring-length str start end)
  (substring str start (- (string-length str) end)))
