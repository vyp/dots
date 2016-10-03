(define-module (site pipe)
  #:export (-> ->>))

;; Thread first macro.
;;
;; The first s-expression will be inserted as the first argument of the next
;; s-expesssion.
;;
;; Example: (-> 100 (/ 10) (/ 5)) becomes (/ (/ 100 10) 5)

(define-syntax ->
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x (f . (f-rest ...))) (f x f-rest ...))
    ((_ x f) (f x))
    ((_ x (f . (f-rest ...)) rest ...) (-> (f x f-rest ...) rest ...))
    ((_ x f rest ...) (-> (f x) rest ...))))

;; Thread last macro.
;;
;; The first s-expression will be inserted as the last argument of the next
;; s-expesssion.
;;
;; Example: (->> 100 (/ 10) (/ 5)) becomes (/ 5 (/ 10 100))

(define-syntax ->>
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x (f ...)) (f ... x))
    ((_ x f) `(f x))
    ((_ x (f ...) rest ...) (->> (f ... x) rest ...))
    ((_ x f rest ...) (->> (f x) rest ...))))
