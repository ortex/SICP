(load "chapter3/streams.scm")

(define (show x)
  (display x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; 0
;; ;Value: x

(stream-ref x 5)
;; 12345
;; ;Value: 5

(stream-ref x 7)
;; 67
;; ;Value: 7
