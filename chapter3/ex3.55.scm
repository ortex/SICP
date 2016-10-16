(load "chapter3/streams.scm")

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
 (cons-stream (stream-car s)
              (add-streams (stream-cdr s) (partial-sums s))))

(define seq (partial-sums integers))

(stream-ref seq 0) ;; 1
(stream-ref seq 1) ;; 3
(stream-ref seq 2) ;; 6
(stream-ref seq 3) ;; 10
(stream-ref seq 4) ;; 15
