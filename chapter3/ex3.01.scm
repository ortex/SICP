(define (make-accumulator accum)
  (lambda (x)
    (begin (set! accum (+ accum x))
           accum)))

(define A (make-accumulator 5))

(A 10)
