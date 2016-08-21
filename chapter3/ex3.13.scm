(load "chapter3/ex3.12.scm")

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle '('a 'b 'c)))

(last-pair z) ; infinity loop
