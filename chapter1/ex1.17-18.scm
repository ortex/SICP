(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (* x y)
  (cond
    ((= y 0) 0)
    ((= y 1) x)
    ((even? y) (* (double x) (halve y)))
    (else (+ y (* x (- y 1))))))

(define (iter-* x y)
  (define (multiplication acum a b)
    (if (= b 0)
      acum
      (if (even? b)
        (multiplication acum (double a) (halve b))
        (multiplication (+ acum a) a (- b 1)))))
  (multiplication 0 x y))
