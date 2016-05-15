(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;;

(define (cube x)
  (define (good-enough-cube? guess)
    (< (abs (- (* guess guess guess) x)) 0.001))
  (define (improve-cube guess)
    (/
      (+ (/ x (square guess)) (* 2 guess))
      3))
  (define (cube-iter guess)
    (if (good-enough-cube? guess)
      guess
      (cube-iter (improve-cube guess))))
  (cube-iter 1.0))
