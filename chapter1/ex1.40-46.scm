(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define tolerance 0.000001)
(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess))


(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c))) ; y(x) = x^3 + a*x^2 + b*x + c;

(newton-method (cubic 1 1 1) -1) ; y(x) = x^3 + x^2 + x + 1; y(-1) = 0;

; 1.41

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5) ; 21

; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43

(define (repeated f n)
  (if (= n 1)
    f
    (lambda (x) ((compose f (repeated f (- n 1))) x))))

((repeated square 2) 5)

; 1.44

(define (smooth f dx)
  (lambda (x)
    (/
      (+ (f (- x dx))
         (f x)
         (f (+ x dx)))
      3)))

(define (smooth-n f dx n)
  ((repeated (lambda (g) (smooth g dx)) n) f))

((smooth-n square dx 3) 2)

; 1.45

(define (log2 x)
  (/ (log x) (log 2)))

(define (sqrtn x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (log2 n))
                            1.0))

(sqrtn 4294967296 32)

; 1.46

(define (iterative-improve good-enough? improve)
  (define (iter x)
    (if (good-enough? x)
      x
      (iter (improve x))))
  iter)


((iterative-improve (lambda (x) (= x 7)) inc) 1) ; just incrementation :)

(define (sqrt x)
  ((iterative-improve (lambda (g) (< (abs (- (square g) x)) tolerance))
                      (lambda (g) (average g (/ x g))))
    1.0))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (g) (< (abs (- g (f g))) tolerance))
                      (lambda (g) (f g)))
    first-guess))

(fixed-point (lambda (y) (average y (/ 4 y))) 1.0) ; 2.0000000929222947
