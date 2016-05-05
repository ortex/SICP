(define (average a b) (/ (+ a b) 2))
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
      (display guess)
      (newline)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess))

(fixed-point (lambda (y) (average y (/ 4 y))) 1.0)

; 1.35
; φ^2 = φ + 1
; φ = 1 + 1/φ

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; 1.618033813400125

; 1.36
(fixed-point (lambda (x) (* (log 1000) (log x))) 2.0) ; 19

(fixed-point (lambda (x) (average x (* (log 1000) (log x)))) 2.0) ; 43

; 1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(/ 1
   (cont-frac (lambda (i) 1.0)
              (lambda (i) 1.0)
              12))
; k = 11, φ = 1.6179775280898876
; k = 12, φ = 1.6180555555555558

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

; 1.38

(define (e k)
  (define (d i)
    (if (= (remainder (+ i 1) 3) 0)
        (* (/ (+ i 1) 3) 2)
        1.0))
  (+ 2 (cont-frac (lambda (i) 1.0)
                  d
                  k)))
(e 100)

; 1.39

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* i 2) 1.0))
             k))

(tan-cf (/ 3.14 4) 100) ; .9992039901050428 ~ 1.0
