(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (cube x) (* x x x))
(define (even? n) (= (remainder n 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(integral cube 0 1 0.01)  ; .24998750000000042
(integral cube 0 1 0.001) ; .249999875000001

; 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (coef k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (g k) (* (coef k) (yk k)))
  (define (next x) (+ x 1))
  (* (/ h 3) (sum g 0 next n)))

(simpson cube 0 1 100)  ; 1/4
(simpson cube 0 1 1000) ; 1/4

; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi)
  (define (pi-term x)
    (/
      (* (* x 2) (+ (* x 2) 2))
      (square (+ (* x 2) 1))))
  (* 4.0 (product pi-term 1 inc 100)))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; 1.32
(define (accumulate combinet null-value term a next b)
  (if (> a b)
      null-value
      (combinet (term a)
                (accumulate combinet null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(sum identity 1 inc 10)
(product identity 1 inc 5)

(define (accumulate combinet null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combinet result (term a)))))
  (iter a null-value))

; 1.33
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate combinet predicate null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combinet result (if (predicate a)
                                            (term a)
                                            null-value)))))
  (iter a null-value))
  
; 1.33a
(define (sum-prime-numbers a b)
  (filtered-accumulate + prime? 0 square a inc b))

(sum-prime-numbers 1 10) ; 1 + 4 + 9 + 25 + 49 = 88

; 1.33b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-prime-for n)
  (define (predicate k)
    (= (gcd n k) 1))
  (filtered-accumulate * predicate 1 identity 1 inc n))

(product-prime-for 10) ; 1 * 3 * 7 * 9 = 189
