(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

; 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (prime? n)
  (= n (smallest-divisor n)))

; 1.22
(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " - ")
  (display elapsed-time)
  true)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      false))

(define (timed-prime-test? n)
  (start-prime-test n (runtime)))

(define (search-for-primes number n)
  (if (> n 0)
      (if (timed-prime-test? number)
          (search-for-primes (+ number 1) (- n 1))
          (search-for-primes (+ number 1) n))))

(search-for-primes 100000000000 3)    ; avg 0.33 sec
(search-for-primes 1000000000000 3)   ; avg 1.05 sec
(search-for-primes 10000000000000 3)  ; avg 3.26 sec
(search-for-primes 100000000000000 3) ; avg 10.72 sec

; t2/t1 = 3.2
; t3/t2 = 3.04
; t4/t3 = 3.28

; (sqrt 10) = 3.16

; 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(search-for-primes 100000000000 3)    ; avg 0.21 sec
(search-for-primes 1000000000000 3)   ; avg 0.65 sec
(search-for-primes 10000000000000 3)  ; avg 2.12 sec
(search-for-primes 100000000000000 3) ; avg 6.79 sec

; 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10000)
      (report-prime n (- (runtime) start-time))
      false))

(list
   (timed-prime-test 1009)
   (timed-prime-test 1013)
   (timed-prime-test 1019)
   (timed-prime-test 10007)
   (timed-prime-test 10009)
   (timed-prime-test 10037)
   (timed-prime-test 100003)
   (timed-prime-test 100019)
   (timed-prime-test 100043)
   (timed-prime-test 1000003)
   (timed-prime-test 1000033)
   (timed-prime-test 1000037))

;1009 - .2
;1013 - .2
;1019 - .22
;10007 - .25
;10009 - .24
;10037 - .26
;100003 - .31
;100019 - .31
;100043 - .3
;1000003 - .34
;1000033 - .37
;1000037 - .35

; 1.27
(define (test-deception n)
  (define (test-deception-iter a)
    (if (< a n)
      (if (= (expmod a n n) a)
          (test-deception-iter (+ a 1))
          false)
      true))
  (test-deception-iter 1))

(test-deception 561)  ; deception
(test-deception 1105) ; deception
(test-deception 1729) ; deception
(test-deception 2465) ; deception
(test-deception 2821) ; deception
(test-deception 6601) ; deception

(test-deception 17) ; prime
(test-deception 18) ; not-prime

; 1.28
(define (check k n r)
  (if (and (not (= k 1))
           (not (= k (- n 1)))
           (= r 1))
      0
      r))

(define (remainder-or-check k m)
  (check k m (remainder (square k) m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder-or-check (expmod base (/ exp 2) m) m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (miller-rabin-iter a t)
    (define (try-it a)
      (= (expmod a (- n 1) n) 1))
    (if (= a n)
      (> t (/ n 2))
      (if (try-it a)
          (miller-rabin-iter (+ a 1) (+ t 1))
          (miller-rabin-iter (+ a 1) t))))
  (miller-rabin-iter 1 0))


(miller-rabin-test 121)
(miller-rabin-test 21)
