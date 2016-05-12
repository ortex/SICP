; old:

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                    (- kinds-of-coins 1))
                (cc (- amount
                       (first-denomination kinds-of-coins))
                    kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100) ; 292

; 2.19

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define (no-more? coin-values) (null? coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (first-denomination coin-values) (car coin-values))

(cc 100 (list 50 25 10 5 1)) ; 292
(cc 100 (list 1 5 10 25 50)) ; 292

(cc 30 (list 100 50 25 10 5 2 1 0.5)) ; 985
(cc 30 (list 100 5 2 1 50 25 10 0.5)) ; 985
