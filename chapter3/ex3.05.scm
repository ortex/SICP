(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
            (/ trials-passed trials))
          ((experiment)
            (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimateintegral predicate x1 x2 y1 y2 trials)
  (define (experiment)
    (predicate (random-in-range x1 x2)
               (random-in-range y1 y2)))
  (* (monte-carlo trials experiment)
     (* (- x2 x1)
        (- y2 y1))
     1.0))


(define (pred x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      9))

(estimateintegral pred 2 8 4 10 10000) ; ~28 ; S = pi * r^2 = 3.14 * 9 = 28.26

(estimateintegral (lambda (x y) (<= (+ (expt x 2) (expt y 2)) 1))
                  -1.0 1.0 -1.0 1.0 10000) ; 3.1336
