(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
    (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral-stream predicate x1 x2 y1 y2)
  (cons-stream (predicate (random-in-range x1 x2)
                          (random-in-range y1 y2))
               (estimate-integral-stream predicate x1 x2 y1 y2)))

(define (estimate-integral predicate x1 x2 y1 y2)
  (stream-map (lambda (e) (* e
                             (* (- x2 x1) (- y2 y1))
                             1.0))
              (monte-carlo (estimate-integral-stream predicate x1 x2 y1 y2) 0 0)))

(define (pred x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      9))

(stream-ref (estimate-integral pred 2 8 4 10) 1000) ;; ~28

(stream-ref (estimate-integral (lambda (x y) (<= (+ (expt x 2) (expt y 2)) 1))
                               -1.0 1.0 -1.0 1.0)
            10000) ;; 3.15
