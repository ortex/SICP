(define (random-update x)
  (modulo (+ (* 3 x) 5) 19))

(define random-init (real-time-clock))

(define (rand)
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (random-update x))
                    x))
            ((eq? m 'reset)
             (lambda (val) (set! x val)))))))

(define r (rand))

(r 'generate)
(r 'generate)
(r 'generate)

((r 'reset) 10)
(r 'generate)
(r 'generate)
(r 'generate)

((r 'reset) 10)
(r 'generate)
(r 'generate)
(r 'generate)
