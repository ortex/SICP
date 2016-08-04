(define (make-monitored func)
  (let ((count-call 0))
    (lambda (p)
      (cond ((eq? p 'how-many-calls?) count-call)
            ((eq? p 'reset-count) (set! count-call 0))
            (else (begin (set! count-call (+ count-call 1))
                         (func p)))))))

(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
(s 'reset-count)
