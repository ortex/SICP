(define (make-f)
  (let ((state 0))
    (define (switch x)
      (let ((old state))
        (set! state (+ x state))
        old))
    switch))

(define f (make-f))

(f 0) ; 0
(f 1) ; 0

(define f (make-f))

(f 1) ; 0
(f 0) ; 1
