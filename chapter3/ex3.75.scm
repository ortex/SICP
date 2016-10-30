(load "chapter3/streams.scm")

(define (make-zero-crossing input-stream last-value last-avg-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg-value)
                 (make-zero-crossing (stream-cdr input-stream)
                                     (stream-car input-stream)
                                     avpt))))

(define sense-data (random-stream 10))

(display-first-n (make-zero-crossing sense-data 0 0) 10)
