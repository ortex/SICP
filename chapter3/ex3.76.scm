(load "chapter3/streams.scm")

(define (smooth stream)
  (cons-stream
    (/ (+ (stream-car stream) (stream-car (stream-cdr stream))) 2)
    (smooth (stream-cdr stream))))

(define (zero-crossing sense-data smooth)
  (let ((smoothed-sense-data (smooth sense-data)))
    (stream-map sign-change-detector
                smoothed-sense-data
                (cons-stream 0 smoothed-sense-data))))

(display-first-n (zero-crossing sense-data smooth) 20)
