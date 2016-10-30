(load "chapter3/streams.scm")

(define (sign-change-detector sense last-value)
  (cond ((and (< sense 0) (>= last-value 0))
          -1)
        ((and (> sense 0) (<= last-value 0))
          1)
        (else 0)))

(define (zero-crossing sense-data)
  (stream-map sign-change-detector
              sense-data
              (cons-stream 0 sense-data)))

(define sense-data (random-stream 10))
(display-first-n sense-data 20)
(display-first-n (zero-crossing sense-data) 20)

;; 6 3 -5 -8 -6 -4 -8 -7 -5 5 7 -8 -1 -6 -8 4 1 8 -1 -8 9
;; 1 0 -1  0  0  0  0  0  0 1 0 -1  0  0  0 1 0 0 -1  0 1
