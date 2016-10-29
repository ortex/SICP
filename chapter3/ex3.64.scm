(define (sqrt-improve guess x)
  (/
    (+ guess (/ x guess))
    2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream))))
    (if (< (abs (- s1 s2)) tolerance)
        s2
        (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.1)         ;; 1.4166666666666665
(sqrt 2 0.000000001) ;; 1.414213562373095
