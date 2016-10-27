(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define zero-stream
  (cons-stream 0 zero-stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (sub-streams s1 s2)
  (stream-map - s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (show-first-n s n)
  (stream-for-each
    (lambda (x) (begin
      (display (stream-ref s x))
      (display " ")))
    (stream-enumerate-interval 0 n))
  'ok)
