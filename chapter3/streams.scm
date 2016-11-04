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

(define ones
  (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (sub-streams s1 s2)
  (stream-map - s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (display-first-n s n)
  (stream-for-each
    (lambda (x) (begin
      (display (stream-ref s x))
      (display " ")))
    (stream-enumerate-interval 0 n))
  'ok)

(define (displayln-first-n s n)
  (stream-for-each
    (lambda (x) (begin
      (display (stream-ref s x))
      (display "\n")))
    (stream-enumerate-interval 0 n))
  'ok)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (random-stream n)
  (cons-stream (- (random (* 2 n)) n) (random-stream n)))
    
(define (make-stream . args)
  (define (make-stream-from-list args)
    (if (null? args)
        '()
        (cons-stream (car args) (make-stream-from-list (cdr args)))))
  (make-stream-from-list args))
