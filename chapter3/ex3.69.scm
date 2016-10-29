(load "chapter3/streams.scm")

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

;; (1 1 1) + (1 1 z) + (1 y z) + (x y z)
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                 (stream-cdr u))
     (interleave
      (stream-map (lambda (pair) (cons (stream-car s) pair))
                  (pairs (stream-cdr t) (stream-cdr u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(displayln-first-n (triples integers integers integers) 100)

(define pythagorean-triples
  (stream-filter
    (lambda (triple)
      (= (+ (square (car triple))
            (square (cadr triple)))
          (square (caddr triple))))
    (triples integers integers integers)))

(displayln-first-n pythagorean-triples 100)
