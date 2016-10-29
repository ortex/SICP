(load "chapter3/streams.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (if (< (weight s1car) (weight s2car))
                (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
                (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (weight-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weight-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (display-first-n-weighted s n weight)
  (stream-for-each
   (lambda (x) (begin
     (display (stream-ref s x))
     (display " ")
     (display (weight (stream-ref s x)))
     (display "\n")))
   (stream-enumerate-interval 0 n))
  'ok)

(define (weight-a pair)
  (+ (car pair) (cadr pair)))

(define a (weight-pairs integers integers weight-a))
(display-first-n-weighted a 10 weight-a)

(define (weight-b pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define b (weight-pairs integers integers weight-b))
(display-first-n-weighted b 10 weight-b)
