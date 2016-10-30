(load "chapter3/ex3.70.scm")

(define (weight pair)
  (+ (expt (car pair) 3) (expt (cadr pair) 3)))

(define numbers-stream (weight-pairs integers integers weight))

(define (numbers-r stream)
  (if (= (weight (stream-car stream))
         (weight (stream-car (stream-cdr stream))))
      (cons-stream (stream-car stream) (numbers-r (stream-cdr stream)))
      (numbers-r (stream-cdr stream))))

(display-first-n-weighted (numbers-r numbers-stream) 10 weight)

;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312
;; 40033
;; 46683
;; 64232
;; 65728
;; 110656
