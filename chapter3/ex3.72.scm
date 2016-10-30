(load "chapter3/ex3.70.scm")

(define (weight pair)
  (+ (expt (car pair) 2) (expt (cadr pair) 2)))

(define numbers-stream (weight-pairs integers integers weight))

(define (numbers stream)
  (let ((p1 (stream-car stream))
        (p2 (stream-car (stream-cdr stream)))
        (p3 (stream-car (stream-cdr (stream-cdr stream)))))
    (if (= (weight p1) (weight p2) (weight p3))
        (begin
          (display (weight p1)) (display " ")
          (display p1) (display " ")
          (display p2) (display " ")
          (display p3) (display "\n")
          (cons-stream (weight p1)
                       (numbers (stream-cdr (stream-cdr (stream-cdr stream))))))
        (numbers (stream-cdr stream)))))

(stream-ref (numbers numbers-stream) 10)
