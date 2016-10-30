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

;; 325 (10 15) (6 17) (1 18)
;; 425 (13 16) (8 19) (5 20)
;; 650 (17 19) (11 23) (5 25)
;; 725 (14 23) (10 25) (7 26)
;; 845 (19 22) (13 26) (2 29)
;; 850 (15 25) (11 27) (3 29)
;; 925 (21 22) (14 27) (5 30)
;; 1025 (20 25) (8 31) (1 32)
;; 1105 (23 24) (12 31) (9 32)
;; 1250 (25 25) (17 31) (5 35)
;; 1300 (20 30) (12 34) (2 36)
