; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; 2.45
(define (split op1 op2)
  (define (split-inner painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-inner painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  split-inner)
