(define (pascal row col)
  (cond
    ((or (= col 1) (= col row)) 1)
    ((and (> col 1) (< col row))
      (+
        (pascal (- row 1) col)
        (pascal (- row 1) (- col 1))))))
