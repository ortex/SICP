(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (f dy y))
  y)


;; 3.78
  (solve-2nd (lambda (dy y) (add-streams (scale-stream dy a)
                                         (scale-stream ddy b)))
             dt
             y0
             dy0)
