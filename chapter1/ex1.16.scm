(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (define (expt-fast a count b)
    (if (= count 0)
      a
      (if (even? count)
        (expt-fast a (/ count 2) (* b b))
        (expt-fast (* a b) (- count 1) b))))
  (expt-fast 1 n b))
