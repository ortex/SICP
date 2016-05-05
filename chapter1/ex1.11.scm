(define (func-recur n)
  (if (< n 3)
    n
    (+ 
      (func-recur (- n 1))
      (func-recur (- n 2))
      (func-recur (- n 3)))))

(define (func-iter n)
  (define (func-iter-local a b c count)
    (if (= count 0)
      c
      (func-iter-local (+ a b c) a b (- count 1))))
  (func-iter-local 2 1 0 n))
