(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car c)
  (help-fun c 2 0))

(define (cdr c)
  (help-fun c 3 0))

(define (help-fun x b n)
  (if (= (remainder x b) 0)
      (help-fun (/ x b) b (+ n 1))
      n))

(define z (cons 2 3))
(car z) ; 2
(cdr z) ; 3
