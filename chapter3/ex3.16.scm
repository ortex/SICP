(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define l3 (cons 'a (cons 'b (cons 'c '()))))

(count-pairs l3)

(define tmp (cons 'a '()))
(define l4 (cons tmp (cons 'b tmp)))

(count-pairs l4)

(define tmp2 (cons tmp tmp))
(define l7 (cons tmp2 tmp2))

(count-pairs l7)
