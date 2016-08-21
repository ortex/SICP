(load "chapter3/ex3.12.scm")

(define (count-pairs x)
  (define (iter x pairs)
    (if (not (pair? x))
        0
        (if (memq x pairs)
             0
             (let ((new-pairs (append pairs x)))
               (+ 1
                  (iter (car x) new-pairs)
                  (iter (cdr x) pairs))))))
  (iter x '()))


(define l3 (cons 'a (cons 'b (cons 'c '()))))
(count-pairs l3) ; 3

(define tmp (cons 'a '()))
(define l4 (cons tmp (cons 'b tmp)))
(count-pairs l4) ; 3

(define tmp2 (cons tmp tmp))
(define l7 (cons tmp2 tmp2))
(count-pairs l7) ; 3
