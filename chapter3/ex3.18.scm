(load "chapter3/ex3.13.scm")

(define (exist-infinity-loop? l)
  (define (iter l elems)
    (if (null? l)
        false
        (if (memq (car l) elems)
            true
            (iter (cdr l) (append elems (list (car l)))))))
  (iter l '()))

(exist-infinity-loop? '(1 2 23)) ; false

(define inf-loop (make-cycle '('a 'b 'c)))

(exist-infinity-loop? inf-loop) ; true
