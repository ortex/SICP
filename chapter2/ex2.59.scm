(define (equals? x y)
  (cond ((and (null? x) (null? y)) true)
        ((and (number? x) (number? y)) (= x y))
        ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (pair? x) (pair? y))
            (and (equals? (car x) (car y))
                 (equals? (cdr x) (cdr y))))
        (else false)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equals? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-set (adjoin-set (car set2) set1)
                         (cdr set2)))))

(define set1 '(1 2 3 4))
(define set2 '(4 5 6 7))

(intersection-set set1 set2) ; (4)
(union-set set1 set2) ; (7 6 5 1 2 3 4)
