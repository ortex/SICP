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
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

(define set1 '(1 2 3 4))
(define set2 '(4 5 6 7))

(intersection-set set1 set2) ; (4)
(union-set set1 set2) ; (7 6 5 1 2 3 4)

; 2.60

; element-of-set? - without change

(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2) (append set1 set2))

(define (intersection-set set1 set2)
  (define (intersection-iter set result)
    (cond ((null? set) result)
          ((and (element-of-set? (car set) set2)
                (not (element-of-set? (car set) result)))
            (intersection-iter (cdr set) (cons (car set) result)))
          (else (intersection-iter (cdr set) result))))
  (intersection-iter set1 '()))

(define set1 '(1 2 3 4 4 4 4))
(define set2 '(4 5 6 7))

(intersection-set set1 set2) ; (4)

; old union-set O(n^2);        new union-set O(1)
; old intersection-set O(n^2); new intersection-set O(n^3)
