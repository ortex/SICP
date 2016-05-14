(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))

(square-tree '(1 (2 (3 4) 5) (6 7)))
