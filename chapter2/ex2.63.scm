(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

(define set7 (adjoin-set '7 (adjoin-set '6 (adjoin-set '5 (adjoin-set '4 (adjoin-set '3 (adjoin-set '2 (make-tree 1 '() '()))))))))

; 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-tree)
    (if (null? tree)
        result-tree
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-tree)))))
  (copy-to-list tree '()))


(tree->list-1 set7) ; (1 2 3 4 5 6 7)
(tree->list-2 set7) ; (1 2 3 4 5 6 7)
; a - the same


(define (random-tree n)
  (define (add-num i result)
    (if (< i n)
        (add-num (+ i 1) (adjoin-set (random 1000) result))
        result))
  (add-num 0 (make-tree 500 '() '())))

(define rt20 (random-tree 20))
(tree->list-1 rt20) ; tree->list-1 called 43 times
(tree->list-2 rt20) ; copy-to-list called 43 times

; b - the same
