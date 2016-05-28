(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(partial-tree '(1 3 5 7 9 11) 6)
;
; b - O(n)

; 2.65

(define (union-set-tree set1 set2)
  (list->tree (union-set (tree->list-1 set1)
                         (tree->list-1 set2))))

(define (intersection-set-tree set1 set2)
  (list->tree (intersection-set (tree->list-1 set1)
                                (tree->list-1 set2))))
