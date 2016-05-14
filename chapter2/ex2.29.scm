(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

; a
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-lenght branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b
(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define mobile (make-mobile (make-branch 10 2)
                            (make-branch 10 1)))

(define mobile2 (make-mobile (make-branch 10 5)
                             (make-branch 5 mobile)))

(total-weight mobile) ; 3
(total-weight mobile2) ; 8

; c
(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (balanced-branch? left)
         (balanced-branch? right))))

(define (torque branch)
  (let ((length (branch-lenght branch))
        (struct (branch-structure branch)))
    (if (number? struct)
        (* length struct)
        (* length (total-weight struct)))))

(define (balanced-branch? branch)
  (if (number? (branch-structure branch))
      true
      (balanced? (branch-structure branch))))

(define simple-balance-mobile (make-mobile (make-branch 3 5)
                                           (make-branch 5 3)))
(define composite-balance-mobile (make-mobile (make-branch 1 16)
                                              (make-branch 2 simple-balance-mobile)))

(balanced? simple-balance-mobile)
(balanced? composite-balance-mobile)
(balanced? mobile)
(balanced? mobile2)

; d
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

; need to change only this:
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))
