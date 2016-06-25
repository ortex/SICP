(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

;; ---

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (the-empty-termlist) (list -1 '()))

(define (first-term term-list) (make-term (car term-list) (caadr term-list)))
(define (rest-terms term-list) (list (- (car term-list) 1) (cdadr term-list)))
(define (empty-termlist? term-list) (= (car term-list) -1))

(define (add-first-coeff coeff term-list)
  (list (+ (car term-list) 1)
        (cons coeff (cadr term-list))))

(define (adjoin-term term term-list)
  (cond ((= (order term) (car term-list))
          (add-first-coeff (+ (coeff term)
                              (coeff (first-term term-list)))
                           (rest-terms term-list)))
        ((= (order term) (+ (car term-list) 1))
          (add-first-coeff (coeff term) term-list))
        ((> (order term) (car term-list))
          (adjoin-term term
                       (adjoin-term (make-term (- (order term) 1) 0)
                                    term-list)))
        (else (adjoin-term (first-term term-list)
                           (adjoin-term term
                                        (rest-terms term-list))))))

(define term (make-term 0 5))

(define terms (list 3 '(1 2 3 4)))

(adjoin-term term terms)
