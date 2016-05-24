(define (accumulate op initial itemsuence)
  (if (null? itemsuence)
      initial
      (op (car itemsuence)
          (accumulate op initial (cdr itemsuence)))))

; 2.57

(define (not-number? x) (not (number? x)))

(define (make-sum-list items)
  (if (null? (cdr items))
      (car items)
      (cons '+ items)))

(define (augend s) (make-sum-list (cddr s)))

(define (make-sum . items)
  (let ((vars (filter not-number? items))
        (const (accumulate + 0 (filter number? items))))
    (cond ((null? vars) const)
          ((= 0 const) (make-sum-list vars))
          (else (make-sum-list (cons const vars))))))

(define (make-product-list items)
  (if (null? (cdr items))
      (car items)
      (cons '* items)))

(define (multiplicand p) (make-product-list (cddr p)))

(define (make-product . items)
  (let ((vars (filter not-number? items))
        (const (accumulate * 1 (filter number? items))))
    (cond ((null? vars) const)
          ((= 0 const) 0)
          ((= 1 const) (make-product-list vars))
          (else (make-product-list (cons const vars))))))

(deriv '(* x y (+ x 3)) 'x) ; (+ (* x y) (* y (+ x 3)))
