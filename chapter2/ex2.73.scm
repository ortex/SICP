(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 2.73.a
; Происходит: программирование управляемое данными.
; Достаем по типу операции нужную процедуру и передаем в нее операнды и переменную по которой диффенецируемся
; number? и variable? нельзя включить в операцию выбора т.к. они не имею метки типа (знака операции)

; 2.73.b

(define (install-sum-and-product-package)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-product exp var)
    (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                           (multiplicand exp))))
  ; 2.73.c
  (define (make-exponent b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  (define (base e) (cadr e))
  (define (exponent e) (caddr e))
  (define (deriv-exponentiation exp var)
    (make-product
      (exponent exp)
      (make-product
       (make-exponent (base exp) (- (exponent exp) 1))
       (deriv (base exp) var))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponent))

; 2.84.d
; Надо будет изменить метод регистрации процедур:
; (put '+ deriv make-sum)
