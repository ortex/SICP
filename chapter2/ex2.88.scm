(define (change-sign x)
  (apply-generic 'change-sign x))

(define (change-sign-polynomial p)
  (make-polymonial (variable p)
                   (map change-sign (term-list p))))

(put 'change-sign '(scheme-number) (lambda (x) (make-scheme-number (- x))))
(put 'change-sign '(rational)
 (lambda (x) (make-rational (- (numer x)) (- (denom x)))))

(put 'change-sign '(complex)
 (lambda (x) (make-complex-from-real-imag (real-part x) (imag-part x))))

(put 'change-sign '(polynomial) change-sign-polynomial)

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (change-sign p2))
      (error "Not same variable of polys -- SUB-POLY"
             (list p1 p2))))

(put 'sub '(polynomial polynomial) sub-poly)
