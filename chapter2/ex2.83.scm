(define (raise x) (apply-generic 'raise x))

(define (scheme-number->rational x)
  (make-rational x 1))

(define (rational->real x)
  (make-real (/ (numer x) (denom x))))

(define (real->complex x)
  (make-complex-from-real-imag x 0))

(put 'raise 'scheme-number scheme-number->rational)
(put 'raise 'rational rational->real)
(put 'raise 'real real->complex)
