(define tower '(scheme-number rational real complex))

(define (higher? t1 t2)
  (define (iter tower)
    (cond ((null? tower) (error "Illegal State"))
          ((eq? t1 (car tower)) false)
          ((eq? t2 (car tower)) true)
          (else (iter (cdr tower)))))
  (iter tower))

(define (get-highest-type types)
  (define (iter t types)
    (cond ((null? types) t)
          ((higher? (car types) t)
            (iter (car types) (cdr types)))
          (else (iter t (cdr types)))))
  (iter (car types) (cdr types)))

(define (raise-to-type arg type)
  (if (eq? type (type-tag arg))
      arg
      (raise-to-type ((get 'raise arg) arg) type)))

(define (raise-all-to-type args type)
  (if (null? args)
      '()
      (cons (raise-to-type (car args) type)
            (raise-all-to-type (cdr args) type))))

(define (all-equal? args arg)
  (if (null? args)
      false
      (if (eq? arg (car args))
          (if (null? (cdr args))
              true
              (all-equal (cdr ags) arg))
          false))

(define (apply-generic-with-raise op args types)
  (let ((highest-type (get-highest-type type-tags)))
    (if (all-equal? types highest-type)
        (error "Нет метода для этих типов" (list op args))
        (let ((coerced-args (raise-all-to-type args highest-type)))
          (apply apply-generic (op coerced-args))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc args)
          (apply-generic-with-raise op arg type-tags)))))
