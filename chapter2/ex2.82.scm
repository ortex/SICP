(define (coercion arg type)
  (let ((arg-type (type-tag arg)))
    (if (eq? type arg-type)
        arg
        (let ((arg->type (get-coercion arg-type type)))
          (if arg->type
              (arg->type arg)
              false)))))

(define (coercion-all args type)
  (if (null? args)
      '()
      (let ((first (coercion (car args) type))
            (rest (coercion-all (cdr args) type)))
        (if (and first rest)
            (cons first rest)
            false))))

(define (apply-generic-with-coercion op args types)
  (if (null? types)
      (error "Нет метода для этих типов" (list op args))
      (let ((type-coercion (car types)))
        (let ((coerced-args (coercion-all args type)))
          (if coercion-args
              (apply apply-generic (op coercion-args))
              (apply-generic-with-coercion op args (cdr types)))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc args)
          (apply-generic-with-coercion op args type-tags)))))
