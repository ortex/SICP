(define (possible-project-complex? z)
  (= (imag-part z) 0))

(define (project-complex z)
  (if (possible-project-complex z)
      (make-real (real-part z))
      (error "Impossible project" z)))

(define (possible-project-real r)
  (= (round r) r))

(define (project-real r)
  (if (possible-project-real r)
      (make-scheme-number (round r))
      (error "Impossible project" r)))

(define (possible-project-rational z)
  (= (denom z) 1))

(define (project-rational z)
  (if (possible-project-rational z)
      (make-scheme-number (numer z))
      (error "Impossible project" z)))

(define (possible-project-scheme-number n)
  false)

(put 'possible-project 'scheme-number possible-project-scheme-number)
(put 'possible-project 'rational possible-project-rational)
(put 'possible-project 'real possible-project-real)
(put 'possible-project 'complex possible-project-complex)

(put 'project 'rational project-rational)
(put 'project 'real project-real)
(put 'project 'complex project-complex)

(define (drop arg)
  (if ((get 'possible-project (type-tag arg)) arg)
      (drop ((get 'project (type-tag)) arg))
      arg))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc args))
          (apply-generic-with-raise op arg type-tags)))))
