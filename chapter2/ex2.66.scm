(load "chapter2/ex2.63.scm")
(load "chapter2/ex2.64-65.scm")

(define (key record) (car record))
(define (body record) (cdr record))
(define (make-record key body) (cons key record))

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((= given-key (key (entry tree-of-records)))
          (entry tree-of-records))
        ((< given-key (key (entry tree-of-records)))
          (lookup given-key (left-branch tree-of-records)))
        ((> given-key (key (entry tree-of-records)))
          (lookup given-key (right-branch tree-of-records)))))