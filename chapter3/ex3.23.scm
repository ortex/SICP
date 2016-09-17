(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (data item) (car item))
(define (prev item) (cadr item))
(define (next item) (cddr item))

(define (make-item data) (cons data (cons '() '())))

(define (set-prev! item prev) (set-car! (cdr item) prev))
(define (set-next! item next) (set-cdr! (cdr item) next))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-insert-deque! deque data)
  (let ((new-item (make-item data)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-item)
            (set-rear-ptr! deque new-item))
          (else
            (set-next! new-item (front-ptr deque))
            (set-prev! (front-ptr deque) new-item)
            (set-front-ptr! deque new-item)))))

(define (rear-insert-deque! deque data)
  (let ((new-item (make-item data)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-item)
            (set-rear-ptr! deque new-item))
          (else
            (set-prev! new-item (rear-ptr deque))
            (set-next! (rear-ptr deque) new-item)
            (set-rear-ptr! deque new-item)))))

(define (front-delete-deque! deque)
  (set-front-ptr! deque (next (front-ptr deque)))
  (if (not (null? (front-ptr deque)))
    (set-prev! (front-ptr deque) '())))

(define (rear-delete-deque! deque)
  (set-rear-ptr! deque (prev (rear-ptr deque)))
  (if (not (null? (rear-ptr deque)))
    (set-next! (rear-ptr deque) '())))

(define (print-item item)
  (cond ((null? item)
          (display ";"))
        (else
          (display (data item))
          (display " ")
          (print-item (next item)))))

(define (print-deque deque)
  (cond ((empty-deque? deque)
          (display "empty deque"))
        (else
          (print-item (front-ptr deque)))))

;;
(define d1 (make-deque))

(empty-deque? d1)
(data d1)
(front-ptr d1)
(rear-ptr d1)

(front-insert-deque! d1 'a)
(print-deque d1)
(front-insert-deque! d1 'b)
(print-deque d1)
(front-insert-deque! d1 'c)
(print-deque d1)
(rear-insert-deque! d1 'd)
(print-deque d1)
(front-delete-deque! d1)
(rear-delete-deque! d1)
