(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


; 2.42
(define (queens boeard-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 broad-size)))
            (queen-cols (- k 1))))))
  (queen-cols boeard-size))


(define (append-n what n)
  (if (= n 0)
      (list what)
      (append (list what) (append-n what (- n 1)))))

;(define empty-board (append-n (enumerate-interval 1 8) 8))
(define empty-board (append-n (append-n 0 8) 8))

(define (get-n items n)
  (if (= n 1)
    (car items)
    (get-n (cdr items) (- n 1))))

(define (get board i j)
  (get-n (get-n board j) i))

(define (safe? k positions)
  (if (= k 1)
      true
      (...))

(define (get-index pred? items)
  (define (get-index-iter pred? items n)
    (if (null? items)
        '()
        (if (pred? (car items))
            n
            (get-index-iter pred? (cdr items) (+ n 1)))))
  (get-index-iter pred? items 1))


(define (is-queen? x) (= x 1))
