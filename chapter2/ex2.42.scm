(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; utils

(define (append-n what n)
  (if (= n 1)
      (list what)
      (append (list what) (append-n what (- n 1)))))

(define (get-n items n)
  (cond ((or (null? items) (< n 1)) '())
        ((= n 1) (car items))
        (else (get-n (cdr items) (- n 1)))))

(define (get-index pred? items)
  (define (get-index-iter pred? items n)
    (if (null? items)
        '()
        (if (pred? (car items))
            n
            (get-index-iter pred? (cdr items) (+ n 1)))))
  (get-index-iter pred? items 1))

(define (set-n items n val)
  (if (= n 1)
      (cons val (cdr items))
      (cons (car items) (set-n (cdr items) (- n 1) val))))

; 2.42
(define empty-col (append-n 0 8))
(define empty-board (append-n empty-col 8))
(define queen 1)

(define (is-queen? x) (and (not (null? x)) (= x queen)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? k positions)
  (if (= k 1)
      true
      (safe-iter? positions
                  (- k 1)
                  (get-index is-queen? (get-n positions k))
                  1)))

(define (safe-iter? positions i j shift)
  (if (= 0 i)
      true
      (let ((col (get-n positions i)))
        (and (not (is-queen? (get-n col j)))
             (not (is-queen? (get-n col (- j shift))))
             (not (is-queen? (get-n col (+ j shift))))
             (safe-iter? positions (- i 1) j (+ shift 1))))))

(define (adjoin-position new-row k rest-of-queens)
  (set-n rest-of-queens k (set-n empty-col new-row 1)))

; tests
(define safe-board
  '((0 0 1 0 0 0 0 0)
    (0 0 0 0 0 0 1 0)
    (0 1 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 1)
    (0 0 0 0 1 0 0 0)
    (1 0 0 0 0 0 0 0)
    (0 0 0 1 0 0 0 0)
    (0 0 0 0 0 1 0 0)))

(safe? 3 safe-board )

(get-index is-queen? (get-n safe-board 2))

(get-n safe-board 1)

(define (lenght sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(lenght (queens 8)) ; 92
