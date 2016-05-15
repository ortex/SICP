(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree '(1 (2 (3 4)) 5))

; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append '(1 2 3) '(4 5 6))

(define (lenght sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-term) (+ this-coeff (* x higher-term)))
              0
              coefficient-sequence))

(horner-eval 2 '(1 3 0 5 0 1)) ; y(x) = 1 + 3*x + 5*x^3 + x^5; y(2) = 79;

; 2.35
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
; variant #1
(define (count-leaves tree)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (x) (lenght (enumerate-tree x))) ; (1 (2 3 4) 5) -> (1 3 1)
                   tree)))

; variant #2

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1))
                   tree)))

(count-leaves '(1 (2 (3 4)) 5))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(dot-product '(1 2) '(2 1))
(matrix-*-vector '((1 2) (1 1)) '(2 2))

(define (transponse m)
  (accumulate-n cons '() m))

(transponse '((1 2) (3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transponse n)))
    (map (lambda (row) (matrix-*-vector cols row))
         m)))

(matrix-*-matrix '((1 2) (3 4)) '((5 6) (7 8))) ; ((19 22) (43 50))

; 2.38
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define (fold-right op init seq)
  (accumulate op init seq))

(fold-right / 1 '(1 2 3)) ; 3/2
(fold-left / 1 '(1 2 3)) ; 1/6

(fold-right list '() '(1 2 3)) ; (1 (2 (3 ()))))
(fold-left list '() '(1 2 3)) ; (((() 1) 2) 3)

; (a1 op (a2 op … (an op init) …)) = (… ((init op a1) op a2 ) … op an)
; if:
; 1) (op a b) = (op b a)
; 2) (op a (op b c)) = (op (op a b) c)

; 2.39
(define (reverse seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))

(define (reverse seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(reverse '(1 2 3))
