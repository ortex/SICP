(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "IllegalArgument -- choose-branch" bit))))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ; (a d a b b c a)

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((not (memq symbol (symbols tree)))
          (error "IllegalSymbol -- encode-symbol" symbol))
        ((leaf? tree) '())
        ((memq symbol (symbols (left-branch tree)))
          (cons '0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
          (cons '1 (encode-symbol symbol (right-branch tree))))))

(encode '(a d a b b c a) sample-tree) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; --
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge tree-set)
  (if (= (length tree-set) 1)
      (car tree-set)
      (successive-merge
        (adjoin-set (make-code-tree (car tree-set)
                                    (cadr tree-set))
                    (cddr tree-set)))))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

; 2.70
(define rock-alphabet
  '((A 2) (BOOM 1) (GET 2) (JOB 2)
    (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define rock-message
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))

(encode rock-message
        (generate-huffman-tree rock-alphabet))

; 2.71
для самого частого символа потербуется 1 бит
для самого редкого (n - 1) бит
