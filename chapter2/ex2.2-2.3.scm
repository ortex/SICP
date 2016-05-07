(define (average . ns) (/ (apply + ns) (length ns)))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment end start) (cons end start))
(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
  (make-point
    (average (x-point start) (x-point end))
    (average (y-point start) (y-point end)))))

(define zero (make-point 0 0))
(define ten (make-point 10 10))

(define midpoint (midpoint-segment (make-segment zero ten)))

(print-point midpoint) ; (5,5)

; 2.3

; -------p2
; |      |
; p1------

(define (make-rectangle p1 p2) (cons p1 p2))

(define (h-rectangle r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))

(define (w-rectangle r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (perimeter r)
  (* 2 (+ (w-rectangle r) (h-rectangle r))))

(define (area r)
  (* (w-rectangle r) (h-rectangle r)))


; --------------
; |            |
; h            |
; |            |
; p1-----w------

(define (make-rectangle p h w) (cons p (cons h w)))

(define (h-rectangle r) (car (cdr r)))
(define (w-rectangle r) (cdr (cdr r)))
