; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; 2.45
(define (split op1 op2)
  (define (split-inner painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-inner painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  split-inner)

; 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))

(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; 2.47

(define (make-frame origin e1 e2)
  (list origin e1 e2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame origin e1 e2)
  (cons origin (cons e1 e2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

; 2.48
(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

; 2.49
(define (make-segment x1 y1 x2 y2)
  (cons (cons x1 y1) (cons x2 y2)))

; a
(segments->painter
  (list (make-segment 0.0 0.0 0.0 1.0)
        (make-segment 0.0 1.0 1.0 1.0)
        (make-segment 1.0 1.0 1.0 0.0)
        (make-segment 1.0 0.0 0.0 0.0)))
; b
(segments->painter
  (list (make-segment 0.0 0.0 1.0 1.0)
        (make-segment 1.0 0.0 0.0 1.0)))
;c
(segments->painter
  (list (make-segment 0.0 0.5 0.5 1.0)
        (make-segment 0.5 1.0 1.0 0.5)
        (make-segment 1.0 0.5 0.5 0.0)
        (make-segment 0.5 0.0 0.0 0.5)))

; d - dull

; 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


(define (rotate180 painter)
 (transform-painter painter
                    (make-vect 1.0 1.0)
                    (make-vect 0.0 1.0)
                    (make-vect 1.0 0.0)))


(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
         (paint-bottom frame)
         (paint-top frame)))))

 (define (below painter1 painter2)
   (rotate-90 (beside (rotate-270 painter1)
                      (rotate-270 painter2))))
; 2.52

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))
