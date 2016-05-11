(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

; 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; 2.9

(define (radius-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define i1 (make-interval 10 11))
(define i2 (make-interval 5 6))

(define i3 (make-interval 6 8))
(define i4 (make-interval 10 12))


(radius-interval (add-interval i1 i2)) ; 1
(radius-interval (add-interval i3 i4)) ; 2

(radius-interval (mul-interval i1 i2)) ; 8
(radius-interval (mul-interval i3 i4)) ; 18

; 2.10

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "second interval crosses zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define i1 (make-interval 1 2))
(define i2 (make-interval -1 2))

(radius-interval (div-interval i1 i1)) ; 0.75
(radius-interval (div-interval i1 i2)) ; error

; 2.11

(define (mul-interval x y)
    (let ((lx (lower-bound x))
          (ux (upper-bound x))
          (ly (lower-bound y))
          (uy (upper-bound y)))
      (cond ((>= lx 0)
             (cond ((>= ly 0)
                    (make-interval (* lx ly) (* ux uy)))
                   ((<= uy 0)
                    (make-interval (* ux ly) (* lx uy)))
                   (else
                    (make-interval (* ux ly) (* ux uy)))))
            ((<= ux 0)
             (cond ((>= ly 0)
                    (make-interval (* lx uy) (* ux ly)))
                   ((<= uy 0)
                    (make-interval (* ux uy) (* lx ly)))
                   (else
                    (make-interval (* lx uy) (* lx ly)))))
            (else
             (cond ((>= ly 0)
                    (make-interval (* lx uy) (* ux uy)))
                   ((<= uy 0)
                    (make-interval (* ux ly) (* lx ly)))
                   (else
                    (make-interval (min (* lx uy) (* ux ly))
                                   (max (* lx ly) (* ux uy)))))))))

; 2.12

(define (make-center-with c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))))

(define (width i)
  (/ (- (lower-bound i) (upper-bound i))))


(define (make-center-percent c p)
  (let ((w (* c (/ p 100.0))))
    (make-center-with c w)))

(define (percent i)
  (let ((l (lower-bound i))
        (u (upper-bound i)))
    (* (/ (- u l) (+ u l)) 100)))

; 2.13

(percent (mul-interval (make-center-percent 100 1) (make-center-percent 100 1))) ; 1.9998000199980004
(percent (mul-interval (make-center-percent 100 2) (make-center-percent 100 3))) ; 4.997001798920648
(percent (mul-interval (make-center-percent 200 5) (make-center-percent 100 3))) ; 7.98801797304044

; Pxy ~ Px + Py

; 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define r1 (make-center-percent 100 2))
(define r2 (make-center-percent 100 5))

(percent (par1 r1 r2)) ; 2.999200239928031
(percent (par2 r1 r2)) ; 1.000000000000007
