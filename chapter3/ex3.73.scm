(load "chapter3/streams.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (vs is v0)
    (cons-stream v0
                 (add-streams (scale-stream is R)
                              (integral (scale-stream is (/ 1 C)) v0 dt))))
  vs)

(define RC1 (RC 5 1 0.5))
(define s (RC1 ones 10))

(displayln-first-n s 5)

;; 10
;; 15
;; 15.5
;; 16.
;; 16.5
;; 17.
