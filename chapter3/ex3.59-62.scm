(load "chapter3/streams.scm")

;; 3.59a
(define (integrate-series s)
  (div-streams s integers))

;; 3.59b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(display-first-n cosine-series 10)
(display-first-n sine-series 10)

;; 3.60

(define (mul-series s1 s2)
  (cons-stream
     (* (stream-car s1) (stream-car s2))
     (add-streams
       (scale-stream (stream-cdr s2) (stream-car s1))
       (mul-series (stream-cdr s1) s2))))

(define s
  (add-streams (mul-series cosine-series cosine-series)
               (mul-series sine-series sine-series)))

(display-first-n s 10)

;; 3.61

(define (invert-unit-series s)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (invert-unit-series s))))

(define s (mul-series cosine-series (invert-unit-series cosine-series)))

(display-first-n s 10)

;; 3.62

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "cann't div on zero")
      (mul-series s1 (invert-unit-series s2))))

(define tangent-series
    (div-series sine-series cosine-series))

(display-first-n tangent-series 10)
