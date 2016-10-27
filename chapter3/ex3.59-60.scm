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

(show-first-n cosine-series 10)
(show-first-n sine-series 10)

;; 3.60

(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s1))
    (add-streams
      (scale-stream (stream-cdr s1) (stream-car s2))
      (add-streams
        (scale-stream (stream-cdr s2) (stream-car s1))
        (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))))))

(define s
  (add-streams (mul-series cosine-series cosine-series)
               (mul-series sine-series sine-series)))

(show-first-n s 10)
