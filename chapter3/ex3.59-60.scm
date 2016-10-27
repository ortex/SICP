(load "chapter3/streams.scm")

(define (integrate-series s)
  (div-streams s integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 sine-series))

(define sine-series
  (cons-stream 0 (sub-streams zero-stream cosine-series)))

(show-first-n cosine-series 10)
(show-first-n sine-series 10)
