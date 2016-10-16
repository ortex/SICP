(load "chapter3/streams.scm")

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))


(stream-car
  (stream-cdr
    (stream-cdr
      (stream-map
        +
        (stream-enumerate-interval 1 10)
        (stream-enumerate-interval 1 20)
        (stream-enumerate-interval 10 30))))) ;; 18 = 3 + 3 + 12

(display-stream
  (stream-map
    *
    (stream-enumerate-interval 1 10)
    (stream-enumerate-interval 1 20)
    (stream-enumerate-interval 10 30)))

;; 10
;; 44
;; 108
;; 208
;; 350
;; 540
;; 784
;; 1088
;; 1458
;; 1900
