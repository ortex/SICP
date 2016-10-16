(load "chapter3/streams.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams
                                    (integers-starting-from 2)
                                    factorials)))

(stream-ref factorials 0) ;; 1
(stream-ref factorials 1) ;; 2
(stream-ref factorials 2) ;; 6
(stream-ref factorials 3) ;; 24
