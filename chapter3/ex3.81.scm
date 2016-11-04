(load "chapter3/streams.scm")

(define (random-update x)
  (modulo (+ (* 3 x) 5) 19))

(define random-init (real-time-clock))

(define (random commands)
  (define (rand commands random-init)
    (cond ((stream-null? commands) '())
          ((eq? (stream-car commands) 'generate)
            (cons-stream (random-update random-init)
                         (rand (stream-cdr commands)
                               (random-update random-init))))
          ((eq? (stream-car commands) 'reset)
            (cons-stream (stream-car (stream-cdr commands))
                         (rand (stream-cdr (stream-cdr commands))
                               (stream-car (stream-cdr commands)))))))
  (rand commands (real-time-clock)))


(define cmds (make-stream 'generate 'generate 'generate 'generate 'reset 10 'generate 'generate))
(displayln-first-n (random cmds) 6)

;; 3
;; 14
;; 9
;; 13
;; 10
;; 16
;; 15

(define cmds (make-stream 'generate 'reset 10 'generate 'generate 'generate 'generate 'generate))
(displayln-first-n (random cmds) 6)

;; 2
;; 10
;; 16
;; 15
;; 12
;; 3
;; 14
