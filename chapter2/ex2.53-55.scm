; 2.53
(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

; 2.54

(define (equals? x y)
  (cond ((and (null? x) (null? y)) true)
        ((and (number? x) (number? y)) (= x y))
        ((and (symbol? x) (symbol? y)) (eq? x y))
        ((and (pair? x) (pair? y))
            (and (equals? (car x) (car y))
                 (equals? (cdr x) (cdr y))))
        (else false)))

(equals? '(this is a list) '(this (is a) list))

(equals? 1 1)

; 2.55

(car ''abracadabra)

''abracadabra -> (quote (qoute abracadabra))

(car (quote (qoute abracadabra))
