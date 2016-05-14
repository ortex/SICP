; 2.25

(cadr (caddr '(1 3 (5 7) 9)))

(caar '((7)))

(car (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 (7)))))))))))

; 2.26

(define x (list 1 2 3))
(define y (list 1 2 3))

(append x y)
(cons x y)
(list x y)
