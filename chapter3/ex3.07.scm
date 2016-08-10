(load "chapter3/ex3.03.scm")

(define (make-joint account acc-password new-acc-password)
  (define (dispatch password m)
      (if (eq? password new-acc-password)
          (account acc-password m)
          (error "Incorrect password" password)))
  dispatch)


(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)

(define acc2 (make-joint acc 'secret-password 'secret2))
((acc2 'secret2 'withdraw) 10)
((acc 'secret-password 'withdraw) 10)
