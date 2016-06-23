(put '=zero? '(polynomial)
  (lambda (p) (empty-termlist? (term-list p))))
