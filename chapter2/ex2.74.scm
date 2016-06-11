(define (get-record employed file)
  ((get 'get-record' (department-tag file) file employed)))

(define (get-salary employed file)
  ((get 'get-salary (department-tag file)) (get-record employed file)))

(define (find-employee-record employed files)
  (if (null? files)
      null
      (let ((record (get-record employed (car files))))
        (if (null? record)
            (find-employee-record employed (cdr files))
            record))))
