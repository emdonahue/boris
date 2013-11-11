#lang racket

(require "../utils/emd/emd.rkt")

(define make-data-dictionary 
  (lambda args
    (define table (chunk args 3))
    (lambda (datum)
      (for/list ([column table])
        (list (first column)
              ((second column) datum)
              (third column))
      ))
    ))




(module+ test
  (require rackunit)
  (check-equal? ((make-data-dictionary
     1 first 3 4 second 6) (list 7 8 9))
                '((1 7 3) (4 8 6)))
  )

             