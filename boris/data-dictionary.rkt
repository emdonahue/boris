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
  (define dd 
    (make-data-dictionary
     1 first 3 4 second 6))
  (dd (list 7 8 9))
  )

             