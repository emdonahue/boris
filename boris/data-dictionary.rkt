#lang racket

(require "../utils/emd/emd.rkt")

(define make-data-dictionary 
  (lambda args
    (define table (chunk args 3))
    (lambda (data)
      (for/list ([column table])
        (list (first column)
              (or (for/first ([example (map (second column) data)] 
                         #:when (not-empty example)) example) "N/A")
              (third column))))))

(define/match (not-empty datum)
  [('()) #f]
  [((regexp #px"^\\s*$")) #f]
  [(_) datum])



(module+ test
  (require rackunit)
  ;(check-equal? 
   ((make-data-dictionary
     1 first "first" 
     2 second "second" 
     3 third "third"
     4 fourth "fourth") 
    '(("  " () 9 "") (10 11 12 "")))
   ;'((1 7 3) (4 8 6)))
  )

             