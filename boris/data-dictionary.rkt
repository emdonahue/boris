#lang racket/base

; Provides utilities to construct data dictionaries from collected data.

(provide make-data-dictionary)

(require racket/list
         racket/match
         "../utils/emd/emd.rkt")

(define make-data-dictionary 
  (lambda args
    (define table (chunk args 3))
    (lambda (data)
      (for/list ([column table])
        (list (first column)
              (or (for/first ([datum data]
                              #:when (not-empty ((second column) datum)))
                    ((second column) datum)) "N/A")
              (third column))))))


(define/match (not-empty datum)
  [('()) #f]
  [((regexp #px"^\\s*$")) #f]
  [(_) datum])



(module+ test
  (require rackunit)
  (check-equal? 
   ((make-data-dictionary
     1 first "first" 
     2 second "second" 
     3 third "third"
     4 fourth "fourth") 
    '(("  " () 9 "") (10 11 12 "")))
   '((1 10 "first")
     (2 11 "second")
     (3 9 "third")
     (4 "N/A" "fourth")))
  )

             