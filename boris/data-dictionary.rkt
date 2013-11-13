#lang racket/base

; Provides utilities to construct data dictionaries from collected data.

(provide make-data-dictionary
         data-dictionary->html)

(require racket/list
         racket/match
         racket/date
         (planet neil/html-writing:2:0)
         "../utils/emd/emd.rkt")

(define make-data-dictionary 
  (lambda args
    (define table (chunk args 3))
    (lambda (data)
      (for/list ([column table])
        (list (first column)
              (second column)
              (or (for/first ([datum data]
                              #:when (not-empty ((third column) datum)))
                    ((third column) datum)) "N/A"))))))


(define/match (not-empty datum)
  [('()) #f]
  [((regexp #px"^\\s*$")) #f]
  [(_) datum])

(define (data-dictionary->html table title)
  (xexp->html 
   `(html
     (head
      (title ,(format "~a Data Dictionary" title))
      (style "thead{font-weight:bold;}"))
     (body 
      (table 
       (thead 
        (tr (td (@ (colspan "3")) ,(format "~a Data Dictionary" title)))
        (tr (td (@ (colspan "3")) ,(date->string (current-date))))
        (tr (td "Column") (td "Description") (td "Example")))
       (tbody
        ,(for/list ([tr table])
           `(tr 
             ,@(for/list ([td tr])
                 `(td ,td))))))))))

(module+ test
  (require rackunit)
  
  (define data-dictionary 
    ((make-data-dictionary
     "1" "first" first 
     "2" "second" second 
     "3" "third" third
     "4" "fourth" fourth)
    '(("  " () "9" "") ("10" "11" "12" ""))))
  
  (check-equal? 
   data-dictionary
   '(("1" "first" "10")
     ("2" "second" "11")
     ("3" "third" "9")
     ("4" "fourth" "N/A")))
  
  data-dictionary
  (data-dictionary->html data-dictionary "Test")
  )

             