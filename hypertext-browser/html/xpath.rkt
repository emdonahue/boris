#lang at-exp racket/base

; Utilities for running and composing general xpath queries. 

(require scribble/srcdoc
         racket/contract/base
         (for-doc 
          racket/base          
          scribble/manual))

; DOCUMENTATION

(provide 
 (proc-doc/names 
  xpath
  (-> (or/c string? (listof string?)) string? (listof string?))
  (html xpath-query)
  @{Returns a list of strings representing the chunks of @racket[html] matching @racket[query].})
 
 (proc-doc/names 
  xpath/text
  (-> (or/c string? (listof string?)) string? (listof string?))
  (html xpath-query)
  @{Returns a list of strings representing the chunks of @racket[html] matching @racket[query] with "/text()" appended. This differs from @racket[xpath] in that @racket[xpath/text] will interpret empty nodes as containing "". @racket[xpath] will skip them.})
 
 (proc-doc/names 
  xpath/first
  (->* (string? string?) (any/c) any/c)
  ((html xpath-query) ((default #f)))
  @{Returns @racket[(first (xpath query html))] or @racket[default] if there are no matches.}))

; IMPLEMENTATION

(require racket/list
         racket/string
         (planet neil/html-parsing:2:0)
         (planet neil/html-writing:2:0)
         (planet clements/sxml2:1:3)
         "../../utils/emd/emd.rkt")

(define (xpath html query)
  (if (string? html) ; If given html,
      (map xexp->html ((sxpath query) (html->xexp html))) ; return the matches.      
      (append* (for/list ([sub-html html]) ; Otherwise, recurse on each html blob in the list and preserve the list.
                 (xpath sub-html query)))))

(define (xpath/text html query)
  (map (lambda (html) (string-trim (string-join (xpath html "/*/text()") ""))) ; Process each match (to preserve empty strings) 
       (xpath html query))) ; after querying for the nodes.

(define (xpath/first html query [default #f])
  (let ([nodes (xpath html query)])
    (if (empty? nodes) default (first nodes))))
        
; TESTS

(module+ test
  (require rackunit)
  (define test-html "<span><p><b>p1</b></p><p><b>p2</b></p><p><b>p3</b></p></span>")
  
  ; xpath
  (check-equal? (xpath test-html "//p") 
                '("<p><b>p1</b></p>" "<p><b>p2</b></p>" "<p><b>p3</b></p>"))
  
  (check-equal? (xpath test-html "//b") 
                '("<b>p1</b>" "<b>p2</b>" "<b>p3</b>"))
  
  (check-equal? (xpath (xpath test-html "//p") "//b")
                '("<b>p1</b>" "<b>p2</b>" "<b>p3</b>"))

(check-equal? (xpath (xpath "<p><b>b1</b><b>b2</b></p><p><b>pb3</b><b>b4</b></p>" "//p") "//b") '("<b>b1</b>" "<b>b2</b>" "<b>pb3</b>" "<b>b4</b>"))
  
  (check-equal? (xpath "<p></p><p></p>" "//p/text()") '())
  
  ; xpath/text
  (check-equal? (xpath/text "<p></p><p></p>" "//p") '("" ""))
  
  ; xpath/first
  (check-equal? (xpath/first test-html "//b/text()") "p1")
  
  (check-false (xpath/first test-html "/foo"))
  
  (check-equal? (xpath/first test-html "/foo" "bar") "bar"))
