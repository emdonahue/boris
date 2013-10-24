#lang at-exp racket/base

; Utilities for extracting links from HTML documents.

; DOCUMENTATION

(require racket/contract/base
         scribble/srcdoc
         (for-doc
          racket/base
          scribble/manual))

(provide
 (proc-doc/names
  links
  (->* (string?) ((or/c regexp? string?)) (listof string?))
  ((html) ((url-regex #rx".")))
  @{Extracts all links from @racket[html] with a url matching @racket[url-regex]. @racket[url-regex] will be converted to a @racket[regex?] with @racket[regexp] if necessary.})
 
 (proc-doc/names
  links/text
  (-> string? (or/c regexp? string?) (listof string?))
  (html link-text-regex)
  @{Extracts all links from @racket[html] with link text matching @racket[link-text-regex]. @racket[url-regex] will be converted to a @racket[regex?] with @racket[regexp] if necessary.}))

; IMPLEMENTATION

(require racket/function
         racket/list
  "xpath.rkt"
         "../../utils/emd/emd.rkt")

(define (links html [rx||str #rx"."])
  (define rx (if (regexp? rx||str) rx||str (regexp rx||str)))
  (filter (curry regexp-match rx) (xpath html "//a/@href/text()")))
         
(define (links/text html rx||str)
  (define rx (if (regexp? rx||str) rx||str (regexp rx||str)))
   (filter-map 
    (lambda (link) ; For each link,
      (if (regexp-match rx (xpath/first link "/a/text()" "")) ; if the name matches,
          (xpath/first link "/a/@href/text()") #f)) ; grab the url.
    (xpath html "//a")))
         
; TESTS
         
(module+ test
  (require rackunit)
  (define html "<p><a href='foo'>bar</a></p><a href='fee'>baz</a>")
  
  ; links
  (check-equal? (links html) '("fee" "foo"))
  (check-equal? (links html #rx"fo") '("foo"))
  (check-equal? (links html "fe") '("fee"))
  
  ; links/name
  (check-equal? (links/text html #rx"bar") '("foo")))