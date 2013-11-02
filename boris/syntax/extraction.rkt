#lang racket/base

; Provides forms that extract values encountered during a crawl to an external source, such as the embedding program or a database.

; DOCUMENTATION 

(provide extract extract/list)

; IMPLEMENTATION

(require "state.rkt"  
         "error.rkt"
         (prefix-in sem: "../semantics.rkt")
         (for-syntax racket/base
                     syntax/parse))

; Extracts a single value to the external system.
(define-syntax-rule (extract extraction subcrawl ...)
  (list (sem:extract (lambda (browser bindings)
                       (parameterize ([current-document browser]
                                      [current-parameters bindings])
                         (handle-page-errors
                           (list extraction)))))))
  
; Extracts a sequence of values to the external system.
(define-syntax-rule (extract/list extraction subcrawl ...)
  (list (sem:extract (lambda (browser bindings)
                       (parameterize ([current-document browser]
                                      [current-parameters bindings])
                         extraction)))))

; TESTS

(module+ test
  (require rackunit
           racket/class
           racket/generator
           "../interpreter/browser-services.rkt"
           "../semantics/state.rkt"
           "../../hypertext-browser/main.rkt")
  
  (define browser (make-hypertext-browser))
  (define state (crawl-state browser `((foo . "bar")) `(,(lambda (a b) 1))))
  (define services (make-object browser-services% (make-hash)))
  
  (check-equal? ((generator () ((car (extract (current-document))) state services)
  )) browser)

(check-equal? ((generator () ((car (extract/list (list (current-document)))) state services)
  )) browser))