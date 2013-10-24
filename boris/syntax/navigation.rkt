#lang racket/base

; Navigation forms request new documents from external sources.

; DOCUMENTATION

(provide go click submit)

; IMPLEMENTATION

(require "state.rkt"
         "../semantics.rkt"
         "../../hypertext-browser/main.rkt"
         "../../utils/emd/emd.rkt")

; The most basic navigation form, this dispatches to hypertext-browser's generic hypertex/get.
(define-syntax-rule (go url-or-urls subcrawls ...)
  (list (navigate 
         (lambda (document parameters)
           (parameterize
               ([current-document document]
                [current-parameters parameters])
             (for/list ([url (->list url-or-urls)])
               (hypertext/get document url)))))
        subcrawls ...))

; Click models a browser click action, assigning an appropriate Referer header.
(define-syntax-rule (click url-or-urls subcrawls ...)
  (list (navigate 
         (lambda (document parameters)
           (parameterize
               ([current-document document]
                [current-parameters parameters])
             (for/list ([url (->list url-or-urls)])
               (http/click document url)))))
        subcrawls ...))

; Submit acts like browser submission, posting data and setting the proper headers.
(define-syntax-rule (submit forms subcrawl ...)
  (list (navigate 
         (lambda (document parameters)
           (parameterize
               ([current-document document]
                [current-parameters parameters])
             (for/list ([form forms])
               (http/submit document (first form) (third form))))))
        subcrawl ...))

;(define-syntax-rule (fragment fragments subcrawls ...)
;  (list (lambda (state services)
;          (parameterize
;              ([current-document (crawl-state-browser state)]
;               [current-parameters (crawl-state-bindings state)])           
;           (for/list ([frag fragments])
;             (struct-copy crawl-state 
;                          state
;                          [browser (let ([browser (crawl-state-browser state)])
;                                      (browser:next browser (browser-request browser)
;                                                    (struct-copy response (browser-response browser) 
;                                                                 [body frag])))]))))
;        subcrawls ...))

; TESTS

(module+ test
  (require rackunit
           racket/class
           racket/generator
           racket/runtime-path
           net/url
           racket/promise
           racket/file
           "../interpreter/browser-services.rkt"
           "../semantics/state.rkt"
           "../../hypertext-browser/main.rkt")
  
  (define browser (make-hypertext-browser))
  (define state (crawl-state browser `((foo . "bar")) `(,(lambda (a b) 1))))
  (define services (make-object browser-services% (make-hash)))
  (define-runtime-path navigation.rkt "navigation.rkt")
  
  (check-equal? (browser-body (crawl-state-browser (force (car ((car (go (url->string (path->url navigation.rkt)))) state services))))) (file->string navigation.rkt))
  )