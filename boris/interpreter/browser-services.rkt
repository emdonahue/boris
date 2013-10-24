#lang at-exp racket/base

; Provides a services object with hypertext-browser-friendly services.

; DOCUMENTATION

(require racket/contract/base)

(provide 
 (contract-out
  [browser-services% (class/c
                      (init-field [cache (or/c dict? #f)])
                      [request (->m crawl-state? request? promise?)]
                      [extract (->m (listof any/c) void?)])]))
 
; IMPLEMENTATION

(require racket/promise
         racket/generator
         racket/class
         racket/dict
         "services.rkt"
         "../semantics/state.rkt"
         "../../hypertext-browser/main.rkt"
         "../../hypertext-browser/url.rkt"
         "../../utils/emd/emd.rkt")

; A basic services object that implements browser navigation, caching, and extracts to a generator.
(define browser-services% 
  (class* object%
    (services<%>)
    (super-new)
    (init-field (cache #f))
    
    (define/public (request state req)
      (delay (let ([cached-response (and cache (cache->response cache req))])
               ; If we have a cached response, just return that.
               (if cached-response
                   (state->cached-state state req cached-response)
                   ; Otherwise fetch a new document and
                   (let ([new-state (state->new-state state req)])
                     ; add it to the cache, if we have one.
                     (when cache
                       (dict-set! cache 
                                  (url->string/raw (request-url req)) 
                                  (browser-response (crawl-state-browser new-state))))
                     new-state)))))    
    
    ; Extract yields all extracted values to an enclosing generator.
    (define/public (extract extracted)
      (for ([e extracted])
        
        (when (and e (not (void? e))) (yield e))))))

; Look up a request in the cache.
(define (cache->response cache req)
  (and cache (dict-ref cache (url->string/raw (request-url req)) #f)))

; Build a new state from a cached response.
(define (state->cached-state state request cached-response)
  (struct-copy crawl-state state 
               [browser (browser:next 
                         (crawl-state-browser state)
                         request 
                         cached-response
                         (browser-state (crawl-state-browser state)))]))

; Perform a request and build a new state from it.
(define (state->new-state state request)
  (struct-copy crawl-state state
               [browser (request (crawl-state-browser state))]))

; TESTS

(module+ test
  (require rackunit
           net/url
           racket/runtime-path
           racket/file
           racket/date
           "../../hypertext-browser/base.rkt")
  
  (define foo-response (response 0 '() "foo" (current-date)))
  (define cache (make-hash `(("file:///foo" . ,foo-response))))
  (define services (make-object browser-services% cache))
  (define state (crawl-state (make-hypertext-browser) '() '()))
  
  (define-runtime-path services.rkt "services.rkt")
  
  (define services-url (url->string (path->url services.rkt)))
  
  (define services-request (file/request (crawl-state-browser state) services-url))
  
  (check-equal? (browser-body (crawl-state-browser (force (send services request state services-request)))) (file->string services.rkt))
  
  
  (define foo-request (file/request (crawl-state-browser state) "file:///foo"))
  (check-equal? (browser-body (crawl-state-browser (force (send services request state foo-request)))) "foo")
  )