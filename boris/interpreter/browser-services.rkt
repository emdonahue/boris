#lang at-exp racket/base

; Provides a services object with hypertext-browser-friendly services.

; DOCUMENTATION

(require racket/contract/base)

(provide 
 (contract-out
  [browser-services% (class/c
                      (init-field [cache (or/c dict? #f)])
                      (init-field [time-delay (or/c #f real? (-> real?))])
                      [request (->m crawl-state? request? promise?)]
                      [extract (->m (listof any/c) void?)])]))
 
; IMPLEMENTATION

(require racket/promise
         racket/generator
         racket/class
         racket/dict
         racket/serialize
         racket/list
         racket/match
         "services.rkt"
         "../semantics/state.rkt"
         "../../hypertext-browser/main.rkt"
         "../../hypertext-browser/uri.rkt"
         "../../utils/emd/emd.rkt")

; A basic services object that implements browser navigation, caching, and extracts to a generator.
(define browser-services% 
  (class* object%
    (services<%>)
    (super-new)
    (init-field (cache #f))
    (init-field (time-delay #f))
    
    (define/public (request state req)
      (delay (let ([cached-state (cache->state cache state req)])
               ; If we have a cached response, just return that.               
               (if cached-state
                   cached-state
                   ; Otherwise fetch a new document and
                   (let ([new-state (state->new-state state req)])
                     ; add it to the cache, if we have one.
                     (state->cache new-state cache req)
                     (sleep-for time-delay)
                     new-state)))))    
    
    ; Extract yields all extracted values to an enclosing generator.
    (define/public (extract extracted)
      (for ([e extracted])
        
        (when (and e (not (void? e))) (yield e))))))

; Look up a request in the cache.
(define (cache->state cache state req)
  (let ([cached-browser-state (and cache (dict-ref cache (serialize req) #f))])
    (if cached-browser-state (state->cached-state state (deserialize cached-browser-state)) #f)))

; Build a new state from a cached response.
(define (state->cached-state state cached-response)
  (struct-copy crawl-state state 
               [browser (hypertext-browser 
                         (cons cached-response 
                               (hypertext-browser-history 
                                (crawl-state-browser state))))]))

; Perform a request and build a new state from it.
(define (state->new-state state request)
  (struct-copy crawl-state state
               [browser (request (crawl-state-browser state))]))

; Insert the current page into the cache.
(define (state->cache state cache req)
  (when cache
    (dict-set! cache 
               (serialize req)
               (serialize (first (hypertext-browser-history (crawl-state-browser state)))))))

; Delay to reduce network strain.
(define (sleep-for time-delay)
  (cond 
    [(equal? time-delay #f) (void)]
    [(procedure? time-delay) (sleep (time-delay))]
    [(real? time-delay) (sleep time-delay)]
    ))

; TESTS

(module+ test
  (require rackunit           
           racket/runtime-path
           racket/file
           racket/date
           "../../hypertext-browser/base.rkt")
  
  (define foo-response (response 0 '() "foo" (current-date)))
  (define cache (make-hash))
                 ;`(("file:///foo" . ,foo-response))))
  (define services (make-object browser-services% cache .2))
  (define state (crawl-state (make-hypertext-browser) '() '()))
  
  (define-runtime-path services.rkt "services.rkt")
  
  (define services-url (path->uri services.rkt))
  
  (define services-request (file/request (crawl-state-browser state) services-url))
  
  (define new-browser (crawl-state-browser (force (send services request state services-request))))
  
  (check-equal? (browser-body new-browser) (file->string services.rkt))
  
  ; Test cache
  (check-equal? (dict-ref cache (serialize services-request)) (serialize (first (hypertext-browser-history new-browser))))

  
  ;(define foo-request (file/request (crawl-state-browser state) (string->uri "file:///foo")))
  ;(check-equal? (browser-body (crawl-state-browser (force (send services request state foo-request)))) "foo")
  )