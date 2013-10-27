#lang at-exp racket/base

; Consumes a Boris web and actually performs the crawl.

; DOCUMENTATION

(require racket/contract/base
         scribble/srcdoc
         "semantics/state.rkt"
         (for-doc 
          racket/base          
          scribble/manual))

(provide 
 (proc-doc/names 
  spider
  (->* (web/c)
       (#:cache (or/c dict? #f))
       (listof any/c))
  ((web) ((cache #f)))
  @{"Spiders" a web, returning a list of all values extracted by the spider during its crawl. @racket[cache] is shared among all branches of the crawl, and allows the spider to avoid making network requests twice. If a persistent cache (such as @racket[fs-dict] is used, pages can be cached between crawls, allowing the spider to quickly redo a crawl up to a point of failure, or for a crawl to be modified and re-run entirely offline.})
 
 (proc-doc/names
  spider/generator
  (->* (web/c)
       (#:cache (or/c dict? #f))
       generator?)
  ((web) ((cache #f)))
  @{Similar to @racket[spider], but returns a generator that yields each extracted value as it is encountered, so results can be processed as they come in.}))

; IMPLEMENTATION

(require racket/promise
         racket/generator
         racket/dict
         racket/list
         racket/class
         "interpreter/browser-services.rkt"
         "../hypertext-browser/main.rkt"
         "../utils/emd/emd.rkt")




(define (spider/generator web #:cache [cache #f])
  (generator () (crawl 
                 (crawl-state (make-hypertext-browser) '() web)
                 (make-object browser-services% cache))))

(define (spider web #:cache [cache #f])
  (for/list ([fly (in-producer (spider/generator web #:cache cache) (void))]) fly))

; Run the crawl, threading new states into sub webs.
(define (crawl state services)
  (let ([web (crawl-state-web state)])
    (unless (empty? web) ; If our web is empty, prune this branch of the crawl.
      (for* ([next-state ((car web) state services)] ; Otherwise for each following statew
            [next-web (cdr (crawl-state-web (force next-state)))]) ; and each of that state's subwebs
          (crawl (struct-copy crawl-state (force next-state) [web next-web]) services))))) ; produce a new state and recurse.

; TESTS

(module+ test
  (require rackunit
           racket/runtime-path
           racket/file
           "../hypertext-browser/uri.rkt"
           "syntax.rkt")
  
  (define-runtime-path interpreter.rkt "interpreter.rkt")
  (define interpreter-url (uri->string (path->uri interpreter.rkt)))
  
  (define web (let/web ([a '(3 4)])
                    (go interpreter-url
                        (extract (body)))))
  
  (check-equal? (spider web #:cache (make-hash))
                (list (file->string interpreter.rkt)))
  
  
  )
