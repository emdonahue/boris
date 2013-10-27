#lang at-exp racket/base

; Provides the core semantic targets for all Boris syntax forms.

(require racket/contract/base
         scribble/srcdoc
         "semantics/state.rkt"
         "interpreter/services.rkt"
         "../hypertext-browser/base.rkt"
         "../utils/emd/emd.rkt"
         (for-doc racket/base
                  scribble/manual))

; DOCUMENTATION

(provide 
 (proc-doc/names 
  navigate
  (-> (-> hypertext-browser? dict? (listof request?)) semantic/c)
  (browsers-thunk)
  @{Navigate forms fetch external documents.})
 
 (proc-doc/names 
  extract
  (-> (-> hypertext-browser? dict? any/c) semantic/c)
  (extractor)
  @{All forms extracted during the crawl with any of Boris' extraction forms will be fed to the extract service via this semantic form.})
 
 (proc-doc/names
  bind 
  (-> (-> hypertext-browser? dict? web/c (listof (listof (cons/c symbol? any/c)))) semantic/c)
  (binding-pairs-thunk)
  @{@racket[binding-pairs-thunk] must return a list of alists. For each alist, each key is bound in the current crawl bindings and a new state is produced for each such set of bindings.})
 
 (proc-doc/names
  jump 
  (-> (-> dict? web/c web/c) semantic/c)
  (web-thunk)
  @{@racket[web-thunk] must return a web/c that will be used in place of the remainder of the web as instructions to the spider.}))

  (define semantic/c (-> crawl-state? (is-a?/c services<%>) (listof (or/c crawl-state? promise?))))

  ; IMPLEMENTATION
  
  (require racket/dict
         racket/list
         racket/promise
         racket/class
         racket/function)

; The procedure at the head of each tree may be a request, which fetches an external document and alters crawl-state-browser,
(define (navigate browsers-thunk)
  (lambda (state services)
    (map (lambda (req) (send services request state req))
         (browsers-thunk 
          (crawl-state-browser state)
          (crawl-state-bindings state)))))

; an extract, which sends arbitrary data parsed from the document & parameters to
; the extractor,
(define (extract extractor) 
  (lambda (state services)
    (send services extract
          (extractor (crawl-state-browser state)
                     (crawl-state-bindings state)))
    '()))

; a bind, which places an arbitrary value into the interpreter's list of
; bindings for use in later extraction and navigation,
(define (bind binding-pairs-thunk)
  (lambda (state services)
      (for/list ([binding-pairs (binding-pairs-thunk 
                         (crawl-state-browser state) 
                         (crawl-state-bindings state)
                         (crawl-state-web state))])
        (struct-copy  crawl-state state 
                      [bindings 
                       (apply dict-set* 
                              (cons (crawl-state-bindings state) 
                                    (append* (dict-map binding-pairs list))))]))))

; or a jump, which supplies a new crawl-state-subcrawl, effectively changing the
; remaining tree to be interpreted, allowing jumps, loops, recursion,
; etc.

(define (jump jumper)
  (lambda (state services)    
    (list (struct-copy 
           crawl-state state
           [web 
            (jumper (crawl-state-bindings state)
                    (crawl-state-web state))]))))

; TESTS

(module+ test
  (require rackunit
           racket/runtime-path           
           racket/file
           racket/generator
           "interpreter/browser-services.rkt"
           "../hypertext-browser/main.rkt"
           "../hypertext-browser/uri.rkt")
  (define services (make-object browser-services% (make-hash)))
  (define-runtime-path semantics.rkt "semantics.rkt")
  (define semantics-url (path->uri semantics.rkt))
  (define semantics-text (file->string semantics.rkt))
  (define semantics-request (file/request (make-hypertext-browser) 
                                 semantics-url))
  (define browser (semantics-request (make-hypertext-browser)))
  (define state (crawl-state browser `((foo . "bar") (baz . ,semantics-url)) `(,(lambda (a b) 1))))
  
  ; Navigate
  (define (browsers-thunk browser bindings)
    (list (file/request browser (dict-ref bindings 'baz))))
  
  (let ([nav-result (crawl-state-browser (force (car ((navigate browsers-thunk) state services))))])
    (check-equal? (browser-body nav-result) semantics-text)) 
  
  ; Extract
  (define (extractor browser bindings)
    (list (string-append (dict-ref bindings 'foo) (browser-body browser))))
  
  (check-equal? ((generator () ((extract extractor) state services))) (string-append "bar" semantics-text))
  
  ; Bind
  (define (binding-pairs-thunk browser bindings web)
    `(((a . 10) (b . ,(dict-ref bindings'foo)))))
  (check-equal? (dict-ref (crawl-state-bindings (car ((bind binding-pairs-thunk) state services))) 'b) "bar")
  
  ; Jump
  (define (jumper bindings web)
    (list (lambda (a b) (list state)) web))
  
  (let ([web (crawl-state-web (car ((jump jumper) state services)))])
    (check-equal? (length web) 2)
    (check-equal? ((car web) state services) (list state))))



