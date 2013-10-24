#lang racket/base

; Binding forms store information from the current point in the crawl for use later on.

; DOCUMENTATION

(require
  "state.rkt"
  "../semantics.rkt"
  (for-syntax racket/base
              syntax/parse))

(provide for/web let/web)

; IMPLEMENTATION

(require racket/dict)

; Extract a variable from the binding store.
;(define-syntax (var stx)
;  (syntax-parse 
;   stx
;   [(_ key:id) #'(dict-ref (current-parameters) 'key)]
;   [(_ key:id failure-result:expr) #'(dict-ref (current-parameters) 'key failure-result)]))

; Repeats the body crawl for each set of bindings passed in.
(define-syntax (for/web stx)
  (syntax-case stx ()
    [(_ ([key val] ...) body0 body ...) 
     (with-syntax ([stx stx])
       #'(list 
          (derive-loop ((key val) ...) stx)
          body0 body ...))]))

; Dedicated singleton for.
(define-syntax (let/web stx)
  (syntax-case stx ()
    [(_ ([key val] ...) body0 body ...) 
     (with-syntax ([stx stx])
       #'(list 
          (derive-loop ((key (list val)) ...) stx)
          body0 body ...))]
    #;[(_ name ([key val] ...) body0 body ...)
     (with-syntax ([stx stx])
       #'(list 
          (derive-loop ((name (list 100)) (key (list val)) ...) stx)
          body0 body ...))]))

; Actually derive the binding/iteration forms.
(define-syntax (derive-loop stx)
  (syntax-case stx ()
    [(_ ([key val] ...) original)
     #'(bind ; Send to bind a
        (lambda (document parameters _) ; function that
          (parameterize 
              ([current-document document]
               [current-parameters parameters]) 
            (for/fold/derived ; builds a
             original 
             ([params '()]) ; list
             ((key val) ...) ; out of the provided key/value pairs.
             (append
              params
              (list (map cons (list 'key ...) (list key ...))))))))]))

;(define-syntax-rule 
;  (make-crawl-loop binding-pairs keys vals body ...)
;  (list (bind
;         (lambda (document parameters _)
;           (parameterize 
;               ([current-document document]
;                [current-parameters parameters])
;             (for/fold/derived 
;              stx
;              ([params '()])
;              binding-pairs
;              `(,(map cons keys vals) . ,params)))))
;        body ...)
;  )

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
  
  (check-equal? (dict-ref (crawl-state-bindings (car ((car (let/web ([a "baz"]) '())) state services))) 'a) "baz")
  
  (check-equal? (dict-ref (crawl-state-bindings (car ((car (for/web ([a '(3 4)]) '())) state services))) 'a) 3))