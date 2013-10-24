#lang racket

(require 
  "syntax/state.rkt"
  "selectors.rkt"
  "syntax/navigation.rkt"
  "syntax/extraction.rkt"
  "syntax/binding.rkt"
  "syntax/control.rkt"
  "syntax/io.rkt")

(provide
 (all-from-out "selectors.rkt")
 (all-from-out "syntax/navigation.rkt")
 (all-from-out "syntax/extraction.rkt")
 (all-from-out "syntax/binding.rkt")
 (all-from-out "syntax/control.rkt")
 (all-from-out "syntax/io.rkt"))

;(define-syntax-rule (define-web name web)
;  (define name (make-web web)))
;
;(define-syntax make-web
;  (syntax-rules (go click submit extract extract/list let2 for print download)
;    [(_ (go urls web ...)) (boris/go urls (make-web web) ...)]
;    [(_ (click urls web ...)) (boris/click urls (make-web web) ...)]
;    [(_ (submit forms web ...)) (boris/submit forms (make-web web) ...)]
;    
;    [(_ (extract entity)) (boris/extract entity)]
;    [(_ (extract/list entities)) (boris/extract/list entities)]
;    
;    [(_ (let2 bindings web ...)) (boris/let/crawl bindings (make-web web) ...)]
;    [(_ (for bindings web ...)) (boris/let/for bindings (make-web web) ...)]
;    
;    [(_ (print value web ...)) (boris/show value (make-web web) ...)]
;    [(_ (download path)) (boris/download path)]
;    
;    [(_ other) other]))

