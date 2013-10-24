#lang racket/base

; IO forms provide "escape hatches" from the normal flow of a Boris crawl, and are mainly aimed at debugging and monitoring.

; DOCUMENTATION

(provide show download)

(require "../semantics.rkt"
         "../semantics/state.rkt"
         "state.rkt"
         "../../hypertext-browser/base.rkt")

; IMPLEMENTATION

; Prints any arbitrary racket value at the appropriate time during the crawl.
(define-syntax-rule (show msg body ...)
  (list (lambda (state services)    
          (parameterize
              ([current-document (crawl-state-browser state)]
               [current-parameters (crawl-state-bindings state)])
            (print msg)
            (newline))
          (list state))))

; Downloads the current document to the specified path.
(define-syntax-rule (download path)
  (list (lambda (state services)
          (parameterize
              ([current-document (crawl-state-browser state)]
               [current-parameters (crawl-state-bindings state)])
           (with-output-to-file path 
             (lambda () (display (browser-body (current-document))))
             #:mode 'text
             #:exists 'replace))
          '())))