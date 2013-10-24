#lang racket/base

; Control forms alter the subsequent webs along this branch of the crawl.

(provide label recur)

(require
  "../semantics.rkt"
  racket/dict
  (for-syntax racket/base
              syntax/parse))


(define-syntax (label stx)
  (syntax-parse stx
                [(_ lbl:id body ...+)
                 #'(list (bind
                          (lambda (document parameters subcrawl)
                            `(((lbl . ,subcrawl)))))
                         body ...)]))

(define-syntax (recur stx)
  (syntax-parse stx
                [(_ lbl:id body ...)
                 #'(list (jump (lambda (parameters subcrawl)
                                     (dict-ref parameters 'lbl)
                                 ;subcrawl
                                 ))
                         body ...)]))