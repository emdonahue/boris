#lang racket/base

(provide handle-network-errors)

(define-syntax-rule (handle-network-errors stx)
  (with-handlers ([exn:fail? suppress-network-error])
    stx))

(define (suppress-network-error e)
  (eprintf "Network error:\n\n~a" (exn-message e)) #f)
