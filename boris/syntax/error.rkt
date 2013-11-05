#lang racket/base

(require scribble/srcdoc
         "../selectors.rkt"
         "../../hypertext-browser/uri.rkt"
         (for-doc racket-base
                  scribble/manual))

(provide handle-page-errors)

(define-syntax-rule (handle-page-errors stx)
  (with-handlers ([exn:fail? raise-page-error])
    stx))

(define (raise-page-error e)
  (eprintf "Error in the code handling the page: ~a\n\n" (uri->string (url)))
  (raise e))

