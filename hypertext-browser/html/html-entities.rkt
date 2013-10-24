#lang at-exp racket/base

; Provides facilities for decoding html entities.

; DOCUMENTATION

(require scribble/srcdoc
         racket/contract/base
         (for-doc racket/base
                  scribble/manual))

(provide 
 (proc-doc/names
  html-decode
  (-> string? string?)
  (html)
  @{Translates all HTML entity characters to their plaintext equivalents.})
 (proc-doc/names
  html-decode*
  (-> string? string?)
  (html)
  @{Runs @racket[html-decode] repeatedly until further repetition would not change the result.}))

; IMPLEMENTATION

(require racket/dict)

(define entity->string
  (dict-set* (make-immutable-hash)
             "&amp;" "&"
             "&#38;" "&"
             "&#x3a;" ":"
             "&#x2f;" "/"))

(define (html-decode html) 
  (regexp-replace* #rx"&[^;]*;"
                   html
                   (lambda (entity)
                     (dict-ref entity->string
                               entity
                               entity))))

(define (html-decode* html)
  (let ([decoded (html-decode html)])
    (if (string=? html decoded)
        html
        (html-decode* decoded))))

; TESTS

(module+ test
  (require rackunit)
  (check-equal? (html-decode "&#38;") "&")
  (check-equal? (html-decode* "&#38;#38;") "&"))

