#lang at-exp racket/base

(require scribble/srcdoc
         racket/contract/base
         racket/contract/base
         (for-doc racket/base
                  scribble/manual))

; DOCUMENTATION

(provide 
 (except-out (all-from-out "base.rkt") request response)
 (except-out (all-from-out "http.rkt") http-request)
 (except-out (all-from-out "file.rkt") file-request)
 (proc-doc/names
  hypertext/get 
  (-> hypertext-browser? uri? request?)
  (browser url)
  @{Returns as basic a fetch request as possible in the protocol specified in @racket[url]. For http://, this would be a GET request; for file://, it would be a simple read.}))

; IMPLEMENTATION

(require "base.rkt"
         "http.rkt"
         "file.rkt"
         "uri.rkt"
         "../utils/emd/emd.rkt"
         racket/match)

(define (hypertext/get browser u)
  (match (uri-scheme u)
    ["http" (http/request browser u)]
    ["https" (http/request browser u)]
    ["file" (file/request browser u)]
    [#f (hypertext/get browser (combine-uri (browser-url browser) u))])) ;TODO more thoroughly test relative url handling

; TESTS

(module+ test
  (require rackunit)
  (define browser (make-hypertext-browser))
  (define http-req (hypertext/get browser (string->uri "http://foo.com")))
  (check-pred http-request? http-req)
  (check-equal? (http-request-method http-req) 'GET))
    
