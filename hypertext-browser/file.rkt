#lang at-exp racket/base

(require scribble/srcdoc
         racket/contract/base
         (for-doc
          racket/base
          scribble/manual))

; DOCUMENTATION

(provide 
 (struct*-doc
  file-request
  ([url uri?])
  @{Contains all information necessary to retrieve a document from the filesystem.})
 (proc-doc/names
  file/request 
  (-> hypertext-browser? string? file-request?)
  (browser file-url)
  @{Creates a request that reads a file specified by @racket[file-url]. Must be in file:// form.}))

; IMPLEMENTATION

(require racket/file
         racket/serialize
         racket/date
         "uri.rkt"
         "base.rkt")

; Datatypes

(serializable-struct file-request request () 
                     #:transparent
                     #:property prop:procedure
                     (lambda (self browser)
                       (request->browser self browser)))

(define (file/request browser file-url)
  (file-request file-url))

; Fetch a file and package the results up as a browser.
(define (request->browser req browser)
  (browser:next browser req (request->response req) (browser-state browser)))

; Actually fetch the file contents.
(define (request->response req)
  (response "" '() (file->string (uri->path (request-url req))) (current-date)))

; TESTS

(module+ test
  (require rackunit
           racket/runtime-path)
  
  (define-runtime-path file.rkt "file.rkt")
  (define browser (make-hypertext-browser))
  (define file-request (file/request browser (path->uri file.rkt)))
  (define file-browser (file-request browser))
  (check-equal? (browser-body file-browser) (file->string file.rkt)))