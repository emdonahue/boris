#lang at-exp racket/base

; Provides a hypertext browser for requesting external resources such as files or web pages that automatically handles protocol details.

; DOCUMENTATION

(require scribble/srcdoc
         racket/contract/base
         racket/dict
         "uri.rkt"
         (for-doc scribble/manual
                  racket/base))

(provide
 
 ; Ddatatypes
 (struct*-doc hypertext-browser
              ([history (listof (list/c request? response? list?))])
              @{Represents the browsing history a browser after a sequence of requests have been made.})
 
 (proc-doc/names make-hypertext-browser
                 (-> hypertext-browser?)
                 ()
                 @{Initializes a new browser pointing to a blank page.})
 
 (struct*-doc response
              ([status any/c] ;TODO: parse the number out of the http header and replace this contract eg 404
               [head (listof (cons/c symbol? string?))]
               [body string?]               
               [timestamp date*?])
              @{Represents an external document containing @racket[body] fetched at @racket[timestamp] from @racket[url] that returned with @racket[status] and @racket[head].})
 
 (struct*-doc request
              ([url url?])
              @{Contains all information necessary to obtain the external document specified by @racket[url]. Meant to be subclassed by protocol-specific libraries.})
 
 ; Accessors
 (proc-doc/names browser-request
                 (-> hypertext-browser? request?)
                 (browser)
                 @{Returns the most recent request sent by the browser (corresponding to the browser's current page.})
 
 (proc-doc/names browser-url
                 (-> hypertext-browser? url?)
                 (browser)
                 @{Returns the url of the current page.})
 
 (proc-doc/names browser-response
                 (-> hypertext-browser? response?)
                 (browser)
                 @{Returns the @racket[response] corresponding to the browser's current page.})
 
 (proc-doc/names browser-head
                 (-> hypertext-browser? dict?)
                 (browser)
                 @{Returns the head of the browser's current page.})
 
 (proc-doc/names browser-body
                 (-> hypertext-browser? string?)
                 (browser)
                 @{Returns the body of the browser's current page.})
 
 (proc-doc/names browser:next
                 (-> hypertext-browser? request? response? list? hypertext-browser?)
                 (browser request response state)
                 @{Returns a browser that has the history of @racket[browser] but advanced one place to contain @racket[request] @racket[response] and @racket[state].})
 (contract-out
  [browser-history-verbose (-> hypertext-browser? void?)]
  [browser-state (-> hypertext-browser? list?)]))

(require racket/serialize
         racket/list
         racket/date)

; Datatypes

; A browser that maintains a history and current client-side state of a browse.
(serializable-struct hypertext-browser (history) #:transparent)

; A response from an external server to be stored in a browser's history.
(serializable-struct response (status head body timestamp) #:transparent)

; A request containing all information required to obtain a document from an external server.
(serializable-struct request (url) #:transparent)

(define (make-hypertext-browser)
  (hypertext-browser `((,(request (string->uri "")) ,(response "" '() "" (current-date)) ()))))

; Mutators

; Advances the current page of the browser by one request/response pair.
(define (browser:next browser req resp state)
  (struct-copy hypertext-browser browser 
               [history `((,req ,resp ,state) . ,(hypertext-browser-history browser))]))

; Accessors

(define (browser-request browser)
  (first (first (hypertext-browser-history browser))))

(define (browser-response browser)
  (second (first (hypertext-browser-history browser))))

(define (browser-url browser)
  (request-url (browser-request browser)))

(define (browser-head browser)
  (response-head (browser-response browser)))

(define (browser-body browser)
  (response-body (browser-response browser)))

(define (browser-state browser)
  (third (first (hypertext-browser-history browser))))


; DEBUG
;TODO invent some kind of "debug time" module
(define (browser-history-verbose browser)
  (newline)
  (display "BROWSER HISTORY:\n")
  (for ([req-resp (reverse (hypertext-browser-history browser))])
    (newline)
    (display (format "> ~a\n" (first req-resp)))
    (display (format "- ~a\n" (second req-resp)))
    (display (format "< ~a" (third req-resp)))))

; TESTS

(module+ test
  (require rackunit)
  (define browser1 (hypertext-browser '((1 2 3))))
  (check-equal? (browser-request browser1) 1)
  (check-equal? (browser-response browser1) 2)
  (check-equal? (browser-state browser1) 3)
  
  (define browser3 (browser:next browser1 4 5 6))
  (check-equal? (browser-request browser3) 4)
  (check-equal? (browser-response browser3) 5)
  (check-equal? (browser-state browser3) 6)
  
  (define browser2 (make-hypertext-browser))
  (check-equal? (url->string (browser-url browser2)) ""))


