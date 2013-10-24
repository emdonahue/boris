#lang at-exp racket/base

; Extends the hypertext browser with support for the http protocol.

; DOCUMENTATION

(require scribble/srcdoc
         racket/contract/base
         (for-doc racket/base
                  scribble/manual))

(provide
 
 (struct*-doc 
  http-request
  ([url url?]
   [method symbol?]
   [header (listof (cons/c symbol? string?))]
   [data (listof (cons/c string? string?))])
  @{Contains all information required to fetch a document over HTTP})
 
 ; Request Constructors
 (proc-doc/names http/request
                 (->* (hypertext-browser? string?)
                      (#:method symbol?)
                      http-request?)
                 ((browser url) ((method 'GET)))
                 @{Creates a request function from the current state of @racket[browser].})
 
 (proc-doc/names http/redirect
                 (-> hypertext-browser? string? http-request?)
                 (browser url)
                 @{Creates a request function from the current state of @racket[browser].})
 
 (proc-doc/names http/click
                 (-> hypertext-browser? string? http-request?)
                 (browser url)
                 @{Creates a request function from the current state of @racket[browser].})
 
 (proc-doc/names http/submit
                 (->* (hypertext-browser? string?)
                      (dict?)
                      http-request?)
                 ((browser url) ((data '())))
                 @{Creates a form submit function from the current state of @racket[browser].}))

(require racket/serialize
         racket/date
         net/url
         net/uri-codec
         net/http-client
         racket/dict
         racket/match
         racket/port
         "base.rkt"
         "url.rkt"
         "http/head.rkt"
         "http/cookies.rkt"
         "../utils/emd/emd.rkt")

; IMPLEMENTATION

; Datatypes

(serializable-struct http-request request (method header data) 
                     #:transparent
                     #:property prop:procedure
                     (lambda (self browser)
                       (request->browser self browser)))

(define (http-request-data/encoded request)
  (if (http-request-data request)
      (cond
        [(string=? (dict-ref (http-request-header request) 'Content-Type) "application/x-www-form-urlencoded") (alist->form-urlencoded (http-request-data request))])
      #f))

; Request Constructors

(define (http/request browser url/string #:method [method 'GET] #:headers [headers '()] #:data [data #f])
  (let ([url (resolve-url browser url/string)])
    (dbg #f (url->string/raw url))
    (dbg #f (browser-state browser))
    (http-request url
                  method 
                  (dbg #f 
                        (headers-Cookie-set 
                         headers 
                         (dbg #f (cookies-ref (browser-state browser) url)))
                        ) 
                  data)))

(define (http/click browser url)
  (http/request browser url 
                #:method 'GET
                #:headers (headers-set (http-request-header (browser-request browser)) 'Referer (url->string/raw (browser-url browser)))))

(define (http/redirect browser url)
  (http/request browser url 
                #:method 'GET ;(http-request-method (browser-request browser))
                #:headers `((Referer . ,(url->string/raw (browser-url browser)))))) ; (headers-set (http-request-header (browser-request browser)) 'Referer (url->string/raw (browser-url browser)))
                ;#:data (http-request-data (browser-request browser))))

(define (http/submit browser url [data #f])
  (dbg #f (http/request browser url #:method 'POST 
                                   #:headers `((Referer . ,(url->string/raw (browser-url browser)))
                                              (Content-Type . "application/x-www-form-urlencoded")) 
                                   #:data data)))
  
; Request Client

(define (request->browser request browser) 
  (let* ([response (request->response request)]
         [state (dbg #f (headers-Set-Cookies 
                         (response-head response)
                         (request-url request) 
                         (browser-state browser)))]
         [browser (browser:next browser request response state)])
    (dbg #f state)
    (match (dict-ref (response-head response) 'Location #f)
      [#f browser]
      [redirect ((http/redirect browser redirect) browser)] ; If we get a Location, run the redirect.
      )))

(define (request->response request)
  (let ([u (request-url request)])
    (dbg "url" (url->string/raw u))
    (let-values ([(status header port)
                  (http-sendrecv 
                   (dbg #f (url-host u))
                   (dbg #f (url->path&query&fragment u))
                   #:ssl? (string=? (url-scheme u) "https")
                   #:port (dbg #f (or (url-port u) (if (string=? (url-scheme u) "https") 443 80)))
                   #:method (dbg #f (http-request-method request))
                   #:headers (dbg "HEADER" (alist->headers (http-request-header request)))                   
                   #:data (dbg #f (http-request-data/encoded request)))])
      (response (dbg "STATUS" status) (dbg #f (headers->alist (dbg "RAW HEAD" header))) (port->string port) (current-date)))))

; Utilities

(define (resolve-url browser url)
  (combine-url/relative (browser-url browser) url))

; TESTS

(module+ test
  (require rackunit
           "../echo-server/main.rkt")
  
  (define (echo->body browser)
    (read (open-input-string (browser-body browser))))
  
  (define server (start-echo-server/detached 60415))
  
  (define browser0 (make-hypertext-browser))
  (define browser1 ((http/request browser0 "http://localhost:60415") browser0))           
  (define browser2 ((http/request browser1 "http://localhost:60415/head") browser1))
  (define browser3 ((http/submit browser2 "http://localhost:60415/head") browser2))
  
  (check-equal? (dict-ref (echo->body browser1) 'uri) "/")
      
  (check-equal? (url->string (browser-url browser2)) "http://localhost:60415/redirected")

  (check-equal? (dict-ref (dict-ref (echo->body browser3) 'headers) #"Cookie") #"send2all=v1")
  (check-equal? (length (browser-state browser3)) 2)
  
  (kill-thread server))
