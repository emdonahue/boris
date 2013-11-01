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
  ([url uri?]
   [method symbol?]
   [header (listof (cons/c symbol? string?))]
   [data (listof (cons/c string? string?))])
  @{Contains all information required to fetch a document over HTTP})
 
 ; Request Constructors
 (proc-doc/names http/request
                 (->* (hypertext-browser? uri?)
                      (#:method symbol?)
                      http-request?)
                 ((browser url) ((method 'GET)))
                 @{Creates a request function from the current state of @racket[browser].})
 
 (proc-doc/names http/redirect
                 (-> hypertext-browser? uri? http-request?)
                 (browser url)
                 @{Creates a request function from the current state of @racket[browser].})
 
 (proc-doc/names http/click
                 (-> hypertext-browser? uri? http-request?)
                 (browser url)
                 @{Creates a request function from the current state of @racket[browser].})
 
 (proc-doc/names http/submit
                 (->* (hypertext-browser? uri?)
                      (dict?
                       #:method symbol?) 
                      http-request?)
                 ((browser url) ((data '()) (method 'POST)))
                 @{Creates a form submit function from the current state of @racket[browser]. If @racket[method] is @racket['GET], @racket[data] will be sent as @racket[uri-query].}))

(require racket/serialize
         racket/date
         net/uri-codec
         net/http-client
         racket/dict
         racket/match
         racket/port
         "base.rkt"
         "uri.rkt"
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
      (if (string=? (dict-ref (http-request-header request) 'Content-Type "") "application/x-www-form-urlencoded") 
          (alist->form-urlencoded (http-request-data request))
          #f)
      #f))

(define (browser-cookies browser)
  (dict-ref (dict-ref (browser-state browser) 'http) 'cookies))

(define (browser-user-agent browser)
  (dict-ref (dict-ref (browser-state browser) 'http) 'user-agent))

; Request Constructors

(define (http/request browser url/relative #:method [method 'GET] #:headers [headers '()] #:data [data #f])
  (let ([url (combine-uri (browser-url browser) url/relative)])
    (dbg #f (uri->string url))
    (dbg #f (browser-cookies browser))
    (http-request url
                  method 
                  (dbg #f 
                        (headers-Cookie-set 
                         (headers-set headers 'User-Agent (browser-user-agent browser))
                         (cookies-ref (browser-cookies browser) url)
                         ))
                  data)))

(define (http/click browser url)
  (http/request browser url 
                #:method 'GET
                #:headers (headers-set (http-request-header (browser-request browser)) 'Referer (uri->string (browser-url browser)))))

(define (http/redirect browser url)
  (http/request browser url 
                #:method 'GET ;(http-request-method (browser-request browser))
                #:headers `((Referer . ,(or (dict-ref (browser-head browser) 'Referer #f) (uri->string (browser-url browser))))))) ; (headers-set (http-request-header (browser-request browser)) 'Referer (url->string/raw (browser-url browser)))
                ;#:data (http-request-data (browser-request browser))))

(define (http/submit browser url [data #f] #:method [method 'POST])
  (dbg #f (http/request browser (if (equal? method 'GET) (uri->form-urlencoded (uri-query-params-set url data)) url) #:method method 
                                   #:headers (append `((Referer . ,(uri->string (browser-url browser))))
                                                     (if (equal? method 'POST) 
                                                         '((Content-Type . "application/x-www-form-urlencoded")) '()))
                                                         
                                   #:data data)))
  
; Request Client

(define (request->browser request browser) 
  (let* ([response (request->response request)]
         [cookies (dbg #f (headers-Set-Cookies 
                         (response-head response)
                         (request-url request) 
                         (browser-cookies browser)))]
         [state (browser-state browser)]
         [http-state (dict-ref state 'http)]
         
         [browser (browser:next browser request response (dict-set state 'http (dict-set http-state 'cookies cookies)))])
    (dbg #f state)
    (let ([redirect (headers-Location (response-head response))])
      (if redirect ((http/redirect browser redirect) browser) browser))))
       

(define (request->response request)
  (parameterize ([debug-mode #t])
  (let ([u (request-url request)])
    (dbg "url" (uri->string u))
    (let-values ([(status header port)
                  (http-sendrecv 
                   (dbg #f (uri-host u))
                   (dbg "Path" (or (uri-relative-ref u) "/"))
                   #:ssl? (string=? (uri-scheme u) "https")
                   #:port (dbg #f (or (uri-port u) (if (string-ci=? (uri-scheme u) "https") 443 80)))
                   #:method (dbg #f (http-request-method request))
                   #:headers (dbg #f (alist->headers (http-request-header request)))                   
                   #:data (dbg #f (http-request-data/encoded request)))])
      (response (dbg #f status) (dbg #f (headers->alist (dbg "RAW HEAD" header))) (port->string port) (current-date))))))

; Utilities

(define (resolve-url browser url)
  (combine-uri (browser-url browser) url))

; TESTS

(module+ test
  (require rackunit
           "../echo-server/main.rkt")
  
  (let ([submit (http/submit (make-hypertext-browser #:user-agent "hypertext-browser") (string->uri "http://foo.com?bar=baz") '((fuzz . "buzz ?")) #:method 'GET)])
    (check-equal? (http-request-header submit) '((User-Agent . "hypertext-browser") (Referer . "")))
    (check-equal? (uri->string (request-url submit)) "http://foo.com?fuzz=buzz+%3F"))
  
  (define (echo->body browser)
    (read (open-input-string (browser-body browser))))
  
  (define server (start-echo-server/detached 60415))
  
  (define browser0 (make-hypertext-browser))
  (define browser1 ((http/request browser0 (string->uri "http://localhost:60415")) browser0))
  (define browser2 ((http/request browser1 (string->uri "http://localhost:60415/head")) browser1))
  (define browser3 ((http/submit browser2 (string->uri "http://localhost:60415/head")) browser2))
  
  (check-equal? (dict-ref (echo->body browser1) 'uri) "/")
      
  (check-equal? (uri->string (browser-url browser2)) "http://localhost:60415/redirected")

  (check-equal? (dict-ref (dict-ref (echo->body browser3) 'headers) #"Cookie") #"send2all=v1")
  (check-equal? (length (browser-cookies browser3)) 2)
  
  (kill-thread server))
