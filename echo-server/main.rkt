#lang at-exp racket/base

; Runs a simple HTTP echo server for testing and development.

; DOCUMENTATION

(require racket/contract/base
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual))

(provide
 (proc-doc/names
  start-echo-server 
  (->* () (natural-number/c) void?)
  (() ((port 8080)))
  @{Blocking call to start an echo server on @racket[port]. Since the call is blocking, this is for testing external services.})
 
 (proc-doc/names
  start-echo-server/detached 
  (->* () (natural-number/c) thread?)
  (() ((port 8080)))
@{Spawns an echo server in a separate thread. The call blocks until the server is running, so it can be used to test web services by the calling thread with no additional book-keeping.}))

; IMPLEMENTATION

(require web-server/servlet
         web-server/servlet-env
         web-server/http/request-structs
         racket/contract/base
         racket/port
         racket/list
         racket/dict
         racket/string
         "../hypertext-browser/url.rkt")

(define-values (echo-server-dispatch echo-server-url)
  (dispatch-rules
   [("head") echo-head]
   [("head2") echo-head2]
   [else echo-body]))

(define (start-echo-server [port 8080])
  (define-values (in out) (make-pipe))
  (let restart-server ()
    (parameterize ()
      (serve/servlet start
                     #:port port
                     #:servlet-regexp #rx""
                     #:launch-browser? #f))
    ;(sleep 1)
    #;(restart-server)))

(define (start-echo-server/detached [port 8080])
  (define-values (in out) (make-pipe))
  
  (let ([thd (thread
              (lambda ()
                (parameterize ([current-output-port out])
                  (start-echo-server port))))])
    (read in)
    thd))

(define (start req)
    (echo-server-dispatch req))

; Collect data from the request to echo back out in the response.
(define (echo-body req)
  (response/xexpr
     (with-output-to-string 
      (lambda () 
        (write 
         `((method . ,(request-method req)) 
           (uri . ,(url->string/raw (request-uri req))) 
           (headers . ,(req->headers req))
           (data . ,(request-post-data/raw req))))))))

; Collect data from the request for use in building custom response headers.
(define (echo-head req)
  (redirect-to 
   (string-append 
    "http://localhost:"
    (number->string (request-host-port req))
    "/redirected")
   #:headers
   (map cookie->header 
        (list (make-cookie "send2all" "v1")
              (make-cookie "send2none" "v2" #:path "/nosend")))))

;TODO this is only a temporary patch
(define (echo-head2 req)
  (redirect-to 
   (string-append 
    "http://localhost:"
    (number->string (request-host-port req))
    "/redirected")
   #:headers
   (map cookie->header 
        (list (make-cookie "send2foo" "v3" #:path "/foo")
              (make-cookie "send2none" "v4" #:path "/nosend")))))

; Convert the headers into an echo-able alist.
(define (req->headers req)
    (for/list ([header (request-headers/raw req)])
      (cons 
       (header-field header) 
       (header-value header))))


(define (url->string/raw u)
  (string-append (or (url-scheme u) "") "://" (or (url-host u) "") (url->path&query&fragment u)))                

; Returns everything following the host as a raw string. 
(define (url->path&query&fragment u) 
  (string-append
   (url-path/string u) ; path
   (if (empty? (url-query u)) "" (string-append* "?" (url-query/string u))) ; query
   (if (url-fragment u) (string-append "#" (url-fragment u)) ""))) ; fragment

(define (url-path/string u)
  (string-append
   "/" (string-join
        (map path/param-path (url-path u)) "/")))

(define (url-query/string u)
  (dict-map (url-query u) 
            (lambda (k v) 
              (string-append (symbol->string k) "=" (or v "")))))