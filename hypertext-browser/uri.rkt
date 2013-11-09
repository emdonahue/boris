#lang at-exp racket/base

(require racket/contract/base
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual))

(provide 
 (struct*-doc
  uri
  ([scheme (or/c string? #f)]
   [userinfo (or/c string? #f)]
   [host (or/c string? #f)]
   [port (or/c #f positive?)]
   [path (or/c string? #f)]
   [query (or/c string? #f)]
   [fragment (or/c string? #f)])
  @{Represents a generic RFC 3986 uri.})
 
 (proc-doc/names
  uri-relative-ref 
  (-> uri? (or/c string? #f))
  (uri)
  @{Munges @racket[uri-path], @racket[uri-query], and @racket[uri-fragment] back into a string.})
 
 (proc-doc/names
  uri->form-urlencoded
  (-> uri? uri?)
  (uri)
  @{Returns @racket[uri] with @racket[uri-query] form-urlencoded.})
 
 (proc-doc/names
  uri-query-params
  (-> uri? (listof (cons/c symbol? string?)))
  (uri)
  @{Returns an alist of query parameters representing @racket[uri-query].})
 
 (proc-doc/names
  uri-query-params-set
  (-> uri? (listof (cons/c symbol? string?)) uri?)
  (uri params)
  @{Creates a new @racket[uri?] from @racket[uri] with @racket[params] as query parameters.})
 
 (proc-doc/names
  combine-uri
  (-> uri? uri? uri?)
  (base relative)
  @{Extends @racket[base] with @racket[relative] in the usual, RFC 3986 way.})
 
 (proc-doc/names
  string->uri 
  (-> (or/c string? uri?) uri?)
  (uri-string)
  @{Parses @racket[uri-string] into a @racket[uri]. If a @racket[uri?] is passed in, it is returned untouched.})
 
 (proc-doc/names
  uri->string 
  (-> uri? string?)
  (uri)
  @{Munges.@racket[uri] back into a string.})
 
 (proc-doc/names
  uri->path
  (-> uri? path?)
  (uri)
  @{Converts @racket[uri] into a system @racket[path?].})
 
 (proc-doc/names
  path->uri
  (-> path? uri?)
  (path)
  @{Converts a file @racket[path] to a file:// @racket[uri?].}))

; IMPLEMENTATION

 (require racket/serialize
          racket/match
          racket/list
          racket/string
          racket/dict
          net/uri-codec
          "html/html-entities.rkt"
          "../utils/emd/emd.rkt")

; DATATYPES ==============================

(define (pretty-print-uri u port mode)
  (match mode
    [#f (display (uri->string u) port)]
    [else (print (string-append "#<uri:" (uri->string u) ">") port)]))

(serializable-struct uri (scheme userinfo host port path query fragment) #:transparent
                     #:methods gen:custom-write
                     [(define write-proc pretty-print-uri)])


; PARAMETERS ================================

;TODO double check teh specs on this
(define (uri->form-urlencoded u) 
  (struct-copy uri u 
               [query (alist->form-urlencoded (uri-query-params u))]))

(define (uri-query-params u)
  (filter-map (lambda (pair)
                (match (regexp-match #px"([^=]+)=(.*)" pair)
                  [#f #f]
                  [(list _ name val) (cons (string->symbol name) val)]))
              (string-split (uri-query u) "&")))
    
(define (uri-query-params-set u params)
  (struct-copy uri u 
               [query (string-join 
                       (for/list ([param params])
                         (string-append (symbol->string (car param))
                                        "="
                                        (cdr param))) "&")]))

; RESOLUTION ==============================

;TODO this is not guaranteed to be to spec
(define (combine-uri base rel)
  (match rel
    [(uri scheme userinfo host port path query fragment)
     (if host (uri (or scheme (uri-scheme base))
                   userinfo host port path query fragment)
         (if path (struct-copy uri base
                               [path 
                                (path->string 
                                 (combine-path 
                                  (or (uri-path base) "") 
                                  (or path "")))]
                               [query query]
                               [fragment fragment])
             (struct-copy uri base 
                          [query (or query (uri-query base))]
                          [fragment fragment])))]))

; PARSING =================================

(define (uri->string u)
  (match u
    [(uri scheme userinfo host port path query fragment)
     (string-append 
      (if scheme (string-append scheme ":") "")
      (authority->string userinfo host port)
      (or (uri-relative-ref u) ""))]))

(define (uri-relative-ref u)
  (match u
    [(uri _ _ _ _ path query fragment)
     (and (or path query fragment)
          (string-append
           (or path "")
           (if query (string-append "?" query) "")
           (if fragment (string-append "#" fragment) "")))]))

(define (authority->string userinfo host port)
  (if (not host) ""
      (string-append
       "//"
       (if userinfo (string-append userinfo "@") "")
       host
       (if port (string-append ":" (number->string port)) ""))))

(define (string->uri s)
  (if (uri? s) s
      (match (cdr (regexp-match (pregexp
                                 (string-append 
                                  "^\\s*(?:([\\w\\d+-.]+)(?=://))?" ; scheme  
                                  "(?:://([^/?#]*))?" ; authority
                                  "([^?#]*)" ; path
                                  "(?:\\?([^#]*))?" ; query
                                  "(?:#(\\S*))?\\s*$")) (html-decode* s))) ; fragment
        [(list scheme authority path query fragment)
         (apply uri `(,scheme ,@(authority->userinfo&host&port authority) ,(if (string=? "" path) #f path) ,query ,fragment))])))

(define (authority->userinfo&host&port authority)
  (match (cdr (regexp-match (pregexp
                      (string-append
                       "(?:([^@]*)@)?"
                       "([^:]*)"
                       "(?::(\\d*))?")) 
                     (or authority "")))
    [(list userinfo host port) (list userinfo (if (string=? "" host) #f host) (and port (string->number port)))]))

(define (uri->path u)
  (string->path (uri-path u))) ;TODO further path research required

(define (path->uri path)
  (uri "file" #f "" #f (path->string path) #f #f))




;; Notes:
;; - url->string should be the exact inverse of string->url (do not encode query parameters)
;; - string->url should trim whitespace
;; This is a quick hack to get around net/url url->string's automatic url encoding.
;(define (url->string/raw u)
;  (string-append (url-scheme u) "://" (url-host u) (url->path&query&fragment u)))                
;
;; Returns everything following the host as a raw string. 
;(define (url->path&query&fragment u) 
;  (string-append
;   (url-path/string u) ; path
;   (if (empty? (url-query u)) "" (string-append* "?" (url-query/string u))) ; query
;   (if (url-fragment u) (string-append "#" (url-fragment u)) ""))) ; fragment;
;(define (url-path/string u)
;  (string-append
;   "/" (string-join
;        (map path/param-path (url-path u)) "/")))
;
;(define (url-query/string u)
;  (dict-map (url-query u) 
;            (lambda (k v) 
;              (string-append (symbol->string k) "=" (or v "")))))

; TESTS

(module+ test
  (require rackunit)
  
  (define u " http://user:pass@foo.com:8080/bar?baz=fuzz#buzz ")
  (define p (build-path "/foo/bar"))
 
  ; Parameters
  
  (check-equal? 
   (uri-query-params (string->uri "http://foo.com?bar=baz&amp;amp;fuzz= buzz ?"))
   '((bar . "baz") (fuzz . " buzz ?")))
  
  (let ([ur (string->uri u)])
    (check-equal? ur (string->uri ur)))
  
  (check-equal? (uri->string (uri->form-urlencoded (string->uri "http://foo.com?bar=baz&fuzz= buzz ?"))) "http://foo.com?bar=baz&fuzz=+buzz+%3F")
  
  (check-equal? (uri->string (uri-query-params-set (string->uri "http://foo.com?bar=baz") '((fuzz . " buzz ?") (fooz . "booz")))) "http://foo.com?fuzz= buzz ?&fooz=booz")
  
  ; Parsing
  
  (check-match (string->uri u) (uri "http" "user:pass" "foo.com" 8080 "/bar" "baz=fuzz" "buzz"))
  
  (check-equal? (string-trim u) (uri->string (string->uri u)))
  
  (check-equal? (string->uri "") (uri #f #f #f #f #f #f #f))
  
  (check-false (uri-relative-ref (string->uri "http://localhost:4949")))
  
  ; Path URIs 
  (check-equal? p (uri->path (path->uri p)))
  (check-equal? (uri->string (path->uri p)) "file:///foo/bar")
  
  ; Resolution
  (check-equal? (uri->string (combine-uri (string->uri u) (string->uri "foo/bar"))) "http://user:pass@foo.com:8080/foo/bar")
  
  
  ; Pretty printing\
  (let-values ([(in out) (make-pipe)]
               [(u) (string->uri "http://foo.com")])
    (write u out)
    (check-equal? (read in) "#<uri:http://foo.com>"))
  
)