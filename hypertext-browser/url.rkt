#lang at-exp racket/base

(require racket/contract/base
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual))

;(provide 
; (proc-doc/names
;  url->string/raw 
;  (-> url? string?)
;  (url)
;  @{Inverse of @racket[string->url]. Does not encode query parameters.})
; 
; (proc-doc/names
;  url->path&query&fragment 
;  (-> url? string?)
;  (url)
;  @{Reurns the raw string from @racket[url-path] to @racket[url-fragment].})
; 
;  (proc-doc/names
;   url-path/string 
;   (-> url? string?)
;   (url)
;   @{Returns the raw @racket[url-path] as a string.}))

; IMPLEMENTATION

 (require racket/serialize
          racket/match
          racket/list
          racket/string
          racket/dict)

; DATATYPES ==============================

(serializable-struct uri (scheme userinfo host port path query fragment) #:transparent)



; PARSING =================================

(define (uri->string u)
  (match u
    [(uri scheme userinfo host port path query fragment)
     (string-append 
      (if scheme (string-append scheme ":") "")
      (authority->string userinfo host port)
      (or path "")
      (if query (string-append "?" query) "")
      (if fragment (string-append "#" fragment) ""))]))

(define (authority->string userinfo host port)
  (if (not host) ""
      (string-append
       "//"
       (if userinfo (string-append userinfo "@") "")
       host
       (if port (string-append ":" (number->string port)) ""))))

(define (string->uri s)
  (match (cdr (regexp-match (pregexp
                             (string-append 
                              "^\\s*(?:([\\w\\d+-.]+)(?=://))?" ; scheme  
                              "(?:://([^/?#]*))?" ; authority
                              "([^?#]*)" ; path
                              "(?:\\?([^#]*))?" ; query
                              "(?:#(\\S*))?\\s*$")) s)) ; fragment
    [(list scheme authority path query fragment)
     (apply uri `(,scheme ,@(authority->userinfo&host&port authority) ,(if (string=? "" path) #f path) ,query ,fragment))]))

(define (authority->userinfo&host&port authority)
  (match (cdr (regexp-match (pregexp
                      (string-append
                       "(?:([^@]*)@)?"
                       "([^:]*)"
                       "(?::(\\d*))?")) 
                     (or authority "")))
    [(list userinfo host port) (list userinfo (if (string=? "" host) #f host) (and port (string->number port)))]))





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
;   (if (url-fragment u) (string-append "#" (url-fragment u)) ""))) ; fragment
;
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
  
  (check-match (string->uri u) (uri "http" "user:pass" "foo.com" 8080 "/bar" "baz=fuzz" "buzz"))
  
  (check-equal? (string-trim u) (uri->string (string->uri u)))
  
  (check-equal? (string->uri "") (uri #f #f #f #f #f #f #f))
  
;  (check-equal? (url-path/string (string->url "http://foo.com")) "/")
;  (check-equal? (url->path&query&fragment (string->url "http://foo.com/bar?baz=f i?#fo")) "/bar?baz=f i?#fo")
;  (check-equal? (url->path&query&fragment (string->url "http://foo.com/bar?baz=f i?")) "/bar?baz=f i?")
;  (check-equal? (url->path&query&fragment (string->url "http://foo.com")) "/")
;  (check-equal? (url->path&query&fragment (string->url "http://foo.com/")) "/")
;  (check-equal? (url->path&query&fragment (string->url "http://foo.com/bar/")) "/bar/")
;  (check-equal? (url->path&query&fragment (string->url "http://foo.com/bar")) "/bar"))
  
)