#lang at-exp racket

; Provides an interface for working with http headers produced by http-client.

(require scribble/srcdoc
         racket/contract/base         
         (for-doc scribble/manual
                  racket/base
                  net/http-client))

(provide 
 
 ; General Headers
 (proc-doc/names headers->alist
                 (-> (listof bytes?) (listof (cons/c symbol? string?)))
                 (header)
                 @{Parses a list of header fields as returned by @racket[http-client] into an alist of title-cased symbols -> utf-8 strings.})
 
 (proc-doc/names alist->headers
                 (-> (listof (cons/c symbol? string?)) (listof bytes?))
                 (alist)
                 @{Returns a list of header fields suitable for use with @racket[http-client].})
 
 (proc-doc/names
  headers-set 
  (-> headers/c symbol? string? headers/c)
  (headers-alist header-field value)
  @{Removes all @racket[header-field] headers from @racket[headers-alist] and inserts @racket[value] as the new @racket[header-field] header.})
 
 ; Header-Specific accessors/Mutators
 (proc-doc/names 
  headers-Set-Cookies 
  (->* (headers/c url?) ((listof cookie?)) (listof cookie?))
  ((headers request-url) ((stale-cookies '())))
  @{Returns all cookies contained in Set-Cookie fields in @racket[headers] with Domain and Path defaults filled in from @racket[request-url]. If @racket[stale-cookies] are provided, the returned cookies will include the stale cookies that have not been overwritten by more freshly baked cookies.})
  
  (proc-doc/names 
   headers-Cookie-set 
   (-> headers/c (listof cookie?) headers/c)
   (headers cookies)
   @{Inserts a Cookie header into @racket[headers] filled in from @racket[cookies].})
  
  (proc-doc/names
   headers-Location
   (-> headers/c (or/c url? #f))
   (headers)
   @{Extracts the Location field from @racket[headers] or returns @racket[#f] if no Location is set.}))

(define headers/c (listof (cons/c symbol? string?)))
  
; IMPLEMENTATIONs

(require net/head
         net/url
         "cookies.rkt"
         "../../utils/emd/emd.rkt")

; General Headers

(define (headers->alist header)
  (for/list ([field header])
    (match (regexp-match (byte-pregexp #"([^:]*):(.*)") field)      
      [(list _ field-name field-value) 
       (cons 
        (string->symbol (string-titlecase (string-trim (bytes->string/utf-8 field-name)))) 
        (string-trim (bytes->string/utf-8 field-value)))])))

(define (alist->headers alist)
  (dict-map alist 
            (lambda (field value)
              (bytes-append
               (string->bytes/utf-8 
                (symbol->string field))
               #": "
               (string->bytes/utf-8 value)))))

(define (headers-set headers field value)
  (append (list (cons field value))
          (filter-not (lambda (field-value)
                        (equal? field 
                                (car field-value)))
                      headers)))

; Specific Header Fields

; Cookies 
(define (headers-Set-Cookies headers url [stale-cookies '()])
  (cookies-set*
   stale-cookies
   (for/list ([header-field headers]
                     #:when (equal? (car header-field) 'Set-Cookie))
     (Set-Cookie->cookie (cdr header-field) url))))

(define (headers-Cookie-set headers cookies)
  (if (empty? cookies)
      headers
      (headers-set headers 'Cookie
                   (cookies->Cookie cookies))))
         
; Location         
(define (headers-Location headers)
  (let ([location (dict-ref headers 'Location #f)])
    (if location (string->url location) #f)))

; TESTS

(module+ test
  (require rackunit)
  
  ; Headers
  (define head '(#"Set-Cookie: foo=fee; path=/bar ; domain = baz.com" #"Location: http://bar.com" #"Set-Cookie: baz=bax"))
  (check-equal? (headers->alist head) '((Set-Cookie . "foo=fee; path=/bar ; domain = baz.com") (Location . "http://bar.com") (Set-Cookie . "baz=bax")))
  (check-equal? (alist->headers (headers->alist head)) head)
  (check-equal? (url->string (headers-Location (headers->alist head))) (url->string (string->url "http://bar.com")))
  
  (let ([headers (headers->alist head)])
    (check-equal? (headers-set headers 'Set-Cookie "fu=chu") '((Set-Cookie . "fu=chu") (Location . "http://bar.com"))))
  
  ; Cookies
  (check-equal? (headers-Set-Cookies (headers->alist head) (string->url "http://fiz.com") (list (cookie "foo" "fyy" "baz.com" "/bar"))) 
           (list (cookie "foo" "fee" "baz.com" "/bar") 
                 (cookie "baz" "bax" "fiz.com" "/"))))
