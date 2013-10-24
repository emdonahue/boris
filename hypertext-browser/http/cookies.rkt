#lang at-exp racket/base

; Partial implementation of RFC 6265 compliant utilities for parsing and generating http cookies.

; DOCUMENTATION

(require racket/contract/base
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual))

(provide 
 
 (struct*-doc 
  cookie 
  ((name string?) (value string?) (domain string?) (path string?))
  @{An HTTP Cookie.})
 
 (proc-doc/names
  cookies-ref 
  (-> (listof cookie?) (or/c url? string?) (listof cookie?))
  (cookies name-or-request-url)
  @{If @racket[name-or-request-url] is a string, it is interpreted as a name and the cookies returned are those from @racket[cookies] matching that name. If @racket[name-or-request-url] is a url, the returned cookies will be those matching the @racket[url-host] and @racket[url-path] of @racket[name-or-request-url].})
 
  (proc-doc/names 
   cookies-set* 
   (-> (listof cookie?) (listof cookie?) (listof cookie?))
   (stale-cookies fresh-cookies)
   @{Appends @racket[fresh-cookies] to @racket[stale-cookies] and removes any cookies overwritten by newer ones.})
  
  (proc-doc/names 
   Set-Cookie->cookie 
   (-> string? url? cookie?)
   (Set-Cookie-header request-url)
   @{Parses @racket[Set-Cookie-header], filling in default Domain and Path values with @racket[request-url].})
  
  (proc-doc/names 
   cookies->Cookie 
   (-> (listof cookie?) string?)
   (cookies)
   @{Serializes @racket[cookies] into a Cookie header for use with @racket[http-client].})
  
  (contract-out [cookie=? (-> cookie? cookie? boolean?)])) ;TODO export this only at test time)

; IMPLEMENTATION

(require racket/serialize
         racket/match
         racket/string
         racket/dict
         racket/list
         net/url
         "../url.rkt"
         "../../utils/emd/emd.rkt")

; Datatypes

; Represents an http cookie.
(serializable-struct cookie (name value domain path) #:transparent)

; Datatype Extensions

(define (cookie=? c1 c2)
  (andmap string=? (cookie-name&domain&path c1) (cookie-name&domain&path c2)))

(define (cookie-name&domain&path c)
  (list (cookie-name c) (cookie-domain c) (cookie-path c)))

; Cookie Jar Accessors

(define/match (cookies-ref cookies name||url)
  [(cookies (and name (? string?)))
   (filter (lambda (cookie) (string=? name (cookie-name cookie))) cookies)]
  [(cookies (and u (? url?)))
   (filter (lambda (cookie) 
             (and 
              (domain-match 
               (url-host u) 
               (cookie-domain cookie))
              (path-match 
               (url-path/string u)
               (cookie-path cookie)))) cookies)])

; Check if a cookie domain-matches a host.
(define (domain-match host/raw domain)
  (let ([host (string-downcase host/raw)])
    (or (string=? host domain)
        (and (suffix? host domain)
             (char=? #\.
              (string-last
               (string- host domain)))))))

; Check if a cookie path-matches a request-path.
(define (path-match request-path cookie-path)
  (or (string=? request-path cookie-path)
      (and (prefix? request-path cookie-path)
           (or (char=? #\/ (string-last cookie-path))
               (char=? #\/ (string-ref 
                        (string-trim request-path cookie-path #:right? #f) 0))))))

; Cookie Jar Mutators

; Insert a cookie into a cookie jar and remove any cookies it overwrites.
(define (cookies-set jar cookie)
  (sort (remove-duplicates (cons cookie jar) cookie=?) 
        (lambda (longer shorter)
          (> (string-length longer)
             (string-length shorter)))
        #:key cookie-path))

(define (cookies-set* jar cookies)
  (foldl (lambda (cookie jar) 
           (cookies-set jar cookie))
         jar cookies))

; Header Manipulation

(define (cookies->Cookie cookies)
  (string-join 
   (for/list ([cookie cookies])
     (format "~a=~a" 
             (cookie-name cookie)
             (cookie-value cookie)))
   "; "))

(define (Set-Cookie->cookie set-cookie-string url)
  (cookie:fill (parse/set-cookie set-cookie-string) url))

; Fill in a raw Set-Cookie header with defaults from url.
(define (cookie:fill c url)
  (and c
       (struct-copy cookie c
               [domain (string-trim (string-downcase (or (cookie-domain c) (url-host url))) ".")]
               [path (or (cookie-path c) (url-path/string url))])))

; RFC 6265 5.2 algorithm for parsing Set-Cookie header fields.
(define (parse/set-cookie set-cookie-string)
  (match (regexp-match #px"([^=;]*)(?:=([^;]*))?(?:;(.*))?" set-cookie-string)
    [(list _ _ #f _) #f] ; no "="
    [(list _ (regexp #px"^\\s*$") _ _) #f] ; no name
    [(list _ name value cookie-av) (alist->cookie (append `((,name . ,value)) (parse/cookie-av (or cookie-av ""))))]))

; Parse a cookie-av string into an alist of attribute-value pairs.
(define (parse/cookie-av cookie-av)
  (match (regexp-match #px"([^=;]*)(?:=([^;]*))?(?:;(.*))?" cookie-av)
    [(list _ name value #f) `((,name . ,value))] ; no further cookie-av
    [(list _ name value cookie-av) (append `((,name . ,value)) (parse/cookie-av cookie-av))]))

; Produce a cookie struct from an alist. 
(define (alist->cookie d)
  (match d
    [(list (cons name value) av-pairs ...)
     (let ([cookie-av (normalize-cookie-av av-pairs)])
       (cookie (string-trim name)
               (string-trim value)
               (dict-ref cookie-av "domain" #f)
               (dict-ref cookie-av "path" #f)))])) ;TODO use appropriate defaults
         
; Trim and downcase a dict of cookie-av pairs.
(define (normalize-cookie-av cookie-av)  
  (reverse
   (dict-map cookie-av 
            (lambda (name value)
              (cons (string-downcase (string-trim (or name "")))
                    (string-trim (or value "")))))))

; TESTS

(module+ test 
  (require rackunit)
  
  (define c1 (cookie "foo" "bar" "foo.com" "/"))
  (define c2 (cookie "baz" "bez" "fii.foo.com" "/foo/bar/"))
  (define c3 (cookie "baz" "biz" "fii.foo.com" "/foo/bar/"))
  
  ; Accessors
  
  (check-true (domain-match "foo.com" "foo.com"))
  (check-true (domain-match "bar.foo.com" "foo.com"))
  (check-true (domain-match "bar.foo.com" "foo.com"))
  (check-false (domain-match "foo.com" ".foo.com"))
  
  (check-true (path-match "/foo" "/foo"))
  (check-true (path-match "/foo/bar" "/foo/"))
  (check-true (path-match "/foo/bar/" "/foo/bar"))
  (check-false (path-match "/foo" "/bar"))
  (check-false (path-match "/foobar" "/foo"))
  (check-false (path-match "/foobar" "/foo/"))
  (check-false (path-match "/" "/foo/bar"))
  
  (check-equal? (cookies-ref (list c1 c2 c3) "baz") (list c2 c3))
  (check-equal? (cookies-ref (list c1 c2 c3) (string->url "http://fii.foo.com")) (list c1))
  (check-equal? (cookies-ref (list c1 c2 c3) (string->url "http://fyy.foo.com/foo/bar/")) (list c1))
   (check-equal? (cookies-ref (list c1 c2 c3) (string->url "http://fii.foo.com/foo/bar/")) (list c1 c2 c3))

                              
   ; Mutators
  
  (check-equal? (cookies-set 
                 (cookies-set 
                  (cookies-set 
                   (list c1) c1) c2) c3) (list c3 c1))
  (check-equal? (cookies-set* (list c1) (list c2 c1)) (list c2 c1) )
  
  ; Generation
  
  (check-equal? (cookies->Cookie (list c1 c2)) "foo=bar; baz=bez")
  
  ; Parsing
  
  ; Generate a number of possible Set-Cookie strings and
  (for* ([name '("" "name" "  name  ")]
        [value '("" "=value" "==value" "=  value  ")]
        [path '("" ";" "; path = / ")]
        [domain '("" ";" "; domain = Foo.com ")])
    ; verify that the parser handles them correctly.
    (let* ([cookie-str (string-append name value path domain)]
           [cookie (Set-Cookie->cookie cookie-str (string->url "http://fuzz.com/buzz"))])  
      ; If the parser returned #f,
      (unless cookie
        ; the cookie must either 
        (check-true (or 
                     ; not have an = in the first pair,
                     (and                         
                      (not (regexp-match #px"=" name))
                      (not (regexp-match #px"=" value)))
                     ; or not have a name.
                     (string=? name ""))))
      (when cookie
        ; The parsed cookie must have correct name, 
        (check-equal? (cookie-name cookie) (string-trim name) cookie-str)
        ; value,
        (check-equal? (cookie-value cookie) (string-trim (regexp-replace #px"=" value "")) cookie-str)
        ; path, and
        (check-equal? (cookie-path cookie) (if (< 2 (string-length path)) "/" "/buzz") cookie-str)
        ; domain.
        (check-equal? (cookie-domain cookie) (if (< 2 (string-length domain)) "foo.com" "fuzz.com")) cookie-str)))
  (check-equal? (cookie-domain (Set-Cookie->cookie "name=value; domain=bar; domain=foo;" (string->url ""))) "foo"))
