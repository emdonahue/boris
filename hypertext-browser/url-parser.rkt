#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
  (unreserved (:or alphabetic numeric (char-set "-._~")))
  (reserved (:or gen-delims sub-delims))
  (gen-delims (char-set ":/?#[]@"))
  (sub-delims (char-set "!$&'()*+,;="))
  (scheme (:: alphabetic (:* alphabetic numeric (char-set "+-."))))
  (hexdig (:or numeric (:/ "afAF"))))
  
(define-tokens alphanum (ALPHA DIGIT))
(define-empty-tokens delimiters (COLON SLASH PLUS DASH DOT EOF))

(define lex-uri
  (lexer   
   [alphabetic (token-ALPHA lexeme)]
   [numeric (token-DIGIT lexeme)]
   [":" (token-COLON)]
   ["/" (token-SLASH)]
   ["+" (token-PLUS)]
   ["-" (token-DASH)]
   ["." (token-DOT)]
   [(eof) (token-EOF)]
   ))

(define parse-uri
  (parser
   (tokens alphanum delimiters)
          (start uri)
          (grammar (uri [(scheme COLON) `((scheme . ,$1))])
                   (scheme [(scheme-char scheme) (string-append $1 $2)]
                               [(scheme-char) $1])
                   (scheme-char [(ALPHA) $1]
                                [(DIGIT) $1]
                                [(PLUS) "+"]
                                [(DASH) "-"]
                                [(DOT) "."]))
          (end EOF)
          (error (lambda args (display args)))))


(define (string->uri s)
  (with-input-from-string 
   s
   (lambda ()
     (define ip (current-input-port))
     (port-count-lines! ip)
     (parse-uri (lambda () (lex-uri ip))))))

;(parse-uri (lambda () (lex-uri (open-input-string ":"))))
;(lex-uri (open-input-string ":"))

;(parse-uri token-COLON)

(define (string->tokens s)
  (define ip (open-input-string s))
  (port-count-lines! ip)
  (pop-token ip))

(define (pop-token ip)
  (let ([token (lex-uri ip)])
    (if (equal? token (token-EOF))
        (list token)
        (cons token (pop-token ip)))))



;(string->tokens "http://")
(string->uri "http:")

;(string->uri ":")