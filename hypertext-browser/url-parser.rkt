#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
  (unreserved (:or alphabetic numeric (char-set "-._~")))
  (reserved (:or gen-delims sub-delims))
  (gen-delims (char-set ":/?#[]@"))
  (sub-delims (char-set "!$&'()*+,;="))
  (scheme (:: alphabetic (:* alphabetic numeric (char-set "+-.")))))
  

(define-empty-tokens delimiters (SCHEME COLON EOF))

(define lex-uri
  (lexer
   [(eof) (token-EOF)]
   [":" (token-COLON)]
   [scheme (token-SCHEME)]
   ))

(define parse-uri
  (parser
   (tokens delimiters)
          (start delim)
          (grammar (delim [(COLON) "WHATEVS"]))
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
  (define ip (open-input-string "http:"))
  (port-count-lines! ip)
  (pop-token ip))

(define (pop-token ip)
  (let ([token (lex-uri ip)])
    (if (equal? token (token-EOF))
        (list token)
        (cons token (pop-token ip)))))



(string->tokens "http:")

;(string->uri ":")