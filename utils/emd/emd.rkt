#lang racket

(provide 
 (contract-out 
  [dbg (-> (or/c string? #f) any/c any/c)]
  [string-last (-> string? char?)]
  [prefix? (-> string? string? boolean?)]
  [suffix (-> string? natural-number/c string?)]
  [suffix? (-> string? string? boolean?)]
  [string- (-> string? string? string?)]
  [->symbol (-> (or/c string? symbol?) symbol?)]
  [->list (-> any/c list?)]
  [car/or (-> list? any/c any/c)]))

; DEBUGGING

(define debug-mode (make-parameter #f))

(define (dbg a b)
  (when (and a (debug-mode)) (display (format "~a: ~a\n" a b))) b)

; COERCION

(define (->symbol s)
  (if (symbol? s) s (string->symbol s)))

; Wrap non-list values in a list (leave lists alone).
(define (->list maybe-head . maybe-rest)
  (if (empty? maybe-rest)
      (if (list? maybe-head)
          maybe-head
          (list maybe-head))
      (cons maybe-head maybe-rest)))

; CHOICE

(define (car/or lst [default #f])
  (if (empty? lst) default (car lst)))

; CHARACTER REFERENCE

(define (string-last str)
  (string-ref str (- (string-length str) 1)))

; PREFIX

(define (prefix? sequence subsequence)
  (and (<= (string-length subsequence)
           (string-length sequence))
       (string=? subsequence (substring sequence 0
                       (string-length subsequence)))))

; SUFFIX

(define (suffix sequence len)
  (if (<= len (string-length sequence))
       (substring sequence 
             (- (string-length sequence)
                len))
       sequence))

(define (suffix? sequence sfx)
  (string=?
   sfx
   (suffix sequence (string-length sfx))))

(define (string- subtractor subtractand)
  (if (suffix? subtractor subtractand)
      (substring subtractor 0 (- 
                               (string-length subtractor)
                               (string-length subtractand)))
      subtractor))
  





(module+ test
  (require rackunit)
  
  (check-equal? (->symbol "foo") 'foo)
  
  (check-true (prefix? "foobar" "foo"))
  (check-false (prefix? "foo" "foobar"))
  (check-true (prefix? "foobar" "foobar"))
  
  (check-equal? (suffix "foobar" 3) "bar")
  (check-true (suffix? "foobar" "bar"))
  (check-false (suffix? "foobar" "baz"))
  (check-false (suffix? "foo" "foobar"))
  (check-equal? (string- "foobar" "bar") "foo")
  (check-equal? (string- "foobar" "baz") "foobar")
  (check-equal? (string- "foobarbaz" "baz") "foobar")
  (check-equal? (string-last "bar") #\r))