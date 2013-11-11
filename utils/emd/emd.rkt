#lang racket

(define maybe-path? (or/c "" path-string?))

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
  [car/or (-> list? any/c any/c)]
  [value->file (-> path-string? any/c any/c)]
  [combine-path (-> maybe-path? maybe-path? maybe-path?)]
  [run-time-filename (-> path-string?)]
  [truncate (-> list? integer? list?)])
 debug-mode)

; DEBUGGING

(define debug-mode (make-parameter #t))

(define (dbg a b)
  (when (and a (debug-mode)) (display (format "~a: ~a\n" a b))) b)

; LIST

(define (truncate lst [pos 0])  
  (if (< (length lst) pos)
      lst (take lst pos)))


(define (chunk lst size)
  (reverse
   (map reverse
        (foldl (lambda (index element result)
                 (if (and (= (modulo index size) 0)
                          (< 0 index))
                     (cons (list element) result)
                     (cons (cons element (first result)) (list-tail result 1))))
               '(()) (range 0 (length lst)) lst))))

; DICT

(define (dict-invert d)
  (dict-map d (lambda (a b) (cons b a))))

; FILE IO

(define (value->file file value)
  (with-output-to-file file 
    (lambda () (write value))
  #:exists 'replace))

(define (run-time-filename)
  (let-values ([(dir filename dir?) (split-path (find-system-path 'run-file))])
     (path-replace-suffix filename "")))

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
  



; FILE

(define (combine-path base rel)
  (if (absolute-path? rel) (if (path? rel) rel (string->path rel))
      (let-values ([(dir name dir?) (split-path base)])
        (if dir? (build-path base rel)
            (build-path dir rel)))))


(module+ test
  (require rackunit)
  
  ; combine-path
  (check-equal? (combine-path "/foo/bar/" "/baz") (string->path "/baz"))
  (check-equal? (combine-path "/foo/bar/" "baz") (string->path "/foo/bar/baz"))
  (check-equal? (combine-path "/foo/bar" "baz") (string->path "/foo/baz"))
  
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
  (check-equal? (string-last "bar") #\r)
  
  ; DICT
  (check-equal? (dict-invert '((1 . 2) (3 . 4)))
                '((2 . 1) (4 . 3)))
  
  ; LIST
  
  (check-equal? (chunk '(1 2 3 4 5 6 7 8 9 10) 2)
  '((1 2) (3 4) (5 6) (7 8) (9 10)))
  )
