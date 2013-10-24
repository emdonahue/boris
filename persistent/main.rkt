#lang at-exp racket/base

; Provides a collection of persistent datastructures that make use of generic interfaces.

; DOCUMENTATION

(require racket/contract/base
         scribble/srcdoc
         (for-doc
          racket/base
          racket/dict
          scribble/manual))

(provide 
 
 (struct*-doc
  fs-dict
  ([path string?]
   [key->tmp-path hash?])
  @{A filesystem-backed mutable @racket[dict]. Data is stored at the location @racket[fs-path] as a directory containing an index file and a number of temporary files.})
 
 (proc-doc/names 
  make-fs-dict 
  (->* () (path-string? #:exists (or/c 'replace 'append)) fs-dict?)
  (() ((fs-path (build-path (find-system-path 'temp-dir) "fs-dict")) (exists 'append)))
  @{Creates a @racket[fs-dict]. If @racket[exists] is @racket['append], then any index files found at @racket[fs-path] will be added to. Otherwise the index is eliminated.}))

; IMPLEMENTATION

(require 
  racket/generic
  racket/dict
  racket/file
  racket/serialize)

; An fs-store maintains a set of temp files (one per value) in the directory "path" and an "index" to re-read those files keyed to arbitrary seralizable objects.
(struct fs-dict (path key->tmp-path) #:transparent
  #:methods gen:dict
  [(define (dict-ref store key [failure-result #f])   
     (if (hash-ref (fs-dict-key->tmp-path store) key #f)
         (deserialize (file->value (hash-ref (fs-dict-key->tmp-path store) key #f)))
         failure-result))
   ; TODO decide whether ref needs to match the normal dict error-throwing semantics.
   
   (define (dict-set! cache key v)
     (let ([tmp-path (hash-ref 
                      (fs-dict-key->tmp-path cache) 
                      key 
                      (cache->tmpfile cache))])
       ; Write the value to disk,
       (write-to-file (serialize v) tmp-path #:exists 'replace)
       ; store its key in the index hash, and
       (hash-set! (fs-dict-key->tmp-path cache) key tmp-path)
       ; write the index hash to disk.
       (write-to-file (serialize (fs-dict-key->tmp-path cache))
                      (cache->index cache)
                      #:exists 'replace)))
   
   (define (dict-has-key? store key)
     (hash-has-key? (fs-dict-key->tmp-path store) key))])

; Constructs a new fs-store.
(define (make-fs-dict [path (build-path (find-system-path 'temp-dir) "fs-dict")] #:exists [exists 'append])
  (make-directory* path)
  (fs-dict path (if (and (not (equal? exists 'replace)) (file-exists? (path->index path)))
                     (deserialize (file->value (path->index path)))
                     (make-hash))))

; UTILITY

; Generates a temp file path name for cache.
(define (cache->tmpfile cache) 
  (make-temporary-file (path->string (build-path (fs-dict-path cache) "~a"))))

; Generates the index file name for cache.
(define (cache->index cache)
  (path->index (fs-dict-path cache)))
  
; Generates the index file name from a path.
(define (path->index path)
  (build-path path "index"))

; TESTS

(module+ test
  (require rackunit)
  (define in-store (make-fs-dict))
  (dict-set! in-store 'foo '(bar . "baz"))
  
  (define out-store (make-fs-dict))
  (check-true (dict-has-key? out-store 'foo))
  (check-equal? (dict-ref out-store 'foo) '(bar . "baz"))
  
  (dict-set! out-store 'foo "bar")
  (check-equal? (dict-ref in-store 'foo) "bar"))
