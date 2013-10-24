#lang at-exp racket/base

; Provides the essential data structures that Boris' semantic functions depend on.

; DOCUMENTATION

(require racket/contract/base
         scribble/srcdoc
         racket/list
         racket/promise
         racket/dict
         (for-doc racket/base
                  scribble/manual))

(provide
 (struct*-doc crawl-state 
              ([browser any/c]
               [bindings dict?]
               [web web/c])
              @{Represents the state of a single point in a crawl as defined by the current @racket[browser] being processed, the environment @racket[bindings] available, and the remaining @racket[web] left to crawl. A Boris program is a tree in which each node is a procedure that accepts a @racket[crawl-state] and a @racket[services<%>] capability objext, and produces a list of @racket[crawl-state]s. A Boris @racket[spider] passes a crawl-state into the node procedure (along with some interpreter services such as request and extract for use in the procedure) and then passes each of the resulting @racket[crawl-state]s to each of the children of the tree. The process repeats recursively until the tree is exhausted.})
 web/c)
  
; IMPLEMENTATION

(struct crawl-state (browser bindings web) #:transparent)


(define web/c 
  (or/c empty?
        (cons/c (-> crawl-state? any/c (listof (or/c crawl-state? promise?))) ;TODO object contracts
                (listof (recursive-contract web/c))))) ; TODO figure
                                        ; out why this requires a
                                        ; promise?