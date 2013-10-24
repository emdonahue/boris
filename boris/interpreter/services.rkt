#lang racket/base

; Provides a capability object of external services to the Boris primitives.

; DOCUMENTATION

(provide services<%>)

; IMPLEMENTATION

(require racket/class)

; Minimum interface for a service object to be compadible with current Boris forms.
(define services<%> 
  (interface () request extract))



