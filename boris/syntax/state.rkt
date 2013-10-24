#lang racket

(provide current-document current-parameters)

(define current-document 
  (make-parameter #f))

(define current-parameters
  (make-parameter #f))