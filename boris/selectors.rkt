#lang at-exp racket/base

; Provides various utilities for querying and working with the current crawl state in order to collect data for extraction or compute urls for further crawling.

(require "syntax/state.rkt"
         "../hypertext-browser/base.rkt"
         scribble/srcdoc
         racket/contract/base
         racket/dict
         (prefix-in net/ net/url)
         (for-doc racket/base
                  scribble/manual)
         (prefix-in html: "../hypertext-browser/html.rkt"))


; BASIC ACCESSORS

(provide (proc-doc/names head (-> (listof (cons/c symbol? string?))) () @{Returns the headers of the most recent response.}))
(define (head) (cons 
                (response-status (browser-response (current-document)))
                (browser-head (current-document))))

(provide (proc-doc/names body (-> string?) () @{Returns the text of the current page.}))
(define (body) (browser-body (current-document)))

(provide (proc-doc/names url (-> net/url?) () @{Returns the current url.}))
(define (url) (browser-url (current-document)))

(provide (proc-doc/names previous-request (-> request?) () @{Returns the @racket[request] corresponding to the current page.}))
(define (previous-request) (browser-request (current-document)))

(provide (proc-doc/names var (-> symbol? any/c) (key) @{Returns the value currently bound to @racket[key] by one of Boris' binding forms.}))
(define (var key) (dict-ref (current-parameters) key))

; RE-EXPORTS OF HTML BROWSER UTILITIES

(provide (proc-doc/names xpath (->* (string?) ((or/c string? (listof string?))) (listof string?)) ((query) ((html (browser-body (current-document))))) @{Applies @racket[query] to @racket[html] and returns a list of matches. By default, html is the text of the current page.}))

(define (xpath query [html (browser-body (current-document))])
  (html:xpath html query))



(provide (proc-doc/names xpath/text 
                         (->* (string?) ((or/c string? (listof string?))) (listof string?)) ((query) ((html (browser-body (current-document))))) @{Appends "/text()" to @racket[query], applies it to @racket[html] and returns a list of matches. Unlike @racket[xpath], @racket[xpath/text] will preserve empty nodes as "" rather than skipping them. By default, html is the text of the current page.}))

(define (xpath/text query [html (browser-body (current-document))])
  (html:xpath/text html query))



(provide (proc-doc/names xpath/first
                         (->* (string?) (any/c (or/c string? (listof string?))) any/c) ((query) ((default #f) (html (browser-body (current-document))))) @{Returns the first result of @racket[xpath] or @racket[default] if @racket[xpath] finds no matches.}))

(define (xpath/first query [default #f] [html (browser-body (current-document))])
  (html:xpath/first html query default))



(provide (proc-doc/names forms 
                         (->* () (dict? (or/c string? (listof string?))) (listof html:form/c)) (() ((data '()) (html (browser-body (current-document))))) @{Scans @racket[html] for any html forms and returns a list of @racket[form/c]s ready to supply to the submit form. Each form's fields will be filled out with any name/value pairs supplied in @racket[data]. If @racket[html] is provided, html will be used instead of the default of the current page text. If @racket[html] is a list, forms will run on each element of the list and all forms found in any element will be returned in a flattened list. This makes @racket[forms] suitable for use with @racket[xpath] to submit only specific forms.}))
(define (forms [data '()] [html (browser-body (current-document))])
  (display html)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (newline)
  (html:forms html data))



(provide (proc-doc/names links 
                         (->* () (regexp? string?) (listof string)) (() ((link-regexp #rx".") (html (browser-body (current-document))))) @{Returns all urls extracted from links on the current page (or passed in @racket[html]) matching @racket[link-regexp].}))

(define (links [rx #rx"."] [html (browser-body (current-document))])
  (html:links html rx))

(provide (proc-doc/names links/text (->* (regexp?) (string?) (listof string)) ((link-text-regexp) ((html (browser-body (current-document))))) @{Returns all urls extracted from links on the current page (or passed in @racket[html]) with link text matching @racket[link-text-regexp].}))
(define (links/text rx [html (browser-body (current-document))])
  (html:links/text html rx))

