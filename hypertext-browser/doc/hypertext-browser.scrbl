#lang scribble/manual

@(require
   scribble/extract
   (for-label "../main.rkt"
              "../base.rkt"))

@title{Hypertext Browser}
@author{Evan Donahue}



Hypertext Browser provides a set of protocol-specific interfaces (such as @racket[http/request], @racket[http/click], and @racket[http/submit] for the world wide web) for navigating a web of hyperlinked documents. To the greatest extent possible, Hypertext Browser tries to handle concerns such as HTTP headers and cookies automatically, while allowing users finer-grained control if necessary. 

@racketblock[(require hypertext-browser)
             (define browser (make-hypertext-browser))
             (define wiki-request (hypertext/get browser "http://www.wikipedia.org"))
             (define (wiki-browser (wiki-request browser)))
             (display (browser-body wiki-browser))]

Note that the request functions draw both from user input and from the current browser state and return a fully inspectable @racket[request] object containing all information necessary. This way, the absolute url, domain-specific cookies, Referer headers etc. can be resolved ahead of time and the final request sent only after it passes any relevant subsequent inspection.


@defmodule[hypertext-browser/base]
@(include-extracted hypertext-browser/base)



@include-section["protocols.scrbl"]
@include-section["utilities.scrbl"]


