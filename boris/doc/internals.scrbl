#lang scribble/manual

@(require racket/contract/base)

@title{Boris Internals and Extension}
@author{Evan Donahue}

Given the highly unpredictable nature of data out on the web, Boris was built to be as flexible and extensible as possible.

@section["Extending Webs"]

A web is simply a tree of functions composed of Racket lists. Because webs are just lists, adding new web-spinning forms or modifying existing webs is simple. For example, these three programs have roughly the same effect:


Using normal Boris forms:
@racketblock[(go "http://www.wikipedia.org"
                 (show (body)))]

Extending the web forms:
@racketblock[(go "http://www.wikipedia.org")
               (lambda (state services)
                 (display (browser-body (crawl-state-browser state)))
                 '())]]

Modifying the web at run-time:
@racketblock[(cons (car (make-web (go "http://www.wikipedia.org")))
                   (list (lambda (state services)
                           (display (browser-body (crawl-state-browser state)))
                           '())))]

@section["Extending Spiders"]

@defmodule[boris/interpreter/services]

The spider is a relatively simple function that threads the output states of web forms into the inputs of sub forms. As such, it would not be difficult to simply write a new spider function that executed breadth-first instead of the default spider's more depth-first orientation. However, much of the spider's functionality is provided by an extensible services object that it passes to every web form. Current Boris forms rely only on request and extract methods, but custom forms could rely on corresponding custom extensions to the external services object.

@definterface[services<%> () 
                          @defmethod[(request [state crawl-state?] [browser-request request?]) any/c]
                          @defmethod[(extract [datum any/c]) void?]]
             