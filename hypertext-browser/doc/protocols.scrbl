#lang scribble/manual

@(require (for-label net/url))

@title{Protocols}
@author{Evan Donahue}

These protocols represent the schemes that the browser can navigate, with the exception of the HYPERTEXT protocol, which is a meta-protocol that delegates to the appropriate sub-protocol based on the @racket[url-scheme].

@include-section["protocols/hypertext.scrbl"]
@include-section["protocols/file.scrbl"]
@include-section["protocols/http.scrbl"]