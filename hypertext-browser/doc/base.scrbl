#lang scribble/manual

@(require
   scribble/extract
   (for-label hypertext-browser/base))



@title{Interacting With The Browser}
@author{Evan Donahue}

@defmodule[hypertext-browser/base]

A new browser can be created with @racket[make-browser]. Requests are created with the protocol-specific request functions. 


@(include-extracted hypertext-browser/base)