#lang scribble/manual

@(require
   scribble/extract
   (for-label hypertext-browser/base
              hypertext-browser
              net/url))



@title{HYPERTEXT}
@author{Evan Donahue}

This is a meta protocol that reads the @racket[url-scheme] and tries to "do the right thing" ny mapping the reqest onto an appropriate sub-protocol.

@defmodule[hypertext-browser]
@(include-extracted hypertext-browser)