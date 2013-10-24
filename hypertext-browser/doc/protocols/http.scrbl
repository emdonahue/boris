#lang scribble/manual

@(require
   scribble/extract
   (for-label hypertext-browser/base
              hypertext-browser/http))



@title{HTTP}
@author{Evan Donahue}

Functions for navigating a web via HTTP.

@defmodule[hypertext-browser/http]
@(include-extracted hypertext-browser/http)