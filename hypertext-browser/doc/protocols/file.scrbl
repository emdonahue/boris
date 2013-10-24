#lang scribble/manual

@(require
   scribble/extract
   (for-label hypertext-browser/base
              hypertext-browser/file))



@title{FILE}
@author{Evan Donahue}

Functions for navigating a web of documents stored on the filesystem.

@defmodule[hypertext-browser/file]
@(include-extracted hypertext-browser/file)