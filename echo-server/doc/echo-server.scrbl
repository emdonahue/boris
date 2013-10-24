#lang scribble/manual

@(require
   scribble/extract
   (for-label "../main.rkt"))

@title{Echo Server}
@author{Evan Donahue}

Echo Server provides utiltites for running a simple HTTP echo server for testing network applications.

Note: the /head url is reserved for future use echoing custom headers.

@defmodule[echo-server]
@(include-extracted echo-server)



