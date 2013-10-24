#lang scribble/manual

@(require
   scribble/extract
   (for-label hypertext-browser/html/xpath))



@title{HTML}
@author{Evan Donahue}

Utilities for parsing HTML web pages retrieved by the browser. 

@include-section["xpath.scrbl"]
@include-section["links.scrbl"]
@include-section["forms.scrbl"]
@include-section["html-entities.scrbl"]