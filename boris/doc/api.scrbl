#lang scribble/manual

@(require
   scribble/extract
   (for-label boris/selectors))

@title{Boris API}
@author{Evan Donahue}

@defmodule[boris]

@section["Overview of Boris"]

Boris has three core concepts:

@itemlist[@item{A web is a program structure
defining the urls to visit and the data to extract.}
           @item{A spider is a function that interprets the web and performs the crawl, handling concerns external to the web logic such as caching,
robots.txt, and timing policies.}
           @item{The flies are the data extracted by the spider as it traverses the web and returned to the host program (via return value, yield, etc.).}]

@section["The Web"]

A web is a tree in which each node is a function. The function accepts the current crawl state (containing the current page, environment bindings, and position in the web) and returns a list of "next" states. The spider then executes the subsequent portions of the web for each of these states in turn. In this way, Boris forms can inherently express concepts such as branching (an empty list prunes the sub-tree beneath the current node) and iteration (a full list repeats
the sub-tree code for each state). This way, concepts such as "follow every link in the navigation div" have natural expressions in Boris.

All of Boris' basic web forms translate to one of five semantic operations: @secref["Navigation"], @secref["Extraction"], @secref["Binding"], @secref["Control"], or @secref["IO"]. Because a web is a simple tree of functions, however, it is possible to add new forms at runtime simply by modifying the tree. In addition to web-spinning forms, Boris also comes with a number of utilities listed under @secref["Selectors"] for accessing various parts of the current page (the text, the headers, the url, etc) and processing them with various xpath or regexp utilities.


@subsection["Navigation"]

Navigation forms request new pages from external sources, such as by fetching a document from the web.

@defform/subs[(go urls web)
              ([urls (or/c (listof string?) string?)]
               [web web/c])]{Fetches every document specified in @racket[urls] and executes @racket[web] with each document in turn as the root document of @racket[web]. If @racket[urls] is a string, it is automatically promoted to a singleton list.
                                                                 
                                                                 @racketblock[
                                                                              (go "http://www.wikipedia.com")
                                                                              ]
                                                                 
                                                                 Fetches www.wikipedia.com and does nothing with the result.
                                                                 
                                                                 @racketblock[
                                                                              (go "http://www.wikipedia.com"
                                                                                  (show (body)))]
                                                                 
                                                                 Fetches www.wikipedia.com and prints the html of the returned page.                                                                                                                                 
                                                                 @racketblock[
                                                                              (go (map (curry string-append "http://en.wikipedia.org/wiki/") '("John_Entwistle" "Boris_the_spider"))
                                                                                  (show (body)))]
                                                                 
                                                                 Fetches John Entwistle's wiki page and prints the html, then fetches Boris the Spider's page and prints the html.
                                                                 
                                                                 @racketblock[(go "http://www.wikipedia.com"
                                                                                  (go (xpath "//a/@href/text()")
                                                                                      (show (body))))]
                                                                 
                                                                 Fetches www.wikipedia.com, then collects each link on the page, fetches it, and prints the resulting html.}





@defform/subs[(submit www-forms web)
              ([www-forms (listof (list string? string? (listof pair?)))]
               [web web/c])]{Submits each form in @racket[www-forms] and runs @racket[web] with each resulting document as the root document. Designed to be used with the @racket[forms] query form.
                                                  
                                                  @racketblock[(go "https://email.foo.com"
                                                                   (submit (forms '((username . "bar")
                                                                                           (password . "baz"))))
                                                                           (show (xpath "//tr[@class='subject']/td/text()")))]
                                                  
                                                  Requests the home page for the hypothetical webmail service email.foo.com, submits the first form on the page (overwriting the default username and password values with appropriate login credentials), and prints a list of subject lines for emails in the inbox.}



@subsection["Extraction"]

Extraction forms return data discovered during the crawl to the host program.

@defform/subs[(extract datum)
              ([datum any/c]
               [web web/c])]{Yields @racket[datum] to the spider's generator. The values @racket[#f] and @racket[void?] are ignored by the extractor, permitting the use of expressions such as @racket[when] to extract data conditionally.
                                    
                                    @racketblock[
                                                 (go "http://en.wikipedia.org/wiki/John_Entwistle"
                                                     (extract (xpath "//td[@class='nickname']/text()")))
                                                 ]
                                    
                                    Extracts the name and nickname (both of which have class "nickname") and yields the list @racket['("John Alec Entwistle" "The Ox, Thunderfingers, The Quiet One, Big Johnny Twinkle")] to the spider.
                                    
                                    @racketblock[
                                                 (go "http://en.wikipedia.org/wiki/Boris_the_Spider"
                                                     (extract (unless (empty? (xpath "//td[@class='nickname']/text()")
                                                                              (xpath "//td[@class='nickname']/text()")))))
                                                 ]
                                    
                                    Extracts nothing, as the Boris the Spider page has no "nickname" field. No value is yielded to the spider.
                                    
                                    }

@defform/subs[(extract/list data web)
              ([data (listof any/c)]
               [web web/c])]{Yields each element of @racket[data] to the spider's generator in sequence. The values @racket[#f] and @racket[void?] as elements of @racket[data] are ignored by the extractor, permitting the use of expressions such as @racket[when] to extract data conditionally. This form composes well with @racket[xpath] forms, which return lists.
                                    
                                    @racketblock[
                                                 (go "http://en.wikipedia.org/wiki/John_Entwistle"
                                                     (extract/list (xpath "//td[@class='nickname']/text()")))
                                                 ]
                                    
                                    Extracts the name and nickname (both of which have class "nickname") and yields the values @racket["John Alec Entwistle"] and @racket["The Ox, Thunderfingers, The Quiet One, Big Johnny Twinkle"] to the spider in sequence.
                                    }

@subsection["Binding"]

Binding forms bind identifiers in the spider's memory environment for use later in the crawl, which can be useful for assembling extractable entities whose data is spread across several pages.

@defform[(let/web ([id val-expr] ...) web ...+)]{Largely analagous to the racket/base variant. Binds @racket[val-expr]s to @racket[id]s for use further down in the web. Values can be extracted with @racket[var].
                                                   
                                                   @racketblock[
                                                                (go "http://en.wikipedia.org/wiki/List_of_bass_guitarists"
                                                                    (let/web ([bassist (first (xpath "//div[@id='mw-content-text']/ul[position()>1]/li/a[text()='John Entwistle']/parent::li"))])
                                                                      (let ([band (first (xpath "/li/a[2]/text()" (var 'bassist)))])
                                                                        (go (xpath "/li/a[1]/@href/text()" (var 'bassist))
                                                                            (extract (cons (var band) (xpath "//td[@class='nickname']/text()")))))))
                                                                ]
                                                   
                                                   Fetches Wikipedia's list of bassists, binds Entwistle's entry to @racket[bassist] and Entwistle's band to @racket[band], fetches Entwistle's page, and finally displays Entwistle's name paired with the band name bound earlier.
                                                    }
                                                                 
@defform[(var id)]{Evaluates to a value previously bound to @racket[id] with a binding form. See @racket[let] for example. Will probably be replaced by direct variable reference in future versions.}


@defform[(for/web for-clause web ...+)]{Binds values in the usual way and executes @racket[web] once for each value bound. Values can be extracted with @racket[var].
                                                                               @racketblock[
                                                                                            (go "http://en.wikipedia.org/wiki/List_of_bass_guitarists"
                                                                                                (for ([bassist (xpath "//div[@id='mw-content-text']/ul[position()>1]/li")])
                                                                                                  (let/web ([band (first (xpath "/li/a[2]/text()" (var 'bassist)))])
                                                                                                    (go (xpath "/li/a[1]/@href/text()" (var 'bassist))
                                                                                                        (extract (cons (var' band) (xpath "//td[@class='nickname']/text()")))))))
                                                                                                                                                                                ]
                                                                               An extension of the @racket[let/web] example. Iterates over bassists, storing their band and pairing it with their name as extracted on the clicked-through page.}

@subsection["Control"]

Control forms dynamically alter the sub-trees the spider will visit next, allowing for control constructs such as recursion, which can concisely express common tasks like pagination and full-site mirroring.


@defform/subs[(label id webs ...)
              ((id identifier)
               (webs web/c))]{Binds the current position in the web to the @racket[id] identifier. The spider can be returned here recursively from the sub-web with the @racket[recur] command. This recursion form is useful anywhere code needs to be repeated an unknown number of times based on pages that have yet to be crawled. Full-site crawls and pagination are two common use cases.
                                                                           @racketblock[
                                                                                        (go "foo.com"
                                                                                            (label click-all-links
                                                                                              (go (xpath "//a/@href/text()")
                                                                                                  (extract (body))
                                                                                                  (recur click-all-links))))
                                                                                        ]
                                                                           
                                                                           This will crawl every link on foo.com and extract the html of every page encountered. It will then click every link on the returned pages and repeat the process until there are no more links on any page encountered, at which point the @racket[go] form will find no links and the @racket[recur] form will not run. This code will therefore download an entire site (more care would have to be taken to avoid external links). This form will probably be folded into let in future versions.}

@defform[(recur id)]{Returns the spider to the section of the web bound to @racket[id]. See @racket[lambda] for examples.}

@subsection["IO"]

@defform[(show value web ...)]{Displays the value to the @racket[current-output-port]. Mainly useful for debugging or progress monitoring.}
                                                         
@defform[(download path web ...)]{Writes the current (body) to the file specified by path. This can be useful for getting a closer look at a page with complex markup.}

@subsection["Selectors"]

@defmodule[boris/selectors]

@include-extracted[boris/selectors]
                                                         
                                                         