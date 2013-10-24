#lang scribble/manual

@title{Boris Quickstart Guide}
@author{Evan Donahue}

This section will take you through a few example Boris programs to give you a feel for how Boris works. The completed programs can be found in the tests/boris collection.



@section["Know Your Electric Bass Guitarists"]

If you are still wondering, "Why 'Boris?'" then prepare to be educated. This first application will pull a list of bass guitarists from wikipedia, visit each of their pages, extract their nicknames, and then quiz you on the artist in question. Let's get ready to rock!

First, we need a web.

@racketblock[(require boris)
             (define web1 '())]

Passing the web to spider should produce a list of tasty tasty flies.

@racketblock[(spider web1)]

@racketresultblock['()]

Hmm... no flies. This reminds me of an ancient spider proverb: "You catch more flies with a web than with honey OR vinegar." We need to build a web!

@racketblock[(define web2 (show "No flies yet..."))
             (spider web2)]
           
@racketresultblock["No flies yet..."
                   '()]

web2 is a tree with no children that accepts a document, ignores it, and prints "No flies yet..." The "show" form is useful for development and debugging, and rather than printing a bare string, we can have it print the result of one of Boris' selector forms, which return data about the fetched page.

@racketblock[(define web3 (show (body)))
             (spider web3)]

@racketresultblock[""
                   '()]

The body form returns the full text of the fetched document. In this case, the blank document is simply "". In order to get started with the crawl, we need to nest the show tree within a navigation tree:

@racketblock[(define web4 (go '("http://en.wikipedia.org/wiki/John_Entwistle")
                                  web3))]

@racketresultblock["<!DOCTYPE html><..."
                   '()]

The go form accepts a list of urls and, for each url, fetches the corresponding document and passes that document to each of its sub-webs. Hence, we gain the ability to express conditionals (an empty list prunes that branch of the crawl) and iteration (a full list repeats the sub-webs for each document). Moreover, because each web is just a simple tree of Racket lists, we can easily modify, compose, and reuse tree segments to build more complex crawls. Now let's extract some flies. A quick perusal of the markup reveals that the name and nickname fields are both contained within &lt;td class='nickname'&gt; elements.

@racketblock[(define web5 (go '("http://en.wikipedia.org/wiki/John_Entwistle")
                                  (show (xpath "//td[@class='nickname']/text()"))))]

@racketresultblock['("John Alec Entwistle" "The Ox, Thunderfingers, The Quiet One, Big Johnny Twinkle")
                   '()]

Finally, some flies. The first item is the basist's name, the second is the nickname(s). To get the flies out of the crawl (show only prints to the console), we need to use an extraction form.

@racketblock[(define web6 (go '("http://en.wikipedia.org/wiki/John_Entwistle")
                   (extract (xpath "//td[@class='nickname']/text()"))))]

@racketresultblock['(("John Alec Entwistle"
   "The Ox, Thunderfingers, The Quiet One, Big Johnny Twinkle"))]

This crawl will supply our application with one datum. If this spider is going to make a living, we are going to have to cast a wider web.


@racketblock[(require boris persistent)
             
             (define web7
               (go "http://en.wikipedia.org/wiki/List_of_bass_guitarists"
                   (go (xpath "//div[@id='mw-content-text']/ul[position()>1]/li/a[1]/@href/text()")
                       (extract (xpath "//td[@class='nickname']/text()")))))
             (define flies (spider/generator web7 #:cache (make-fs-dict "/tmp/bassists")))
             (flies)
             (flies)]

@racketresultblock['("Willy Abers" "Breakdance Willy")
                   '("Bryan Guy Adams")]

This code fetches Wikipedia's list of bass guitarists, and then for each guitarist runs the previous extraction. So that we can use the results as we find them rather than waiting for the whole crawl to complete, we use the spider/generator form. We also use a persistent dict as the spider's cache so that after the first run we can play the game in the future without needing to make network requests. Now we can build our application:

@racketblock[(for ([fly (in-producer flies (void))])
               (when (= 2 (length fly))
                 (display (format "To which bassist does the moniker \"~a\" belong?\n" (second fly)))
                 (if (string-ci=? (read-line) (first fly))
                     (display "Correct!\n")
                     (display (format "Wrong: \"~a\"\n" (first fly))))))]

@racketresultblock[To which bassist does the moniker "Breakdance Willy" belong?
                      > willy abers
                      Correct!]

@section["Improved Hacker News Search"]

Not every site on the web even has a search feature, let alone one that does exactly what you want. Hacker News search, for example, can't show you pages where the click-through article matches your search term. This application will scan the first few pages of hacker news, click through each link, and print the title and url of the clicked article if the body of the landing page matches a user-provided regex.

To start with, we can make a web that grabs the first HN article, clicks through, and checks the resulting html with a regex. It is generally good practice to start with a persistent cache in place so that you can develop your crawler without repeatedly hitting the network.

@racketblock[(require boris persistent (only-in net/url url->string))
             
             (define web1 
               (go "https://news.ycombinator.com/"
                   (go (xpath "(//td[@class='title']/a/@href/text())[1]")
                       (extract 
                        (when (regexp-match #rx"." (html)) "An interesting article has been found!")))))
                       
                       (spider web1 #:cache (make-fs-dict "/tmp/hn-search"))]

@racketresultblock["An interesting article has been found!"]

An interesting article has been found. Wonderful. Since we have no title or url, we'll have to take Boris' word for it, and spiders are not known to be particularly refined critics of prose style. This code so far grabs the first link from Hacker News and, if that article contains any letter--any letter at all--Boris reports it as "interesting." Let's grab the link title and combine it with the url later after we check the page.


@racketblock[(define web2
               (go "https://news.ycombinator.com/"
                   (let ([link (xpath "(//td[@class='title']/a)[1]")])
                     (go (xpath "/a/@href/text()" (var link))
                         (extract 
                          (when (regexp-match #rx"." (html)) (format "~a: ~a\n" (first (xpath "/a/text()" (var link))) (url))))))))]

@racketresultblock['("Newly Declassified Documents Show How the Surveillance State was Born: http://www.newrepublic.com/article/114795/declassified-legal-opinions-show-how-surveillance-state-was-born")]

Now that we have it working for one page, we can widen the search to the whole list. We should also probably switch from spider to spider/generator so we can see the results as they come in.

@racketblock[(define search-page
               (for/web ([link (xpath "//td[@class='title']/a")])
                        (go (xpath "/a/@href/text()" (var 'link))
                            (extract 
                             (when (regexp-match #rx"." (body)) (format "~a: ~a" (first (xpath "/a/text()" (var 'link))) (url->string (url))))))))
             
             (define web3 
               (go "https://news.ycombinator.com/"
                   search-page))
                                
                                (define flies (spider/generator web3 #:cache (make-fs-dict "/tmp/hn-search")))
                                (for ([fly (in-producer flies (void))])
                                  (display fly) (newline))]

Now that we have the web that searches a whole page nicely abstracted out, we can follow the pagination links and apply this search to as many pages as we want. Because HN is optimized to serve recent pages, we'll limit it to the first 2.

@racketblock[(define web4  
  (go "https://news.ycombinator.com/"
      (let/web ([pages-crawled 0])
               (label hn-page
                      search-page
                      (go (when (< 2 (var 'pages-crawled)) 
                            (xpath "//td[@class='title']/a[text() = 'More']/@href/text()"))
                               (let/web ([pages-crawled (add1 (var 'pages-crawled))])
                                 (recur hn-page)))))))]

With this in hand, we can package it up into a function that searches over any regexp.

@racketblock[(define (search-hn/page rx)
  (for/web ([link (xpath "//td[@class='title']/a")])
               (go (xpath "/a/@href/text()" (var 'link))
                   (extract 
                    (when (regexp-match rx (body)) (format "~a: ~a" (first (xpath "/a/text()" (var 'link))) (url->string (url))))))))

(define (search-hn rx)
  (go "https://news.ycombinator.com/"
      (let/web ([pages-crawled 0])
               (label hn-page
                      (search-hn/page rx)
                      (go (when (< 2 (var 'pages-crawled)) 
                            (xpath "//td[@class='title']/a[text() = 'More']/@href/text()"))
                               (let/web ([pages-crawled (add1 (var 'pages-crawled))])
                                 (recur hn-page)))))))]

Now at last we can search for news of real interest.

@racketblock[(search-hn #rx"boris")]

@racketresultblock['()]

