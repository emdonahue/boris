#lang racket
(require boris
         persistent)


(define web1 '())

(define web2 (show "No flies yet..."))

(define web3 (show (body)))

(define web4 (go '("http://en.wikipedia.org/wiki/John_Entwistle")
                web3))

(define web5 (go '("http://en.wikipedia.org/wiki/John_Entwistle")
                   (show (xpath "//td[@class='nickname']/text()"))))

(define web6 (go '("http://en.wikipedia.org/wiki/John_Entwistle")
                   (extract (xpath "//td[@class='nickname']/text()"))))

(define web7
  (go "http://en.wikipedia.org/wiki/List_of_bass_guitarists" ; fetch the list of guitarists
      (go (xpath "//div[@id='mw-content-text']/ul[position()>1]/li/a[1]/@href/text()") ; fetch each guitarist in the list
                   (extract (xpath "//td[@class='nickname']/text()"))))) ; extract the guitarist's nickname


(define flies (spider/generator web7 #:cache (make-fs-dict "/tmp/bassists")))

(for ([fly (in-producer flies (void))])
  (when (= 2 (length fly))
  (display (format "To which bassist does the moniker \"~a\" belong?\n" (second fly)))
  (if (string-ci=? (read-line) (first fly))
      (display "Correct!\n")
      (display (format "Wrong: \"~a\"\n" (first fly))))))
  
