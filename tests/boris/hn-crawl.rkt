#lang racket

(require boris persistent (only-in net/url url->string))

(define web1 
  (go "https://news.ycombinator.com/"
      (go (xpath "(//td[@class='title']/a/@href/text())[1]")
          (extract 
           (when (regexp-match #rx"." (body)) "An interesting article has been found!")))))

(define web2
  (go "https://news.ycombinator.com/"
      (let/web ([link (xpath "(//td[@class='title']/a)[1]")])
               (go (xpath "/a/@href/text()" (var 'link))
                   (extract 
                    (when (regexp-match #rx"." (body)) (format "~a: ~a" (first (xpath "/a/text()" (var 'link))) (url->string (url)))))))))

(define search-page
  (for/web ([link (xpath "//td[@class='title']/a")])
               (go (xpath "/a/@href/text()" (var 'link))
                   (extract 
                    (when (regexp-match #rx"." (body)) (format "~a: ~a" (first (xpath "/a/text()" (var 'link))) (url->string (url))))))))

(define web3 
  (go "https://news.ycombinator.com/"
      search-page))
  
(define web4  
  (go "https://news.ycombinator.com/"
      (let/web ([pages-crawled 0])
               (label hn-page
                      search-page
                      (go (when (< 2 (var 'pages-crawled)) 
                            (xpath "//td[@class='title']/a[text() = 'More']/@href/text()"))
                               (let/web ([pages-crawled (add1 (var 'pages-crawled))])
                                 (recur hn-page)))))))

(define (search-hn/page rx)
  (for/web ([link (xpath "//td[@class='title']/a")])
               (go (xpath "/a/@href/text()" (var 'link))
                   (extract 
                    (when (regexp-match rx (body)) (format "~a: ~a" (first (xpath "/a/text()" (var 'link))) (url->string (url))))))))

(define (search-hn rx)
  (define page-search (search-hn/page rx))
  (go "https://news.ycombinator.com/"
      (let/web ([pages-crawled 0])
               (label hn-page
                      page-search
                      (go (if (> 1 (var 'pages-crawled)) 
                              (xpath "//td[@class='title']/a[text() = 'More']/@href/text()")
                              '())
                          (let/web ([pages-crawled (add1 (var 'pages-crawled))])
                                   (recur hn-page)))))))


(display "Enter a search term (regexp) to find Hacker News articles mentioning the term:\n")

(for ([fly (in-producer (spider/generator (search-hn (regexp (read-line)))) (void))]) (display fly) (newline))
  










;(search-hn #rx"boris")
;
;
;(define spider (make-spider #:cache (make-fs-store "/tmp/hn-crawl")))
;(define flies (spider web6))
;
;(for ([fly (in-producer flies (void))])
;  (display fly)
;  (newline))