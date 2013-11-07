#lang at-exp racket/base

; Utilities for extracting HTML forms frsm HTML documents.

; DOCUMENTATION


;TODO figure out how to include this for-doc
(define form/c (cons/c string?
                       (cons/c symbol?
                               (listof (listof any/c)))))

(require racket/contract/base
         scribble/srcdoc
         (for-doc 
          racket/base
          scribble/manual))

(provide 
; (proc-doc/names
;  form:fill
;  (-> (or/c string? form/c) dict? form/c)
;  (hmtl-or-form data)
;  @{Fills out @racket[html-or-form] (an HTML string representing a form or a list such as is produced by this function) with @racket[data] and returns a list representing the form.})
 (proc-doc/names
  forms
  (->* ((or/c string? (listof string?)))
       (dict? 
        #:submit (or/c string? regexp?)) 
       (listof form/c))
  ((html) ((data '()) (submit #px".")))
  @{Extracts each form from the @racket[html] string and fills them out with @racket[data] using @racket[form:fill]. })
 form/c)

; IMPLEMENTATION

(require racket/function
         racket/match
         racket/list
         racket/dict
         "html-entities.rkt"
         "../../utils/emd/emd.rkt"
         "xpath.rkt")



(define/match (form:fill form [data '()] #:submit [submit #px"."])
  [((and form (? string?)) _ _) (form:fill (form->list form submit) data)]
  [((list action method fields) _ _) 
   (list action method (remove-duplicates 
                        (append (dict-map data 
                                          (lambda (field value) 
                                            (cons (->symbol field) value)))
                                          fields) #:key car))])
   
(define (forms html [data '()] #:submit [submit-rx #px"."])
  (if (list? html) 
      (append* (map (curryr forms data) html))
      (map (curryr form:fill data #:submit submit-rx) (xpath html "//form"))))

; Form Extraction

(define (form->list form submit)
  (list (form-action form)
        (form-method form)
        (form-fields form submit)))

(define (form-action form)
  (html-decode* (car/or (xpath form "/form/@action/text()") "")))

(define (form-method form)
  (string->symbol (string-upcase (car/or (xpath form "/form/@method/text()") "POST"))))

(define (form-fields form submit)
  (define submit-rx (if (regexp? submit) submit
        (regexp submit)))
  
  (filter-map input->name&value
              (append (xpath form "/form//input[(@name or @id) and (@type!='submit' or not(@type))]")
                      (truncate (filter (compose (curry regexp-match submit-rx) input-name) (xpath form "/form//input[@type='submit' and (@name or @id)]")) 1))))

(define (input->name&value input)
  (cons (string->symbol (input-name input)) (input-value input)))

(define (input-name input)
  (xpath/first input "/input/@name/text() | /input/@id/text()"))

(define (input-value input)
  (car/or (xpath input "/input/@value/text()") ""))

(define (input-type input)
  (car/or (xpath input "/input/@type/text()") ""))

; TESTS

(module+ test
  (require rackunit)
(define form "<FORM action=/foo/bar method=put><input type=text id=foo name=baz value='bar'></input><input type=submit name=foo value=biz><input value=hug></input></input></FORM>")
  
  (define form/2submit "<FORM action=/foo/bar method=put><input type=text id=foo name=baz value='bar'></input><input type=submit name=foo value=biz></input><input type=submit name=fuzz value=buzz></input></FORM>")
  
  (define form/submit/noname "<FORM action=/foo/bar method=put><input type=text id=foo name=baz value='bar'><input type=submit></input></input></FORM>")
  
(check-match (form:fill form '(("foo" . "bez"))) 
             '("/foo/bar" PUT ((foo . "bez") (baz . "bar"))))
  
  (check-match (form:fill (form:fill form '(("foo" . "bez"))) '(("fii" . "biiz")))
               '("/foo/bar" PUT ((fii . "biiz") (foo . "bez") (baz . "bar"))))
  (check-equal? (forms (string-append form form) '(("foo" . "bez"))) 
         '(("/foo/bar" PUT ((foo . "bez") (baz . "bar"))) ("/foo/bar" PUT ((foo . "bez") (baz . "bar")))))
  
  (check-equal? (forms (list form form))
                '(("/foo/bar" PUT ((baz . "bar") (foo . "biz")))
                  ("/foo/bar" PUT ((baz . "bar") (foo . "biz")))))
  
  (check-equal? (forms form/2submit)
                '(("/foo/bar"
                   PUT ((baz . "bar") (foo . "biz")))))
  
  (check-equal? (forms form/2submit #:submit "fuzz")
                '(("/foo/bar"
                   PUT ((baz . "bar") (fuzz . "buzz")))))
  
  (check-equal? (forms form/submit/noname)
                '(("/foo/bar" PUT ((baz . "bar")))))
  )

