#!/bin/sh
/usr/bin/env guile-2.0 --debug --no-auto-compile -s $0 "$@" || exit 1
exit 0
!#

(use-modules (sxml simple)
             (rnrs bytevectors)
             (ice-9 q)
             (ice-9 match)
             (srfi srfi-9)
             (web uri)
             (web request)
             (web server))

(define-record-type <post>
  (make-post title author content)
  post?
  (title post-title)
  (author post-author)
  (content post-content))

(define posts
  (list
    (make-post "(Test post) Help Wanted"
               "Luigi"
               "Seeking fellow lava cleaner for big spill.")
    (make-post "(Test post) RP-News #1"
               "ElephantFuzzy"
               "Don Pianta arrests himself - Shop closed in east corner")))

(define (generate-post pst)
  (match pst (($ <post> title author content)
    `(div (@ (class "post"))
       (div (@ (class "title")) ,title) " "
       (div (@ (class "author")) "[" ,author "]")
       (div (@ (class "content")) ,content)))))

(define (generate-page posts)
  (string-append
    (with-output-to-string
      (lambda ()
        (begin
          (display "<!DOCTYPE html>\n")
          (sxml->xml
            `(html (@ (lang "en"))
               (head
                 (title "Teeburu")
                 (link (@ (rel "stylesheet")
                          (type "text/css")
                          (href "style.css"))))
               (body
                 (h1 "Teeburu")
                 (form (@ (action "/post/") (method "POST"))
                   "Title: " (input (@ (name "title") (type "text")))
                   "Author: " (input (@ (name "author") (type "text")))
                   "Content: " (input (@ (name "content") (type "text")))
                   (input (@ (type "submit") (value "Submit"))))
                 ,@(map generate-post posts)))))))))

(define (generate-stylesheet)
  (with-output-to-string
    (lambda ()
      (for-each (lambda (line) (begin (display line) (newline)))
        (list
          "body {"
          "  background: white;"
          "  color: black"
          "}"
          ""
          ".title { display: inline; font-weight: bold }"
          ".author { display: inline }"
          ""
          ".post {"
          "  margin: 1em;"
          "  background: lightgray"
          "}")))))

; parse-url-encoded uses pattern matching to parse URL-encoded
; strings into their unencoded forms.  For example, the code
; (parse-escapes "Hello%2Bthere") returns the string "Hello+there".

(define (parse-url-encoded str)
  (letrec ((translate
             (lambda (orig new str)
               (list->string
                 (map (lambda (chr) (if (char=? chr orig) new chr))
                      (string->list str)))))
           (char-is-hex?
             (lambda (c)
               (or (and (char>=? c #\a) (char<=? c #\f))
                   (and (char>=? c #\A) (char<=? c #\F))
                   (and (char>=? c #\0) (char<=? c #\9)))))
           (iter
             (lambda (acc rest)
               (match rest
                 (() acc)
                 ((#\% (? char-is-hex? sixteens) (? char-is-hex? ones) . _)
                  (iter
                    (cons
                      (integer->char
                        (string->number (string sixteens ones) 16))
                      acc)
                    (cdddr rest)))
                 (_ (iter (cons (car rest) acc) (cdr rest)))))))
    (apply string
           (reverse (iter '() (string->list (translate #\+
                                                       #\space
                                                       str)))))))

(define (request-handler request request-body)
  (let* ((uri (request-uri request))
         (path (uri-path uri)))
    (cond
      ((string-ci=? path "/style.css")
        (values
          '((content-type . (text/css)))
          (generate-stylesheet)))
      ((string=? path "/")
        (values
          '((content-type . (text/html)))
          (generate-page
            posts)))
      ((string=? path "/post/")
        (begin
          (set! posts
                (let ((alist
                        (map 
                          (lambda (association)
                            (match (string-split association #\=)
                              ((key value)
                                 (cons (string->symbol
                                         (parse-url-encoded key))
                                       (parse-url-encoded value)))))
                          (string-split (utf8->string request-body) #\&))))
                  (cons (make-post
                          (cdr (assoc 'title alist))
                          (cdr (assoc 'author alist))
                          (cdr (assoc 'content alist)))
                        posts)))
          (values
            '((content-type . (text/plain)))
            "Post successful.")))
      (else
        (values
          '((content-type . (text/plain)))
          "404")))))

(begin
  (run-server request-handler))
