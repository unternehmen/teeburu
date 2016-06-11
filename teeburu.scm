#!/bin/sh
# -*- scheme -*-
exec /usr/bin/env guile-2.0 --debug -s $0 "$@"
!#

(use-modules (ice-9 match)      ; Pattern matching
             (rnrs bytevectors) ; UTF-to-string converter
             (srfi srfi-9)      ; Records
             (sxml simple)      ; SXML to XML conversion
             (web request)      ; HTTP request accessors
             (web uri)          ; URI building and accessors
             (web response)     ; HTTP response creator
             (web server))      ; HTTP serving

; configuration

(define limit-of-posts 10)
(define max-chars-in-title-field 64)
(define max-chars-in-author-field 64)
(define max-chars-in-content-field 1024)
(define port 8080)
(define host "127.0.0.1")

; code

(define-record-type <post>
  (make-post title author content)
  post?
  (title   post-title)
  (author  post-author)
  (content post-content))
  
(define (parse-url-encoded str)
  "Parse a URL-encoded string into its unencoded form."
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
                 ((#\% (? char-is-hex? sixteens)
                       (? char-is-hex? ones) . _)
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

(define (build-post-sxml post)
  "Generate an SXML snippet representing a single post."
  (match post
    (($ <post> title author content)
     `(div (@ (class "post"))
        (div (@ (class "title"))
          ,title) " "
        (div (@ (class "author"))
          "[" ,author "]") " "
        (div (@ (class "content"))
          ,content)))))

(define (build-main-page posts)
  "Generate an HTML page showing the given posts."
  (with-output-to-string
    (lambda ()
      (begin
        (display "<!DOCTYPE html>")
        (newline)
        (sxml->xml
          `(html (@ (lang "en"))
             (head
               (style (@ (type "text/css"))
                 ,(string-append
                    "label { "
                    "display: inline-block; "
                    "width: 5em; "
                    "text-align: right "
                    "} "
                    ".button { padding-left: 5em } "
                    ".post { background: lightgrey } "
                    ".title { display: inline; font-weight: bold } "
                    ".author { display: inline } ")))
             (body
               (h1 "Teeburu")
               (p
                 ,(format #f "Current posts: ~a" (length posts)))
               (form (@ (action "#") (method "POST"))
                 (p
                   ,(format
                      #f
                      "Field limits: 0 < x <= ~a, 0 < x <= ~a, 0 < x <= ~a"
                      max-chars-in-title-field
                      max-chars-in-author-field
                      max-chars-in-content-field))
                 (div
                   (label (@ (for "title")) "Title:") " "
                   (input (@ (type "text")
                             (id "title")
                             (name "title"))))
                 (div
                   (label (@ (for "author")) "Author:") " "
                   (input (@ (type "text")
                             (id "author")
                             (name "author"))))
                 (div
                   (label (@ (for "content")) "Content:") " "
                   (input (@ (type "text")
                             (id "content")
                             (name "content"))))
                 (div (@ (class "button"))
                   (button (@ (type "submit")) "Submit")))
               (div (@ (id "posts"))
                 ,@(map build-post-sxml posts)))))))))

(define (build-redirect-page)
  "Generate an HTML page telling the user to return to the main page."
  (with-output-to-string
    (lambda ()
      (begin
        (display "<!DOCTYPE html>")
        (newline)
        (sxml->xml
          '(html (@ (lang "en"))
            (body
              (p "Success!  To return to the list, click "
                (a (@ (href "#")) "here")
                "."))))))))

(define (try-to-add-post posts request-body)
  "Attempts to parse a POST request body to add a new post to the list."
  (let ((associations
          (map
            (lambda (association)
              (match (string-split association #\=)
                ((key value)
                   (cons (parse-url-encoded key)
                         (parse-url-encoded value)))))
            (string-split (utf8->string request-body) #\&))))
    (let ((maybe-title-pair (assoc "title" associations))
          (maybe-author-pair (assoc "author" associations))
          (maybe-content-pair (assoc "content" associations)))
      (if (or (not (pair? maybe-title-pair))
              (not (pair? maybe-author-pair))
              (not (pair? maybe-content-pair)))
        posts
        (let ((title (cdr maybe-title-pair))
              (author (cdr maybe-author-pair))
              (content (cdr maybe-content-pair)))
          (if (and (> (string-length title) 0)
                   (<= (string-length title) max-chars-in-title-field)
                   (> (string-length author) 0)
                   (<= (string-length author) max-chars-in-author-field)
                   (> (string-length content) 0)
                   (<= (string-length content) max-chars-in-content-field))
            (cons
              (make-post title author content)
              (if (= (length posts) limit-of-posts)
                (list-head posts (- (length posts) 1))
                posts))
            posts))))))

(define (handle-request request request-body posts)
  "Takes all HTTP requests and returns an appropriate HTTP response."
  (let ((path (uri-path (request-uri request))))
    (cond
      ((string=? "/" path)
        (case (request-method request)
          ((GET)
             (values
               '((content-type . (text/html)))
               (build-main-page posts)
               posts))
          ((POST)
             (values
               (build-response
                 #:code 303
                 #:headers `((content-type . (text/html))
                             (location .
                               ,(build-uri 'http
                                           #:path (string-append
                                                     path
                                                     "#")))))
               (build-redirect-page)
               (try-to-add-post posts request-body)))
          (else
            (values
              '((content-type . (text/plain)))
              (build-main-page posts)
              posts)))))))

(define (main args)
  "The entrypoint for the program."
  (begin
    (run-server handle-request
                'http
                `(#:host ,host
                  #:port ,port)
                '())))

(main (command-line))
