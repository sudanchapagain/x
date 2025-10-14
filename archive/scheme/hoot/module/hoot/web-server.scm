;;; Development web server
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Static file web server for development.
;;;
;;; Code:

(define-module (hoot web-server)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (serve))

;; Some common file extensions and their MIME types.
(define %mime-types
  '(("js"   . application/javascript)
    ("bin"  . application/octet-stream)
    ("json" . application/json)
    ("pdf"  . application/pdf)
    ("wasm" . application/wasm)
    ("xml"  . application/xml)
    ("mp3"  . audio/mpeg)
    ("ogg"  . audio/ogg)
    ("wav"  . audio/wav)
    ("gif"  . image/gif)
    ("jpeg" . image/jpeg)
    ("jpg"  . image/jpeg)
    ("png"  . image/png)
    ("svg"  . image/svg+xml)
    ("webp" . image/webp)
    ("ico"  . image/x-icon)
    ("css"  . text/css)
    ("csv"  . text/csv)
    ("html" . text/html)
    ("txt"  . text/plain)
    ("text" . text/plain)
    ("mp4"  . video/mpeg)
    ("ogv"  . video/ogg)))

(define (mime-type-for-file mime-types file-name)
  "Lookup the MIME type for FILE-NAME in the alist MIME-TYPES based upon
its file extension, or return 'text/plain' if there is no such type."
  (define (file-extension file)
    (let ((dot (string-rindex file #\.)))
      (and dot (substring file (+ 1 dot) (string-length file)))))
  (or (assoc-ref mime-types (file-extension file-name))
      'text/plain))

(define (stat:directory? stat)
  "Return #t if STAT is a directory."
  (eq? (stat:type stat) 'directory))

(define (directory? file-name)
  "Return #t if FILE-NAME is a directory."
  (stat:directory? (stat file-name)))

(define (directory-contents dir)
  "Return a list of the files contained within DIR."
  (define name+directory?
    (match-lambda
     ((name stat)
      (list name (stat:directory? stat)))))
  (define (same-dir? other stat)
    (string=? dir other))
  (match (file-system-tree dir same-dir?)
    ;; We are not interested in the parent directory, only the
    ;; children.
    ((_ _ children ...)
     (map name+directory? children))))

(define (request-file-name request work-dir)
  "Return the absolute file name corresponding to REQUEST in the context
of WORK-DIR, or #f if there is no such file."
  (define (request-path-components request)
    (split-and-decode-uri-path (uri-path (request-uri request))))
  ;; Forbid accessing files outside of the directory being served.
  (define (forbidden-components? components)
    (any (lambda (str)
           (or (string=? str ".") (string=? str "..")))
         components))
  (define (resolve-file-name path)
    ;; Implicitly resolve paths like "/" to "/index.html" files when
    ;; an index.html file exists.
    (let* ((file-name (string-append work-dir path))
           (index-file-name (string-append file-name "/index.html")))
      (cond
       ((file-exists? index-file-name) index-file-name)
       ((file-exists? file-name) file-name)
       (else #f))))
  (let ((components (request-path-components request)))
    (and (not (forbidden-components? components))
         (resolve-file-name
          (string-join components "/" 'prefix)))))

(define (render-file file-name mime-types)
  "Return a 200 OK HTTP response that renders the contents of
FILE-NAME."
  (values `((content-type . (,(mime-type-for-file mime-types file-name))))
          (call-with-input-file file-name get-bytevector-all)))

(define (render-directory path dir)
  "Render the contents of DIR represented by the URI PATH."
  (define (concat+uri-encode . file-names)
    (string-join (map uri-encode
                      (remove string-null?
                              (append-map (cut string-split <> #\/) file-names)))
                 "/" 'prefix))
  (define render-child
    (match-lambda
     ((file-name directory?)
      `(li
        (a (@ (href ,(concat+uri-encode path file-name)))
           ,(if directory?
                (string-append file-name "/")
                file-name))))))
  (define file-name<
    (match-lambda*
     (((name-a _) (name-b _))
      (string< name-a name-b))))
  (let* ((children (sort (directory-contents dir) file-name<))
         (title (string-append "Directory listing for " path))
         (view `(html
                (head
                 (title ,title))
                (body
                 (h1 ,title)
                 (ul ,@(map render-child children))))))
    (values '((content-type . (text/html)))
            (lambda (port)
              (display "<!DOCTYPE html>" port)
              (sxml->xml view port)))))

(define (not-found path)
  "Return a 404 response for PATH."
  (values (build-response #:code 404)
          (string-append "Resource not found: " path)))

(define (serve-file request work-dir mime-types)
  "Return an HTTP response for the file represented by PATH."
  (let ((path (uri-path (request-uri request))))
    (match (request-file-name request work-dir)
      (#f (not-found path))
      ((? directory? dir)
       (render-directory path dir))
      (file-name
       (render-file file-name mime-types)))))

(define* (serve #:key (work-dir (getcwd)) (port 8088) (addr INADDR_ANY)
                (mime-types %mime-types))
  "Run a simple HTTP server that serves the files in WORK-DIR over PORT
listening on ADDR.  MIME types are looked up by file extension in the
MIME-TYPES alist."
  (define (handler request body)
    (format #t "~a ~a\n"
            (request-method request)
            (uri-path (request-uri request)))
    (serve-file request work-dir mime-types))
  (format #t "Serving directory: ~a\n" work-dir)
  (format #t "Listening on: ~a:~a\n" (inet-ntop AF_INET addr) port)
  (run-server handler 'http `(#:port ,port #:addr ,addr)))
