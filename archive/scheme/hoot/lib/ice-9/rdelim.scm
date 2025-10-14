;;; Guile delimited read/write
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
;;; Guile delimited read/write module.
;;;
;;; Code:

(define-module (ice-9 rdelim)
  #:use-module (hoot errors)
  #:use-module (hoot match)
  #:use-module ((hoot ports) #:prefix port:)
  #:export (read-line
            read-line!
            read-delimited
            read-delimited!
            read-string
            read-string!
            %read-delimited!
            %read-line
            write-line))

(define* (%read-line #:optional (port (current-input-port)))
  "Read a newline-terminated line from @var{port}, allocating storage as
necessary.  The newline terminator (if any) is removed from the
string, and a pair consisting of the line and its delimiter is
returned.  The delimiter may be either a newline or the
@var{eof-object}.  If @code{%read-line} is called at the end of file,
it returns the pair @code{(#<eof> . #<eof>)}."
  (raise (make-unimplemented-error '%read-line)))

(define* (read-line #:optional (port (current-input-port)) (handle-delim 'trim))
  (raise (make-unimplemented-error 'read-line)))

(define* (read-line! str #:optional (port (current-input-port)))
  (raise (make-unimplemented-error 'read-line!)))

(define* (%read-delimited! delims str gobble #:optional
                           (port (current-input-port))
                           start end)
  (raise (make-unimplemented-error '%read-delimited!)))

(define* (read-delimited delims #:optional (port (current-input-port))
                         (handle-delim 'trim))
  (raise (make-unimplemented-error 'read-delimited)))

(define* (read-delimited! delims buf #:optional
                          (port (current-input-port)) (handle-delim 'trim)
                          (start 0) (end (string-length buf)))
  (raise (make-unimplemented-error 'read-delimited!)))

(define read-string
  (case-lambda*
   ((#:optional (port (current-input-port)))
    (string-concatenate
     (let lp ()
       (match (port:read-string 1000 port)
         ((? eof-object?) '())
         (str (cons str (lp)))))))
   ((port count)
    (port:read-string count port))))

(define* (read-string! buf #:optional
                       (port (current-input-port))
                       (start 0) (end (string-length buf)))
  (raise (make-unimplemented-error 'read-string!)))

(define* (write-line obj #:optional (port (current-output-port)))
  "Display @var{obj} and a newline character to @var{port}.  If
@var{port} is not specified, @code{(current-output-port)} is used."
  (display obj port)
  (newline port))
