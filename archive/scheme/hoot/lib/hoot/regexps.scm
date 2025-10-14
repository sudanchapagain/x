;;; Regular expressions
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
;;; Regular expressions.
;;;
;;; Code:

(library (hoot regexps)
  (export make-regexp
          regexp?
          regexp-exec

          regexp-match?
          regexp-match-string
          regexp-match-start
          regexp-match-end
          regexp-match-count
          regexp-match-substring
          regexp-match-prefix
          regexp-match-suffix)
  (import (hoot cond-expand)
          (hoot eq)
          (hoot errors)
          (hoot ffi)
          (hoot lists)
          (hoot match)
          (hoot numbers)
          (only (hoot primitives)
                guile:make-regexp
                guile:regexp?
                guile:regexp-exec
                guile:regexp-match?
                guile:match:string
                guile:match:start
                guile:match:end
                guile:match:count
                guile:match:substring)
          (hoot pairs)
          (hoot strings)
          (hoot syntax))

    (define-external-type <regexp>
      regexp?
      wrap-regexp
      unwrap-regexp)
    (define-external-type <regexp-match>
      regexp-match?
      wrap-regexp-match
      unwrap-regexp-match)

  (cond-expand
   (guile-vm
    (define make-regexp guile:make-regexp)
    (define regexp? guile:regexp?)
    (define regexp-exec guile:regexp-exec)
    (define regexp-match? guile:regexp-match?)
    (define regexp-match-string guile:match:string)
    (define regexp-match-start guile:match:start)
    (define regexp-match-end guile:match:end)
    (define regexp-match-count guile:match:count)
    (define regexp-match-substring guile:match:substring))
   (hoot
    (define-foreign %make-regexp "rt" "make_regexp"
      (ref string) (ref string) -> (ref extern))
    (define-foreign %regexp-exec "rt" "regexp_exec"
      (ref extern) (ref string) -> (ref null extern))
    (define-foreign %regexp-match-string "rt" "regexp_match_string"
      (ref extern) -> (ref string))
    (define-foreign %regexp-match-start "rt" "regexp_match_start"
      (ref extern) -> i32)
    (define-foreign %regexp-match-end "rt" "regexp_match_end"
      (ref extern) -> i32)
    (define-foreign %regexp-match-count "rt" "regexp_match_count"
      (ref extern) -> i32)
    (define-foreign %regexp-match-substring "rt" "regexp_match_substring"
      (ref extern) i32 -> (ref null string))

    (define (make-regexp pattern . flags)
      (check-type pattern string? 'make-regexp)
      (let ((flags (list->string
                    (fold (lambda (flag chars)
                            (match flag
                              ('case-insensitive (cons #\i chars))
                              ('multiline (cons #\m chars))
                              ;; This is the only regexp type and it's
                              ;; just here for compatibility with
                              ;; Guile's existing API.  "Basic" regexps
                              ;; are not supported.
                              ('extended chars)))
                          '() flags))))
        (wrap-regexp (%make-regexp pattern flags))))

    ;; TODO: Is it possible to respect flags on the web?  Doesn't seem
    ;; so.
    (define* (regexp-exec regexp str #:optional (start 0) (flags '()))
      (check-type str string? 'regexp-exec)
      (match (%regexp-exec (unwrap-regexp regexp)
                           (if (eq? start 0) str (string-copy str start)))
        ((? external-null?) #f)
        (m (wrap-regexp-match m))))

    (define (regexp-match-string m)
      (%regexp-match-string (unwrap-regexp-match m)))
    (define (regexp-match-start m)
      (%regexp-match-start (unwrap-regexp-match m)))
    (define (regexp-match-end m)
      (%regexp-match-end (unwrap-regexp-match m)))
    (define (regexp-match-count m)
      (%regexp-match-count (unwrap-regexp-match m)))
    (define* (regexp-match-substring m #:optional (n 0))
      (check-size n (1- (regexp-match-count m)) 'regexp-match-substring)
      (%regexp-match-substring (unwrap-regexp-match m) n))))

  (define (regexp-match-prefix m)
    (string-copy (regexp-match-string m) 0 (regexp-match-start m)))
  (define (regexp-match-suffix m)
    (string-copy (regexp-match-string m) (regexp-match-end m))))
