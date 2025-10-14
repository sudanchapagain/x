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
;;; Regular expression tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-regexps")

(with-additional-imports ((hoot regexps))
  (test-call "#t"
             (lambda (pattern str)
               (regexp-match? (regexp-exec (make-regexp pattern) str)))
             "[fo]+"
             "foo")
  (test-call "#f"
             (lambda (pattern str)
               (regexp-match? (regexp-exec (make-regexp pattern) str)))
             "[fo]+"
             "bar")
  (test-call "\"xfoox\""
             (lambda (pattern str)
               (regexp-match-string (regexp-exec (make-regexp pattern) str)))
             "[fo]+"
             "xfoox")
  (test-call "1"
             (lambda (pattern str)
               (regexp-match-start (regexp-exec (make-regexp pattern) str)))
             "[fo]+"
             "xfoox")
  (test-call "4"
             (lambda (pattern str)
               (regexp-match-end (regexp-exec (make-regexp pattern) str)))
             "[fo]+"
             "xfoox")
  (test-call "\"foo\""
             (lambda (pattern str)
               (regexp-match-substring (regexp-exec (make-regexp pattern) str)))
             "[fo]+"
             "xfoox")
  (test-call "(\"feed\" #f \"feed\" \"\" nope)"
             (lambda (pattern str)
               (let* ((re (make-regexp pattern))
                      (m (regexp-exec re str)))
                 (list (regexp-match-substring m 0)
                       ;; doesn't match
                       (regexp-match-substring m 1)
                       ;; matches
                       (regexp-match-substring m 2)
                       ;: matches but with 0 chars
                       (regexp-match-substring m 3)
                       ;; index out of bounds
                       (with-exception-handler (lambda (exn) 'nope)
                         (lambda () (regexp-match-substring m 4))
                         #:unwind? #t))))
             "([abc]+)|([def]+)([ghi]*)"
             "feed")
  (test-call "\"aaa\""
             (lambda (pattern str)
               (regexp-match-prefix (regexp-exec (make-regexp pattern) str)))
             "b+"
             "aaabbbccc")
  (test-call "\"ccc\""
             (lambda (pattern str)
               (regexp-match-suffix (regexp-exec (make-regexp pattern) str)))
             "b+"
             "aaabbbccc"))

(test-end* "test-regexps")
