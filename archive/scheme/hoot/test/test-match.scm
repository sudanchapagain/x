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
;;; (ice-9 match) tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-match")

(with-additional-imports ((ice-9 match))
  (test-call "(2 3)" (match-lambda ((x . y) y)) '(1 2 3))

  (test-call "(2 3)" (match-lambda ((first rest ...) rest)) '(1 2 3))

  (test-call "2" (match-lambda (#(_ x _) x)) #(1 2 3))

  (test-call "42" (match-lambda ((? exact-integer? x) x)) 42)

  (test-call "42"
             (lambda ()
               (begin
                 (define-record-type <foo>
                   (make-foo bar)
                   foo?
                   (bar foo-bar))
                 (match (make-foo 42)
                   (($ <foo> bar) bar))))))

(test-end* "test-match")
