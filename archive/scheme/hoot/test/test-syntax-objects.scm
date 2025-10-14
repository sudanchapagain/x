;;; Copyright (C) 2024 Igalia, S.L.
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
;;; Symbol tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-syntax-objects")

(with-additional-imports ((only (hoot syntax) syntax)
                          (only (guile) object->string))
  (test-call "\"#<syntax foo>\"" object->string #'foo)
  (with-additional-imports ((hoot syntax-objects))
    (test-call "#t" syntax? #'foo)
    (test-call "foo" syntax-expression #'foo)
    (test-call "3" syntax-module (make-syntax 1 2 3 #f))
    (test-call "#f" syntax-sourcev #'foo)))

(test-end* "test-syntax-objects")
