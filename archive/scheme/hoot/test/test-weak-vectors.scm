;;; Copyright (C) 2024 Vivianne Langdon
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
;;; Weak vector tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-weak-vectors")

(with-additional-imports
    ((hoot weak-vectors))

  (test-call "4" (lambda () (weak-vector-length (make-weak-vector 4))))

  (test-call "bar" (lambda () (weak-vector-ref (weak-vector 'foo 'bar 'baz) 1)))

  (test-call "#<unspecified>" (lambda () (weak-vector-ref (make-weak-vector 2) 0)))

  (test-call "42" (lambda () (weak-vector-ref (make-weak-vector 3 42) 2)))

  (test-call "baz"
             (lambda ()
               (let ((wv (make-weak-vector 3 'foo)))
                 (weak-vector-set! wv 1 'baz)
                 (weak-vector-ref wv 1))))

  (test-call "42"
             (lambda ()
               (let ((wv (list->weak-vector '(24 42 56))))
                 (weak-vector-ref wv 1)))))

(test-end* "test-weak-vectors")
