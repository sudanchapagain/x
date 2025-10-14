;;; Copyright (C) 2025 Igalia, S.L.
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;; Copyright (C) 2023, 2024 Robin Templeton
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
;;; Hashtable tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils)
             (test hashtable-utils))

(test-begin "test-hashtables-doubly-weak")

(with-additional-imports ((hoot hashtables))
  ;; FIXME: These would need to be run in an async context in order
  ;; for any finalization to happen in the Hoot VM, but at least we
  ;; ensure that the main interface is working.
  (test-hashtable-impl make-doubly-weak-hashtable
                       make-eq-doubly-weak-hashtable
                       make-eqv-doubly-weak-hashtable
                       doubly-weak-hashtable?
                       doubly-weak-hashtable-hash
                       doubly-weak-hashtable-equiv
                       doubly-weak-hashtable-size
                       doubly-weak-hashtable-ref
                       doubly-weak-hashtable-set!
                       doubly-weak-hashtable-delete!
                       doubly-weak-hashtable-clear!
                       doubly-weak-hashtable-contains?
                       doubly-weak-hashtable-copy
                       doubly-weak-hashtable-keys
                       doubly-weak-hashtable-values
                       doubly-weak-hashtable-for-each
                       doubly-weak-hashtable-fold))

(test-end* "test-hashtables-doubly-weak")
