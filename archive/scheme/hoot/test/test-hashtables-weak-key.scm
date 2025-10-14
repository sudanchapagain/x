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

(test-begin "test-hashtables-weak-key")

(with-additional-imports ((hoot hashtables))
  ;; FIXME: These would need to be run in an async context in order
  ;; for any finalization to happen in the Hoot VM, but at least we
  ;; ensure that the main interface is working.
  (test-hashtable-impl make-weak-key-hashtable
                       make-eq-weak-key-hashtable
                       make-eqv-weak-key-hashtable
                       weak-key-hashtable?
                       weak-key-hashtable-hash
                       weak-key-hashtable-equiv
                       weak-key-hashtable-size
                       weak-key-hashtable-ref
                       weak-key-hashtable-set!
                       weak-key-hashtable-delete!
                       weak-key-hashtable-clear!
                       weak-key-hashtable-contains?
                       weak-key-hashtable-copy
                       weak-key-hashtable-keys
                       weak-key-hashtable-values
                       weak-key-hashtable-for-each
                       weak-key-hashtable-fold))

(test-end* "test-hashtables-weak-key")
