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

(test-begin "test-hashtables")

(with-additional-imports ((hoot hashtables))
  (test-hashtable-impl make-hashtable
                       make-eq-hashtable
                       make-eqv-hashtable
                       hashtable?
                       hashtable-hash
                       hashtable-equiv
                       hashtable-size
                       hashtable-ref
                       hashtable-set!
                       hashtable-delete!
                       hashtable-clear!
                       hashtable-contains?
                       hashtable-copy
                       hashtable-keys
                       hashtable-values
                       hashtable-for-each
                       hashtable-fold))

(test-end* "test-hashtables")
