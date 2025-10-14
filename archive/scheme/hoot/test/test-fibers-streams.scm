;;; Copyright (C) 2024, 2025 Igalia, S.L.
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

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-fibers-streams")

(define-syntax-rule (test-fibers repr . body)
  (with-additional-imports ((fibers))
    (test-await repr . body)))

(with-additional-imports
 ((fibers channels)
  (fibers streams)
  (scheme write))
 (test-fibers "hey! (42)"
              (current-output-port (standard-output-stream))
              (display "hey! ")
              (flush-output-port)
              42))

(test-end* "test-fibers-streams")
