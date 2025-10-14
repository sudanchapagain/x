;;; R7RS (scheme process-context) library
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
;;; R7RS (scheme process-context) implementation
;;;
;;; Code:

(library (scheme process-context)
  (export command-line
          get-environment-variable
          get-environment-variables
          emergency-exit
          exit)
  (import (scheme base)
          (only (hoot errors) make-unimplemented-error)
          (hoot inline-wasm)
          (hoot match)
          (only (hoot syntax) define*))

  (define (command-line) '())
  (define (get-environment-variable name) #f)
  (define (get-environment-variables) '())
  (define* (emergency-exit #:optional status)
    (raise (make-unimplemented-error 'emergency-exit)))
  (define* (exit #:optional status)
    ;; TODO: Spec says to run all dynamic-wind after procedures.
    (%inline-wasm
     '(func (param $status i32)
            (call $quit (local.get $status))
            (unreachable))
     (match status
      (#t 0)
      (#f 1)
      ((? exact-integer?) status)
      (_ 1)))))
