;;; Guile extensions to SRFI-9
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
;;; Guile extensions to record types.
;;;
;;; Code:
(define-module (srfi srfi-9 gnu)
  #:use-module ((hoot errors) #:select (check-type))
  #:use-module ((hoot primitives)
                #:select (%struct?
                          %struct-set!
                          guile:struct-vtable?
                          guile:vtable-index-printer))
  #:use-module (hoot inline-wasm)
  #:export (set-record-type-printer!))

(define (vtable? x)
  (cond-expand
   (guile-vm
    (guile:struct-vtable? x))
   (hoot
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (if (ref eq)
                (ref.test $vtable (local.get $x))
                (then
                 (ref.i31 (i32.const 17)))
                (else
                 (ref.i31 (i32.const 1)))))
     x))))

(define (set-record-type-printer! vtable proc)
  (check-type vtable vtable? 'set-record-type-printer!)
  (check-type proc procedure? 'set-record-type-printer!)
  (cond-expand
   (guile-vm
    (%struct-set! vtable guile:vtable-index-printer proc))
   (hoot
    (%inline-wasm
     '(func (param $vtable (ref $vtable)) (param $proc (ref eq))
            (struct.set $vtable $printer
                        (local.get $vtable)
                        (local.get $proc)))
     vtable (lambda (x port write-field) (proc x port))))))
