;;; Hoot external references
;;; Copyright (C) 2023, 2024 David Thompson <dave@spritely.institute>
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
;;; External reference handling.
;;;
;;; Code:

(library (hoot external)
  (export external?
          external-null?)
  (import (hoot inline-wasm)
          (hoot errors)
          (hoot syntax))

  (define (external? obj)
    (%inline-wasm
     '(func (param $obj (ref eq)) (result (ref eq))
            (ref.i31
             (if i32
                 (ref.test $extern-ref (local.get $obj))
                 (then (i32.const 17))
                 (else (i32.const 1)))))
     obj))

  (define (external-null? extern)
    (check-type extern external? 'external-null?)
    (%inline-wasm
     '(func (param $extern (ref $extern-ref)) (result (ref eq))
            (if (ref eq)
                (ref.is_null
                 (struct.get $extern-ref $val (local.get $extern)))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     extern)))
