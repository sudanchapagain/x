;;; Boxes
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
;;; Boxes.
;;;
;;; Code:

(library (hoot boxes)
  (export box?
          make-box
          box-ref
          box-set!)
  (import (only (hoot primitives) %make-box %box-ref %box-set!)
          (hoot inline-wasm)
          (hoot syntax))

  (define (box? x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (if (ref eq)
                (ref.test $variable (local.get $x))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     x))
  (define (make-box init) (%make-box init))
  (define (box-ref box) (%box-ref box))
  (define (box-set! box val) (%box-set! box val)))
