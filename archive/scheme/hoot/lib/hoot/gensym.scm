;;; Gensym
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
;;; To boldy cons, a symbol no one has seen before.
;;;
;;; Code:

(library (hoot gensym)
  (export gensym)
  (import (prefix (only (hoot primitives) gensym)
                  host:)
          (hoot inline-wasm)
          (hoot cross-compilation)
          (hoot write)
          (hoot numbers)
          (hoot strings)
          (hoot symbols)
          (hoot values)
          (hoot syntax))

  (cross-compilation-case
   (#t
    (define gensym host:gensym))
   (#f
    (define counter 0)

    (define (string->symbol* str)
      (%inline-wasm
       '(func (param $str (ref $string))
              (result (ref eq) (ref eq))
              (call $string->symbol* (local.get $str))
              (if (result (ref eq))
                  (then (ref.i31 (i32.const 17)))
                  (else (ref.i31 (i32.const 1)))))
       str))

    (define* (gensym #:optional (stem " "))
      (define str (string-append stem (number->string counter)))
      (set! counter (1+ counter))
      (call-with-values (lambda () (string->symbol* str))
        (lambda (sym fresh?)
          (if fresh?
              sym
              (gensym stem))))))))
