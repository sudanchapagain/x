;;; Debugging utilities
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
;;; Backtraces and so on.
;;;
;;; Code:

(library (hoot debug)
  (export dprint pk backtrace)
  (import (only (hoot primitives) %+ %- %<= guile:pk)
          (hoot cross-compilation)
          (hoot inline-wasm)
          (hoot match)
          (hoot syntax))

  (define (1+ x) (%+ x 1))
  (define (1- x) (%- x 1))
  (define (- x y) (%- x y))
  (define (<= x y) (%<= x y))

  (define dprint
    (case-lambda
     ((message)
      (%inline-wasm
       '(func (param $str (ref string))
              (call $debug-str (local.get $str)))
       message))
     ((message val)
      (%inline-wasm
       '(func (param $str (ref string)) (param $val (ref eq))
              (call $debug-str-scm (local.get $str) (local.get $val)))
       message val))))

  (define pk
    (cross-compilation-case
     (#t guile:pk)
     (#f (lambda (v . v*)
           (let lp ((v v) (v* v*))
             (match v*
               (()
                (dprint "pkv" v)
                v)
               ((v* . v**)
                (dprint "pk_" v)
                (lp v* v**))))))))

  (define (backtrace)
    (define (scm-sp)
      (%inline-wasm
       '(func (result (ref eq))
              (ref.i31 (i32.shl (global.get $scm-sp) (i32.const 1))))))
    (define (raw-sp)
      (%inline-wasm
       '(func (result (ref eq))
              (ref.i31 (i32.shl (global.get $raw-sp) (i32.const 1))))))
    (define (ret-sp)
      (%inline-wasm
       '(func (result (ref eq))
              (ref.i31 (i32.shl (global.get $ret-sp) (i32.const 1))))))
    (define (dyn-sp)
      (%inline-wasm
       '(func (result (ref eq))
              (ref.i31 (i32.shl (global.get $dyn-sp) (i32.const 1))))))
    (define (scm-ref n)
      (%inline-wasm
       '(func (param $n (ref i31))
              (result (ref eq))
              (ref.as_non_null
               (table.get $scm-stack
                          (i32.shr_s (i31.get_s (local.get $n))
                                     (i32.const 1)))))
       n))
    (define (raw-ref n)
      (%inline-wasm
       '(func (param $n (ref i31))
              (result (ref eq))
              (ref.i31
               (i32.shl
                (i32.load8_s $raw-stack
                             (i32.shr_s (i31.get_s (local.get $n))
                                        (i32.const 1)))
                (i32.const 1))))
       n))
    (let ((scm-sp (scm-sp))
          (raw-sp (raw-sp))
          (ret-sp (ret-sp))
          (dyn-sp (dyn-sp)))
      (dprint "scm backtrace" scm-sp)
      (let lp ((i 1))
        (when (<= 0 (- scm-sp i))
          (dprint "scm" (scm-ref (- scm-sp i)))
          (lp (1+ i))))
      (dprint "raw backtrace" raw-sp)
      (let lp ((i 1))
        (when (<= 0 (- raw-sp i))
          (dprint "raw" (raw-ref (- raw-sp i)))
          (lp (1+ i))))
      (dprint "ret stack height" ret-sp)
      (dprint "dyn stack height" dyn-sp)
      (dprint ""))))
