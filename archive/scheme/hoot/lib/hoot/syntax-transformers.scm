;;; Syntax transformers
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

;;; Commentary:
;;;
;;; Syntax transformers, as first-class values.
;;;
;;; Code:

(library (hoot syntax-transformers)
  (export make-syntax-transformer
          syntax-transformer?
          syntax-transformer-type
          syntax-transformer-value)
  (import (hoot syntax)
          (hoot inline-wasm))

  (define make-syntax-transformer
    (case-lambda
     ((type value)
      (%inline-wasm
       '(func (param $type (ref eq))
              (param $value (ref eq))
              (result (ref eq))
              (struct.new $syntax-transformer (i32.const 0)
                          (local.get $type) (local.get $value)))
       type value))
     ((sym type value)
      ;; Used when cross-compiling: Guile's psyntax will emit calls to
      ;; make-syntax-transformer with a useless initial argument.
      (make-syntax-transformer type value))))

  (define (syntax-transformer? x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (if (ref eq)
                (ref.test $syntax-transformer (local.get $x))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     x))

  ;; If x is not a syntax transformer, the ref.cast will crash.  It's
  ;; janky but we can't do too much else at this level.
  (define (syntax-transformer-type x)
    (%inline-wasm
     '(func (param $x (ref $syntax-transformer)) (result (ref eq))
            (struct.get $syntax-transformer $type (local.get $x)))
     x))

  (define (syntax-transformer-value x)
    (%inline-wasm
     '(func (param $x (ref $syntax-transformer)) (result (ref eq))
            (struct.get $syntax-transformer $value (local.get $x)))
     x)))
