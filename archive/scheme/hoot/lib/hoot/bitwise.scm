;;; Bitwise arithmetic
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
;;; R7RS (scheme cxr) implementation
;;;
;;; Code:

(library (hoot bitwise)
  (export logand logior logxor lognot logtest logbit? logcount ash)
  (import (hoot errors)
          (hoot inline-wasm)
          (hoot syntax)
          (only (hoot primitives)
                apply %logand %logior %logxor %logtest %ash
                %exact-integer? %<= %-))

  (define-syntax-rule (define-associative-eta-expansion f %f)
    (define f
      (case-lambda
       (() (%f))
       ((x) (%f x))
       ((x y) (%f x y))
       ((x y . z) (apply f (%f x y) z)))))

  (define-associative-eta-expansion logand %logand)
  (define-associative-eta-expansion logior %logior)

  ;; FIXME: Tree-il doesn't lower single-arity logxor.
  ;(define-associative-eta-expansion logxor %logxor)
  (define logxor
    (case-lambda
     (() 0)
     ((x) (%logxor x 0))
     ((x y) (%logxor x y))
     ((x y . z) (apply logxor (%logxor x y) z))))

  (define (lognot x) (%logxor x -1))
  (define (logtest j k) (%logtest j k))
  (define (logbit? idx k) (%logand k (%ash 1 idx)))

  (define (logcount x)
    (check-type x %exact-integer? 'logcount)
    (if (%<= (ash -1 29) x (%- (ash 1 29) 1))
        ;; Fixnum fast path.
        (%inline-wasm
         '(func (param $x i64) (result i64)
                (i64.popcnt (local.get $x)))
         x)
        ;; Bignum slow path.
        (%inline-wasm
         '(func (param $x (ref $bignum)) (result i64)
                (i64.extend_i32_s
                 (call $bignum-logcount
                       (struct.get $bignum $val
                                   (local.get $x)))))
         x)))

  (define (ash x y) (%ash x y)))
