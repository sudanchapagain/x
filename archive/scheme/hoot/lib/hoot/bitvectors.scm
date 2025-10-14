;;; Bitvectors
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
;;; Bitvectors.
;;;
;;; Code:

(library (hoot bitvectors)
  (export bitvector?
          make-bitvector
          bitvector-length
          bitvector-ref
          bitvector-set-bit!)
  (import (rename (only (hoot primitives)
                        %<= %< %- %exact-integer?
                        %bitvector?)
                  (%<= <=)
                  (%< <)
                  (%- -)
                  (%exact-integer? exact-integer?))
          (hoot bitwise)
          (hoot errors)
          (hoot inline-wasm)
          (hoot match)
          (hoot syntax))

  (define (1- x) (- x 1))

  (define (bitvector? x) (%bitvector? x))

  (define* (make-bitvector len #:optional (fill #f))
    (check-size len (1- (ash 1 29)) 'make-bitvector)
    (%inline-wasm
     '(func (param $len i32) (param $init i32) (result (ref eq))
            (struct.new $mutable-bitvector
                        (i32.const 0)
                        (local.get $len)
                        (array.new $raw-bitvector
                                   (local.get $init)
                                   (i32.add (i32.shr_u (i32.sub (local.get $len)
                                                                (i32.const 1))
                                                       (i32.const 5))
                                            (i32.const 1)))))
     len
     (match fill (#f 0) (#t -1))))

  (define (bitvector-length bv)
    (check-type bv bitvector? 'bitvector-length)
    (%inline-wasm
     '(func (param $bv (ref $bitvector))
            (result (ref eq))
            (ref.i31
             (i32.shl (struct.get $bitvector $len (local.get $bv))
                      (i32.const 1))))
     bv))

  (define (bitvector-ref bv i)
    (check-type bv bitvector? 'bitvector-ref)
    (check-index i (bitvector-length bv) 'bitvector-ref)
    (%inline-wasm
     '(func (param $bv (ref $bitvector))
            (param $i i32)
            (result (ref eq))
            (if (ref eq)
                (i32.and
                 (array.get $raw-bitvector
                            (struct.get $bitvector $vals (local.get $bv))
                            (i32.shr_s (local.get $i) (i32.const 5)))
                 (i32.shl (i32.const 1) (local.get $i)))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     bv i))

  (define (bitvector-set-bit! bv i)
    (define (mutable-bitvector? x)
      (%inline-wasm
       '(func (param $bv (ref eq)) (result (ref eq))
              (if (ref eq)
                  (ref.test $mutable-bitvector (local.get $bv))
                  (then (ref.i31 (i32.const 17)))
                  (else (ref.i31 (i32.const 1)))))
       x))
    (check-type bv mutable-bitvector? 'bitvector-set-bit!)
    (check-index i (bitvector-length bv) 'bitvector-set-bit!)
    (%inline-wasm
     '(func (param $bv (ref $mutable-bitvector))
            (param $i i32)
            (local $i0 i32)
            (local.set $i0 (i32.shr_s (local.get $i) (i32.const 5)))
            (array.set $raw-bitvector
                       (struct.get $bitvector $vals (local.get $bv))
                       (local.get $i0)
                       (i32.or
                        (array.get $raw-bitvector
                                   (struct.get $bitvector $vals (local.get $bv))
                                   (i32.shr_s (local.get $i) (i32.const 5)))
                        (i32.shl (i32.const 1) (local.get $i)))))
     bv i))
  ;; bitvector-set!, list->bitvector etc not yet implemented
  )
