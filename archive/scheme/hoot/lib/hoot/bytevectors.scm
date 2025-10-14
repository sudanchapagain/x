;;; Bytevectors
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
;;; Bytevectors.
;;;
;;; Code:

(library (hoot bytevectors)
  (export make-bytevector
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector-s8-ref
          bytevector-s8-set!
          bytevector-u16-ref
          bytevector-u16-native-ref
          bytevector-u16-set!
          bytevector-u16-native-set!
          bytevector-s16-ref
          bytevector-s16-native-ref
          bytevector-s16-set!
          bytevector-s16-native-set!
          bytevector-u32-ref
          bytevector-u32-native-ref
          bytevector-u32-set!
          bytevector-u32-native-set!
          bytevector-s32-ref
          bytevector-s32-native-ref
          bytevector-s32-set!
          bytevector-s32-native-set!
          bytevector-u64-ref
          bytevector-u64-native-ref
          bytevector-u64-set!
          bytevector-u64-native-set!
          bytevector-s64-ref
          bytevector-s64-native-ref
          bytevector-s64-set!
          bytevector-s64-native-set!
          bytevector-uint-ref
          bytevector-sint-ref
          bytevector-uint-set!
          bytevector-sint-set!
          bytevector-ieee-single-ref
          bytevector-ieee-single-native-ref
          bytevector-ieee-single-set!
          bytevector-ieee-single-native-set!
          bytevector-ieee-double-ref
          bytevector-ieee-double-native-ref
          bytevector-ieee-double-set!
          bytevector-ieee-double-native-set!
          bytevector?
          bytevector
          bytevector-concatenate
          bytevector-concatenate-reverse
          bytevector-append
          bytevector-copy
          bytevector-copy!
          bytevector-slice
          endianness
          native-endianness)
  (import (rename (only (hoot primitives)
                        %null? %car %cdr
                        %bytevector-length %bytevector?
                        %bytevector-u8-ref %bytevector-u8-set!
                        %bytevector-s8-ref %bytevector-s8-set!
                        %bytevector-u16-native-ref %bytevector-u16-native-set!
                        %bytevector-s16-native-ref %bytevector-s16-native-set!
                        %bytevector-u32-native-ref %bytevector-u32-native-set!
                        %bytevector-s32-native-ref %bytevector-s32-native-set!
                        %bytevector-u64-native-ref %bytevector-u64-native-set!
                        %bytevector-s64-native-ref %bytevector-s64-native-set!
                        %bytevector-ieee-single-native-ref
                        %bytevector-ieee-single-native-set!
                        %bytevector-ieee-double-native-ref
                        %bytevector-ieee-double-native-set!
                        guile:make-bytevector guile:bytevector-copy!)
                  (%null? null?)
                  (%car car)
                  (%cdr cdr))
          (hoot cond-expand)
          (hoot errors)
          (hoot inline-wasm)
          (hoot match)
          (hoot numbers)
          (hoot syntax)
          (hoot syntax-objects)
          (hoot bitwise))

  (define (bytevector? x) (%bytevector? x))
  (define (bytevector-length bv) (%bytevector-length bv))

  (cond-expand
   (guile-vm
    (define make-bytevector guile:make-bytevector))
   (hoot
    (define* (make-bytevector len #:optional (init 0))
      (check-size len (1- (ash 1 29)) 'make-bytevector)
      (check-range init -128 255 'make-bytevector)
      (%inline-wasm
       '(func (param $len i32) (param $init i32)
              (result (ref eq))
              (struct.new
               $mutable-bytevector
               (i32.const 0)
               (array.new $raw-bytevector (local.get $init) (local.get $len))))
       len init))))

  (define (bytevector-u8-ref bv i)     (%bytevector-u8-ref bv i))
  (define (bytevector-u8-set! bv i x)  (%bytevector-u8-set! bv i x))
  (define (bytevector-s8-ref bv i)     (%bytevector-s8-ref bv i))
  (define (bytevector-s8-set! bv i x)  (%bytevector-s8-set! bv i x))
  (define (bytevector-u16-native-ref bv i)    (%bytevector-u16-native-ref bv i))
  (define (bytevector-u16-native-set! bv i x) (%bytevector-u16-native-set! bv i x))
  (define (bytevector-s16-native-ref bv i)    (%bytevector-s16-native-ref bv i))
  (define (bytevector-s16-native-set! bv i x) (%bytevector-s16-native-set! bv i x))
  (define (bytevector-u32-native-ref bv i)    (%bytevector-u32-native-ref bv i))
  (define (bytevector-u32-native-set! bv i x) (%bytevector-u32-native-set! bv i x))
  (define (bytevector-s32-native-ref bv i)    (%bytevector-s32-native-ref bv i))
  (define (bytevector-s32-native-set! bv i x) (%bytevector-s32-native-set! bv i x))
  (define (bytevector-u64-native-ref bv i)    (%bytevector-u64-native-ref bv i))
  (define (bytevector-u64-native-set! bv i x) (%bytevector-u64-native-set! bv i x))
  (define (bytevector-s64-native-ref bv i)    (%bytevector-s64-native-ref bv i))
  (define (bytevector-s64-native-set! bv i x) (%bytevector-s64-native-set! bv i x))
  (define (bytevector-ieee-single-native-ref bv i)    (%bytevector-ieee-single-native-ref bv i))
  (define (bytevector-ieee-single-native-set! bv i x) (%bytevector-ieee-single-native-set! bv i x))
  (define (bytevector-ieee-double-native-ref bv i)    (%bytevector-ieee-double-native-ref bv i))
  (define (bytevector-ieee-double-native-set! bv i x) (%bytevector-ieee-double-native-set! bv i x))

  (define-syntax endianness
    (lambda (x)
      (syntax-case x ()
        ((_ sym)
         (match (syntax->datum #'sym)
           ((or 'big 'little) #''sym)
           (_ (syntax-violation 'endianness "unsupported endianness" #'sym)))))))

  (define (native-endianness) (endianness little))

  (define (bytevector-u16-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 2) 'bytevector-u16-ref)
    (match endianness
      ('little (bytevector-u16-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.get_u $raw-bytevector
                            (local.get $vu0)
                            (i32.add (local.get $idx)
                                     (i32.const 1)))
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (local.get $idx))
                        (i32.const 8))
               (i32.or)
               (i64.extend_i32_u))
        bv index))))

  (define (bytevector-u16-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 2) 'bytevector-u16-set!)
    (check-size value (1- (ash 1 16)) 'bytevector-u16-set!)
    (match endianness
      ('little (bytevector-u16-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value i32)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (local.get $value))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.shr_u (local.get $value) (i32.const 8))))
        bv index value))))

  (define (bytevector-s16-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 2) 'bytevector-s16-ref)
    (match endianness
      ('little (bytevector-s16-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.get_u $raw-bytevector
                            (local.get $vu0)
                            (i32.add (local.get $idx)
                                     (i32.const 1)))
               (i32.shl (array.get_s $raw-bytevector
                                     (local.get $vu0)
                                     (local.get $idx))
                        (i32.const 8))
               (i32.or)
               (i64.extend_i32_s))
        bv index))))

  (define (bytevector-s16-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 2) 'bytevector-s16-set!)
    (check-range value (ash -1 15) (1- (ash 1 15)) 'bytevector-s16-set!)
    (match endianness
      ('little (bytevector-u16-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value i32)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (local.get $value))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.shr_s (local.get $value) (i32.const 8))))
        bv index value))))

  (define (bytevector-u32-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-u32-ref)
    (match endianness
      ('little (bytevector-u32-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.get_u $raw-bytevector
                            (local.get $vu0)
                            (i32.add (local.get $idx)
                                     (i32.const 3)))
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (i32.add (local.get $idx)
                                              (i32.const 2)))
                        (i32.const 8))
               (i32.or)
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (i32.add (local.get $idx)
                                              (i32.const 1)))
                        (i32.const 16))
               (i32.or)
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (local.get $idx))
                        (i32.const 24))
               (i32.or)
               (i64.extend_i32_u))
        bv index))))

  (define (bytevector-u32-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-u32-set!)
    (check-size value (1- (ash 1 32)) 'bytevector-u32-set!)
    (match endianness
      ('little (bytevector-u32-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value i32)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 3))
                          (local.get $value))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 2))
                          (i32.shr_u (local.get $value) (i32.const 8)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (i32.shr_u (local.get $value) (i32.const 16)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.shr_u (local.get $value) (i32.const 24))))
        bv index value))))

  (define (bytevector-s32-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-s32-ref)
    (match endianness
      ('little (bytevector-s32-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.get_u $raw-bytevector
                            (local.get $vu0)
                            (i32.add (local.get $idx)
                                     (i32.const 3)))
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (i32.add (local.get $idx)
                                              (i32.const 2)))
                        (i32.const 8))
               (i32.or)
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (i32.add (local.get $idx)
                                              (i32.const 1)))
                        (i32.const 16))
               (i32.or)
               (i32.shl (array.get_s $raw-bytevector
                                     (local.get $vu0)
                                     (local.get $idx))
                        (i32.const 24))
               (i32.or)
               (i64.extend_i32_s))
        bv index))))

  (define (bytevector-s32-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-s32-set!)
    (check-range value (ash -1 31) (1- (ash 1 31)) 'bytevector-s32-set!)
    (match endianness
      ('little (bytevector-s32-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value i32)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 3))
                          (local.get $value))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 2))
                          (i32.shr_u (local.get $value) (i32.const 8)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (i32.shr_u (local.get $value) (i32.const 16)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.shr_s (local.get $value) (i32.const 24))))
        bv index value))))

  (define (bytevector-u64-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 8) 'bytevector-u64-ref)
    (match endianness
      ('little (bytevector-u64-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (i64.extend_i32_u
                (array.get_u $raw-bytevector
                             (local.get $vu0)
                             (i32.add (local.get $idx)
                                      (i32.const 7))))
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 6))))
                        (i64.const 8))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 5))))
                        (i64.const 16))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 4))))
                        (i64.const 24))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 3))))
                        (i64.const 32))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 2))))
                        (i64.const 40))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 1))))
                        (i64.const 48))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (local.get $idx)))
                        (i64.const 56))
               (i64.or))
        bv index))))

  (define (bytevector-u64-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-u64-set!)
    (check-size value (1- (ash 1 64)) 'bytevector-u64-set!)
    (match endianness
      ('little (bytevector-u64-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 7))
                          (i32.wrap_i64 (local.get $value)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 6))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 8))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 5))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 16))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 4))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 24))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 3))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 32))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 2))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 40))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 48))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 56)))))
        bv index value))))

  (define (bytevector-s64-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 8) 'bytevector-s64-ref)
    (match endianness
      ('little (bytevector-s64-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (i64.extend_i32_u
                (array.get_u $raw-bytevector
                             (local.get $vu0)
                             (i32.add (local.get $idx)
                                      (i32.const 7))))
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 6))))
                        (i64.const 8))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 5))))
                        (i64.const 16))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 4))))
                        (i64.const 24))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 3))))
                        (i64.const 32))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 2))))
                        (i64.const 40))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 1))))
                        (i64.const 48))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_s $raw-bytevector
                                      (local.get $vu0)
                                      (local.get $idx)))
                        (i64.const 56))
               (i64.or))
        bv index))))

  (define (bytevector-s64-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-s64-set!)
    (check-range value (ash -1 63) (1- (ash 1 63)) 'bytevector-s64-set!)
    (match endianness
      ('little (bytevector-s64-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value i64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 7))
                          (i32.wrap_i64 (local.get $value)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 6))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 8))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 5))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 16))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 4))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 24))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 3))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 32))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 2))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 40))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $value)
                                      (i64.const 48))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.wrap_i64
                           (i64.shr_s (local.get $value)
                                      (i64.const 56)))))
        bv index value))))

  (define (bytevector-uint-ref bv index endianness size)
    (check-size index (- (bytevector-length bv) size) 'bytevector-uint-ref)
    (match endianness
      ('little
       (case size
         ((1) (bytevector-u8-ref bv index))
         ((2) (bytevector-u16-native-ref bv index))
         ((4) (bytevector-u32-native-ref bv index))
         ((8) (bytevector-u64-native-ref bv index))
         (else
          (let lp ((i 0))
            (if (= i size)
                0
                (logior (ash (bytevector-u8-ref bv (+ index i))
                             (* i 8))
                        (lp (1+ i))))))))
      ('big
       (case size
         ((1) (bytevector-u8-ref bv index))
         ((2) (bytevector-u16-ref bv index endianness))
         ((4) (bytevector-u32-ref bv index endianness))
         ((8) (bytevector-u64-ref bv index endianness))
         (else
          (let lp ((i 0))
            (if (= i size)
                0
                (logior (ash (bytevector-u8-ref bv (+ index (- size 1 i)))
                             (* i 8))
                        (lp (1+ i))))))))))

  (define (bytevector-uint-set! bv index value endianness size)
    (check-size index (- (bytevector-length bv) size) 'bytevector-uint-set!)
    (check-size value (1- (ash 1 (* size 8))) 'bytevector-uint-set!)
    (match endianness
      ('little
       (case size
         ((1) (bytevector-u8-set! bv index value))
         ((2) (bytevector-u16-native-set! bv index value))
         ((4) (bytevector-u32-native-set! bv index value))
         ((8) (bytevector-u64-native-set! bv index value))
         (else
          (let lp ((i 0))
            (unless (= i size)
              (bytevector-u8-set! bv (+ index i) (logand #xff (ash value (- (* i 8)))))
              (lp (1+ i)))))))
      ('big
       (case size
         ((1) (bytevector-u8-set! bv index value))
         ((2) (bytevector-u16-set! bv index endianness value))
         ((4) (bytevector-u32-set! bv index endianness value))
         ((8) (bytevector-u64-set! bv index endianness value))
         (else
          (let lp ((i 0))
            (unless (= i size)
              (bytevector-u8-set! bv (+ index (- size 1 i))
                                  (logand #xff (ash value (- (* i 8)))))
              (lp (1+ i)))))))))

  (define (bytevector-sint-ref bv index endianness size)
    (check-size index (- (bytevector-length bv) size) 'bytevector-sint-ref)
    (match endianness
      ('little
       (case size
         ((1) (bytevector-s8-ref bv index))
         ((2) (bytevector-s16-native-ref bv index))
         ((4) (bytevector-s32-native-ref bv index))
         ((8) (bytevector-s64-native-ref bv index))
         (else
          (let lp ((i 0))
            (if (= i (1- size))
                (ash (bytevector-s8-ref bv (+ index i))
                     (* i 8))
                (logior (ash (bytevector-u8-ref bv (+ index i))
                             (* i 8))
                        (lp (1+ i))))))))
      ('big
       (case size
         ((1) (bytevector-s8-ref bv index))
         ((2) (bytevector-s16-ref bv index endianness))
         ((4) (bytevector-s32-ref bv index endianness))
         ((8) (bytevector-s64-ref bv index endianness))
         (else
          (let ((k (1- size)))
            (let lp ((i 0))
              (if (= i k)
                  (ash (bytevector-s8-ref bv (+ index (- k i)))
                       (* i 8))
                  (logior (ash (bytevector-u8-ref bv (+ index (- k i)))
                               (* i 8))
                          (lp (1+ i)))))))))))

  (define (bytevector-sint-set! bv index value endianness size)
    (check-size index (- (bytevector-length bv) size) 'bytevector-sint-set!)
    (check-range value (ash -1 (1- (* size 8)))
                 (1- (ash 1 (1- (* size 8))))
                 'bytevector-sint-set!)
    (match endianness
      ('little
       (case size
         ((1) (bytevector-u8-set! bv index value))
         ((2) (bytevector-u16-native-set! bv index value))
         ((4) (bytevector-u32-native-set! bv index value))
         ((8) (bytevector-u64-native-set! bv index value))
         (else
          (let lp ((i 0))
            (cond
             ((= i (1- size))
              (bytevector-s8-set! bv (+ index i) (ash value (- (* i 8)))))
             (else
              (bytevector-u8-set! bv (+ index i) (logand #xff (ash value (- (* i 8)))))
              (lp (1+ i))))))))
      ('big
       (case size
         ((1) (bytevector-u8-set! bv index value))
         ((2) (bytevector-u16-set! bv index endianness value))
         ((4) (bytevector-u32-set! bv index endianness value))
         ((8) (bytevector-u64-set! bv index endianness value))
         (else
          (let ((k (1- size)))
            (let lp ((i 0))
              (cond
               ((= i k)
                (bytevector-s8-set! bv (+ index (- k i))
                                    (ash value (- (* i 8)))))
               (else
                (bytevector-u8-set! bv (+ index (- k i))
                                    (logand #xff (ash value (- (* i 8)))))
                (lp (1+ i)))))))))))

  (define (bytevector-ieee-single-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-ieee-single-ref)
    (match endianness
      ('little (bytevector-ieee-single-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result f64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (array.get_u $raw-bytevector
                            (local.get $vu0)
                            (i32.add (local.get $idx)
                                     (i32.const 3)))
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (i32.add (local.get $idx)
                                              (i32.const 2)))
                        (i32.const 8))
               (i32.or)
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (i32.add (local.get $idx)
                                              (i32.const 1)))
                        (i32.const 16))
               (i32.or)
               (i32.shl (array.get_u $raw-bytevector
                                     (local.get $vu0)
                                     (local.get $idx))
                        (i32.const 24))
               (i32.or)
               (f32.reinterpret_i32)
               (f64.promote_f32))
        bv index))))

  (define (bytevector-ieee-single-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-ieee-single-set!)
    (match endianness
      ('little (bytevector-ieee-single-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value f32)
               (local $vu0 (ref $raw-bytevector))
               (local $i0 i32)
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (local.set $i0 (i32.reinterpret_f32 (local.get $value)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 3))
                          (local.get $i0))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 2))
                          (i32.shr_u (local.get $i0) (i32.const 8)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (i32.shr_u (local.get $i0) (i32.const 16)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.shr_u (local.get $i0) (i32.const 24))))
        bv index value))))

  (define (bytevector-ieee-double-ref bv index endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-ieee-double-ref)
    (match endianness
      ('little (bytevector-ieee-double-native-ref bv index))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (result f64)
               (local $vu0 (ref $raw-bytevector))
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (i64.extend_i32_u
                (array.get_u $raw-bytevector
                             (local.get $vu0)
                             (i32.add (local.get $idx)
                                      (i32.const 7))))
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 6))))
                        (i64.const 8))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 5))))
                        (i64.const 16))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 4))))
                        (i64.const 24))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 3))))
                        (i64.const 32))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 2))))
                        (i64.const 40))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (i32.add (local.get $idx)
                                               (i32.const 1))))
                        (i64.const 48))
               (i64.or)
               (i64.shl (i64.extend_i32_u
                         (array.get_u $raw-bytevector
                                      (local.get $vu0)
                                      (local.get $idx)))
                        (i64.const 56))
               (i64.or)
               (f64.reinterpret_i64))
        bv index))))

  (define (bytevector-ieee-double-set! bv index value endianness)
    (check-size index (- (bytevector-length bv) 4) 'bytevector-ieee-double-set!)
    (match endianness
      ('little (bytevector-ieee-double-native-set! bv index value))
      ('big
       (%inline-wasm
        '(func (param $bv (ref $bytevector)) (param $idx i32) (param $value f64)
               (local $vu0 (ref $raw-bytevector))
               (local $j0 i64)
               (local.set $vu0 (struct.get $bytevector $vals (local.get $bv)))
               (local.set $j0 (i64.reinterpret_f64 (local.get $value)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 7))
                          (i32.wrap_i64 (local.get $j0)))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 6))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 8))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 5))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 16))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 4))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 24))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 3))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 32))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 2))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 40))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (i32.add (local.get $idx)
                                   (i32.const 1))
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 48))))
               (array.set $raw-bytevector
                          (local.get $vu0)
                          (local.get $idx)
                          (i32.wrap_i64
                           (i64.shr_u (local.get $j0)
                                      (i64.const 56)))))
        bv index value))))

  (define (bytevector . inits)
    (define (length l)
      (let lp ((len 0) (l l))
        (if (null? l) len (lp (+ len 1) (cdr l)))))
    (let* ((len (length inits))
           (bv (make-bytevector len)))
      (let lp ((i 0) (inits inits))
        (when (< i len)
          (bytevector-u8-set! bv i (car inits))
          (lp (1+ i) (cdr inits))))
      bv))

  (define (bytevector-length* bv*)
    (let lp ((bv* bv*) (len 0))
      (match bv*
        (() len)
        ((bv . bv*) (lp bv* (+ len (bytevector-length bv)))))))

  (define (bytevector-concatenate bv*)
    (match bv*
      (() #vu8())
      ((bv) bv)
      (bv*
       (let* ((len (bytevector-length* bv*))
              (flattened (make-bytevector len 0)))
         (let lp ((bv* bv*) (cur 0))
           (match bv*
             (() flattened)
             ((bv . bv*)
              (bytevector-copy! flattened cur bv)
              (lp bv* (+ cur (bytevector-length bv))))))))))

  (define (bytevector-concatenate-reverse bv*)
    (match bv*
      (() #vu8())
      ((bv) bv)
      (bv*
       (let* ((len (bytevector-length* bv*))
              (flattened (make-bytevector len 0)))
         (let lp ((bv* bv*) (cur len))
           (match bv*
             (() flattened)
             ((bv . bv*)
              (let ((cur (- cur (bytevector-length bv))))
                (bytevector-copy! flattened cur bv)
                (lp bv* cur)))))))))

  (define (bytevector-append . args)
    (bytevector-concatenate args))

  (cond-expand
   (guile-vm
    (define* (bytevector-copy x #:optional (start 0) (end (bytevector-length x)))
      (let* ((len (- end start))
             (new (make-bytevector len)))
        (guile:bytevector-copy! x start new 0 len)
        new))
    (define* (bytevector-copy! to at from #:optional
                               (start 0) (end (bytevector-length from)))
      (guile:bytevector-copy! from start to at (- end start))))
   (hoot
    (define* (bytevector-copy x #:optional (start 0) (end (bytevector-length x)))
      (check-type x bytevector? 'bytevector-copy)
      (check-range start 0 (bytevector-length x) 'bytevector-copy)
      (check-range end start (bytevector-length x) 'bytevector-copy)
      (%inline-wasm
       '(func (param $src (ref $bytevector)) (param $start i32) (param $end i32)
              (result (ref eq))
              (local $i0 i32)
              (local $vu0 (ref $raw-bytevector))
              (local.set $i0 (i32.sub (local.get $end) (local.get $start)))
              (local.set $vu0 (array.new_default $raw-bytevector (local.get $i0)))
              (array.copy $raw-bytevector $raw-bytevector
                          (local.get $vu0) (i32.const 0)
                          (struct.get $bytevector $vals (local.get $src))
                          (local.get $start) (local.get $i0))
              (struct.new $bytevector (i32.const 0) (local.get $vu0)))
       x start end))

    (define* (bytevector-copy! to at from #:optional
                               (start 0) (end (bytevector-length from)))
      ;; FIXME: check that `to` is mutable
      (check-type to bytevector? 'bytevector-copy!)
      (check-range at 0 (bytevector-length to) 'bytevector-copy!)
      (check-type from bytevector? 'bytevector-copy!)
      (check-range start 0 (bytevector-length from) 'bytevector-copy!)
      (check-range end start (bytevector-length from) 'bytevector-copy!)
      (%inline-wasm
       '(func (param $to (ref $mutable-bytevector)) (param $at i32)
              (param $from (ref $bytevector)) (param $start i32) (param $end i32)
              (array.copy $raw-bytevector $raw-bytevector
                          (struct.get $mutable-bytevector $vals (local.get $to))
                          (local.get $at)
                          (struct.get $bytevector $vals (local.get $from))
                          (local.get $start)
                          (i32.sub (local.get $end) (local.get $start))))
       to at from start end))))

  (define* (bytevector-slice bv offset #:optional
                             (size (- (bytevector-length bv) offset)))
    (raise (make-unimplemented-error 'bytevector-slice))))
