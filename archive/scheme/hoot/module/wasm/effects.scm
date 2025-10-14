;;; WebAssembly effects analysis
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
;;; Effects analysis for WebAssembly.  Compute a simple approximation of
;;; the side effects for each WebAssembly instruction.
;;;
;;; We model three kinds of effects: control effects, reads, and writes.
;;; Stack effects are excluded; for that, see (wasm stack) instead.
;;;
;;; A control effect is a potential nonlocal branch.  The current
;;; analysis just notes the possible presence or definite absence of
;;; these effects.
;;;
;;; A read is a is either *, indicating that the instruction reads from
;;; potentially all mutable locations, or a list of specific mutable
;;; locations.  A mutable location is either:
;;;
;;;  - (global ID): a global named ID
;;;  - (table ID): a table named ID
;;;  - (memory ID): a memory named ID (either contents or size)
;;;  - (table ID): a table named ID (either contents or size)
;;;  - (struct TYPE FIELD): a field named FIELD of a struct of type TYPE
;;;  - (array TYPE): a field of an array of type TYPE
;;;
;;; This pass assumes that all ID's denote distinct entities.  This is
;;; the case if there are no duplicate types in the module, and no
;;; duplicate imports, and if all names are symbolified instead of
;;; referred to by index.  See (wasm symbolify).
;;;
;;; This pass also assumes the absence of traps.  For example, we assume
;;; that `table.get` will not issue an out-of-bounds trap, that
;;; `ref.cast` will not trap, and so on.
;;;
;;; As a limitation, we currently do not consider subtyping: reading
;;; e.g. a field from a given struct type and a subtype are considered
;;; separate locations.  We should instead canonicalize types and
;;; compute the array type that defines the given field.
;;;
;;; Code:

(define-module (wasm effects)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (wasm types)
  #:export (compute-effect
            <effect>
            effect-control?
            effect-reads
            effect-writes
            nofx))

(define-record-type <effect>
  (make-effect control? reads writes)
  effect?
  (control? effect-control?)
  (reads effect-reads)
  (writes effect-writes))

(define nofx (make-effect #f '() '()))

;; Assumptions:
;;  - no table get/set traps
;;  - no memory load/store traps
;;  - tables and memories do not alias: distinct identifiers denote
;;    different entities
;;  - no arithmetic traps (division by zero etc)
;;  - types do not alias.  subtyping??
;;  - no array access traps
;;  - no ref.cast / ref.as_non_null traps
;;  - no wtf8 traps or other string traps

(define (compute-effect inst)
  (define-syntax fx-access
    (syntax-rules (global
                   memory
                   table
                   struct
                   array)
      ((_ (global id))         `(global ,id))
      ((_ (memory id))         `(memory ,id))
      ((_ (table id))          `(table ,id))
      ((_ (struct type field)) `(struct ,type ,field))
      ((_ (array type))        `(array ,type))))
  (define-syntax fx-accesses
    (syntax-rules (*)
      ((_ *) '*)
      ((_ (access ...)) (list (fx-access access) ...))))
  (define-syntax-rule (%fx control? reads writes)
    (make-effect control? (fx-accesses reads) (fx-accesses writes)))
  (define-syntax-rule (fx reads writes) (%fx #f reads writes))
  (define-syntax-rule (fx! reads writes) (%fx #t reads writes))
  (match inst
    ((op . args)
     (match op
       ('unreachable          (fx! () ()))
       ('nop                  (fx () ()))
       ('block                (fx! * *))
       ('loop                 (fx! * *))
       ('if                   (fx! * *))
       ('try                  (fx! * *))
       ('try_delegate         (fx! * *))
       ('throw                (fx! () ()))
       ('rethrow              (fx! () ()))
       ('br                   (fx! () ()))
       ('br_if                (fx! () ()))
       ('br_table             (fx! () ()))
       ('return               (fx! () ()))
       ('call                 (fx! * *))
       ('call_indirect        (fx! * *))
       ('return_call          (fx! * *))
       ('return_call_indirect (fx! * *))
       ('call_ref             (fx! * *))
       ('return_call_ref      (fx! * *))
       ('drop                 (fx () ()))
       ('select               (fx () ()))
       ('local.get            (fx () ()))
       ('local.set            (fx () ()))
       ('local.tee            (fx () ()))
       ('global.get           (match args
                                ((id) (fx ((global id)) ()))))
       ('global.set           (match args
                                ((id) (fx () ((global id))))))

       ('table.copy           (match args
                                ((dst src)
                                 (fx ((table src)) ((table dst))))))
       ('table.fill           (match args
                                ((id)
                                 (fx () ((table id))))))
       ('table.grow           (match args
                                ((id)
                                 (fx ((table id))
                                     ((table id))))))
       ('table.init           (match args
                                ((id elem)
                                 (fx () ((table id))))))
       ('table.size           (match args
                                ((id)
                                 (fx ((table id)) ()))))
       ('table.get            (match args
                                ((id)
                                 (fx ((table id)) ()))))
       ('table.set            (match args
                                ((id)
                                 (fx () ((table id))))))

       ('memory.size          (match args
                                ((id)
                                 (fx ((memory id)) ()))))
       ('memory.grow          (match args
                                ((id)
                                 (fx ((memory id))
                                     ((memory id) (memory id))))))
       ('memory.copy          (match args
                                ((dst src)
                                 (fx ((memory src)) ((memory dst))))))
       ('memory.fill          (match args
                                ((id)
                                 (fx () ((memory id))))))
       ('memory.init          (match args
                                ((id data)
                                 (fx () ((memory id))))))
       ((or 'i32.load
            'i64.load
            'f32.load
            'f64.load
            'i32.load8_s
            'i32.load8_u
            'i32.load16_s
            'i32.load16_u
            'i64.load8_s
            'i64.load8_u
            'i64.load16_s
            'i64.load16_u
            'i64.load32_s
            'i64.load32_u)    (match args
                                ((($ <mem-arg> id offset align))
                                 (fx ((memory id)) ()))))
       ((or 'i32.store
            'i64.store
            'f32.store
            'f64.store
            'i32.store8
            'i32.store16
            'i64.store8
            'i64.store16
            'i64.store32)     (match args
                                ((($ <mem-arg> id offset align))
                                 (fx () ((memory id))))))

       ((or 'i32.const
            'i64.const
            'f32.const
            'f64.const)       (fx () ()))
       ((or 'i32.eqz
            'i32.eq
            'i32.ne
            'i32.lt_s
            'i32.lt_u
            'i32.gt_s
            'i32.gt_u
            'i32.le_s
            'i32.le_u
            'i32.ge_s
            'i32.ge_u
            'i64.eqz
            'i64.eq
            'i64.ne
            'i64.lt_s
            'i64.lt_u
            'i64.gt_s
            'i64.gt_u
            'i64.le_s
            'i64.le_u
            'i64.ge_s
            'i64.ge_u
            'f32.eq
            'f32.ne
            'f32.lt
            'f32.gt
            'f32.le
            'f32.ge
            'f64.eq
            'f64.ne
            'f64.lt
            'f64.gt
            'f64.le
            'f64.ge
            'i32.clz
            'i32.ctz
            'i32.popcnt
            'i32.add
            'i32.sub
            'i32.mul
            'i32.div_s
            'i32.div_u
            'i32.rem_s
            'i32.rem_u
            'i32.and
            'i32.or
            'i32.xor
            'i32.shl
            'i32.shr_s
            'i32.shr_u
            'i32.rotl
            'i32.rotr
            'i64.clz
            'i64.ctz
            'i64.popcnt
            'i64.add
            'i64.sub
            'i64.mul
            'i64.div_s
            'i64.div_u
            'i64.rem_s
            'i64.rem_u
            'i64.and
            'i64.or
            'i64.xor
            'i64.shl
            'i64.shr_s
            'i64.shr_u
            'i64.rotl
            'i64.rotr
            'f32.abs
            'f32.neg
            'f32.ceil
            'f32.floor
            'f32.trunc
            'f32.nearest
            'f32.sqrt
            'f32.add
            'f32.sub
            'f32.mul
            'f32.div
            'f32.min
            'f32.max
            'f32.copysign
            'f64.abs
            'f64.neg
            'f64.ceil
            'f64.floor
            'f64.trunc
            'f64.nearest
            'f64.sqrt
            'f64.add
            'f64.sub
            'f64.mul
            'f64.div
            'f64.min
            'f64.max
            'f64.copysign
            'i32.wrap_i64
            'i32.trunc_f32_s
            'i32.trunc_f32_u
            'i32.trunc_f64_s
            'i32.trunc_f64_u
            'i64.extend_i32_s
            'i64.extend_i32_u
            'i64.trunc_f32_s
            'i64.trunc_f32_u
            'i64.trunc_f64_s
            'i64.trunc_f64_u
            'f32.convert_i32_s
            'f32.convert_i32_u
            'f32.convert_i64_s
            'f32.convert_i64_u
            'f32.demote_f64
            'f64.convert_i32_s
            'f64.convert_i32_u
            'f64.convert_i64_s
            'f64.convert_i64_u
            'f64.promote_f32
            'i32.reinterpret_f32
            'i64.reinterpret_f64
            'f32.reinterpret_i32
            'f64.reinterpret_i64
            'i32.extend8_s
            'i32.extend16_s
            'i64.extend8_s
            'i64.extend16_s
            'i64.extend32_s
            'i32.trunc_sat_f32_s
            'i32.trunc_sat_f32_u
            'i32.trunc_sat_f64_s
            'i32.trunc_sat_f64_u
            'i64.trunc_sat_f32_s
            'i64.trunc_sat_f32_u
            'i64.trunc_sat_f64_s
            'i64.trunc_sat_f64_u) (fx () ()))

       ;; GC.
       ('ref.null            (fx () ()))
       ('ref.is_null         (fx () ()))
       ('ref.func            (fx () ()))
       ('ref.eq              (fx () ()))
       ('ref.as_non_null     (fx! () ()))
       ('struct.new          (fx () ()))
       ('struct.new_default  (fx () ()))
       ((or 'struct.get
            'struct.get_s
            'struct.get_u)   (match args
            ((type field)
             (fx ((struct type field)) ()))))
       ('struct.set          (match args
                               ((type field)
                                (fx () ((struct type field))))))
       ('array.new           (fx () ()))
       ('array.new_default   (fx () ()))
       ('array.new_fixed     (fx () ()))
       ('array.new_data      (fx () ()))
       ('array.new_elem      (fx () ()))
       ((or 'array.get
            'array.get_s
            'array.get_u)    (match args
                               ((type)
                                (fx ((array type)) ()))))
       ('array.set           (match args
                               ((type)
                                (fx () ((array type))))))
       ('array.len           (fx () ()))
       ('array.fill          (match args
                               ((type)
                                (fx () ((array type))))))
       ('array.copy          (match args
                               ((dst src)
                                (fx ((array src)) ((array dst))))))
       ('array.init_data     (match args
                               ((type data)
                                (fx () ((array type))))))
       ('array.init_elem     (match args
                               ((type elem)
                                (fx () ((array type))))))
       ('ref.test            (fx () ()))
       ('ref.cast            (fx () ()))
       ('br_on_cast          (fx! () ()))
       ('br_on_cast_fail     (fx! () ()))
       ('extern.internalize  (fx () ()))
       ('extern.externalize  (fx () ()))
       ('ref.i31             (fx () ()))
       ('i31.get_s           (fx () ()))
       ('i31.get_u           (fx () ()))

       ;; Stringref.
       ((or 'string.new_utf8
            'string.new_lossy_utf8
            'string.new_wtf8
            'string.new_wtf16)              (match args
                                              ((id)
                                               (fx ((memory id)) ()))))
       ('string.const                       (fx () ()))
       ((or 'string.measure_utf8
            'string.measure_wtf8
            'string.measure_wtf16)          (fx () ()))
       ((or 'string.encode_utf8
            'string.encode_lossy_utf8
            'string.encode_wtf8
            'string.encode_wtf16)           (match args
                                              ((id)
                                               (fx () ((memory id))))))
       ('string.concat                      (fx () ()))
       ('string.eq                          (fx () ()))
       ('string.is_usv_sequence             (fx () ()))
       ('string.compare                     (fx () ()))
       ('string.from_code_point             (fx () ()))

       ('string.as_wtf8                     (fx () ()))
       ('stringview_wtf8.advance            (fx () ()))
       ((or 'stringview_wtf8.encode_utf8
            'stringview_wtf8.encode_lossy_utf8
            'stringview_wtf8.encode_wtf8)   (match args
                                              ((id)
                                               (fx () ((memory id))))))
       ('stringview_wtf8.slice              (fx () ()))

       ('string.as_wtf16                    (fx () ()))
       ('stringview_wtf16.length            (fx () ()))
       ('stringview_wtf16.get_codeunit      (fx () ()))
       ('stringview_wtf16.encode            (match args
                                              ((id)
                                               (fx () ((memory id))))))
       ('stringview_wtf16.slice             (fx () ()))

       ('string.as_iter                     (fx () ()))
       ('stringview_iter.next               (fx () ()))
       ('stringview_iter.advance            (fx () ()))
       ('stringview_iter.rewind             (fx () ()))
       ('stringview_iter.slice              (fx () ()))

       ((or 'string.new_utf8_array
            'string.new_lossy_utf8_array
            'string.new_wtf8_array)         (fx ((array 'i8)) ()))
       ('string.new_wtf16_array             (fx ((array 'i16)) ()))
       ((or 'string.encode_utf8_array
            'string.encode_lossy_utf8_array
            'string.encode_wtf8_array)      (fx () ((array 'i8))))
       ('string.encode_wtf16_array          (fx () ((array 'i16))))
       
       ;; Vector opcodes.
       ('i8x16.splat                        (fx () ()))
       ('i16x8.splat                        (fx () ()))
       ('i32x4.splat                        (fx () ()))
       ('i64x2.splat                        (fx () ()))
       ('f32x4.splat                        (fx () ()))
       ('f64x2.splat                        (fx () ()))

       ('data.drop                          (fx! () ()))
       ('elem.drop                          (fx! () ()))

       (_ (error "unhandled instruction" inst))))))
