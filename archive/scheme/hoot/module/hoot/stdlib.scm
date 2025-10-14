;;; Standard library for Hoot runtime
;;; Copyright (C) 2023,2024 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
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
;;; Standard runtime routines for Hoot WebAssembly runtime.
;;;
;;; Code:

(define-module (hoot stdlib)
  #:use-module (wasm wat)
  #:use-module (ice-9 match)
  #:export ((compute-stdlib/memoized . compute-stdlib)))

(define (u32->s32 x)
  (centered-remainder x (ash 1 32)))

(define (fixnum-immediate i) `(ref.i31 (i32.const ,(ash i 1))))
(define min-fixnum (- (ash 1 29)))
(define max-fixnum (1- (ash 1 29)))

(define* (compute-stdlib import-abi? #:optional (max-struct-nfields 0))
  (define (maybe-import id)
    (if import-abi?
        `(,id (import "abi" ,(symbol->string id)))
        `(,id)))

  (define maybe-init-proc
    (if import-abi?
        '()
        '((struct.new $proc (i32.const 0)
                      (ref.func $invalid-continuation)))))
  (define maybe-init-i31-zero
    (if import-abi?
        '()
        '((ref.i31 (i32.const 0)))))
  (define maybe-init-i32-zero
    (if import-abi?
        '()
        '((i32.const 0))))
  (define maybe-init-hash-table
    (if import-abi?
        '()
        '((struct.new $hash-table (i32.const 0)
                      (i32.const 0)
                      (array.new $raw-scmvector (ref.i31 (i32.const 13))
                                 (i32.const 47))))))
  (define (struct-name nfields)
    (if (zero? nfields)
        '$struct
        (string->symbol (format #f "$struct/~a" nfields))))
  (define (struct-definition nfields)
    (define (field-name i) (string->symbol (format #f "$field~a" i)))
    `(struct
      (field $hash (mut i32))
      (field $vtable (mut (ref null $vtable)))
      ,@(map (lambda (i)
               `(field ,(field-name i) (mut (ref eq))))
             (iota nfields))))
  (define vtable-fields
    '((field $nfields (mut (ref eq)))
      (field $printer (mut (ref eq)))
      (field $name (mut (ref eq)))
      (field $constructor (mut (ref eq)))
      (field $properties (mut (ref eq)))
      (field $parents (mut (ref eq)))
      (field $mutable-fields (mut (ref eq)))
      (field $compare (mut (ref eq)))
      (field $applicable? (mut (ref eq)))))
  (define vtable-nfields (length vtable-fields))
  (define max-struct-nfields* (max max-struct-nfields vtable-nfields))

  (define no-source
    '((ref.null string)
      (i32.const 0)
      (i32.const 0)))
  (define propagate-source
    '((local.get $file)
      (local.get $line)
      (local.get $col)))

  (define (call-fmath fn . args)
    ;; FIXME: Most of these operations are specified to support complex
    ;; numbers, but we don't do that yet.
    `(struct.new $flonum
                 (i32.const 0)
                 (call ,fn
                       ,@(map (lambda (arg)
                                `(call $scm->f64 ,arg
                                       ,@propagate-source))
                              args))))

  (define* (dispatch-unary-numeric-operation
            op x #:key
            (source propagate-source)
            (s64? #f)
            (u64? #f)
            (exact-integer? (or s64? u64?))
            (real? exact-integer?)
            (check? #t)
            (subr #f)
            (prelude '())
            (expected-type (cond
                            (s64? "exact integer in s64 range")
                            (u64? "exact integer in u64 range")
                            (exact-integer? "exact integer")
                            (real? "real number")
                            (else "number")))
            (unpack-fixnum
             (if check?
                 `((if ,(let ((check '(i32.and (local.tee $tmp-i32 (i31.get_s))
                                               (i32.const 1))))
                          (if s64?
                              `(i32.or ,check
                                       (i32.lt_s (local.get $tmp-i32)
                                                 (i32.const 0)))
                              check))
                       (then (br $bad-operand (ref.i31 (local.get $tmp-i32)))))
                   (i32.shr_s (local.get $tmp-i32) (i32.const 1)))
                 `((i32.shr_s (i31.get_s) (i32.const 1)))))
            (unpack-bignum
             (if (and check? (or s64? u64?))
                 `((local.tee $tmp-bignum (struct.get $bignum $val))
                   (if (i32.eqz (call ,(if s64?
                                           '$bignum-is-i64
                                           '$bignum-is-u64)))
                       (then (br $bad-operand (local.get ,x))))
                   (local.get $tmp-bignum))
                 `((struct.get $bignum $val))))
            (unpack-flonum `((struct.get $flonum $val)))
            (unpack-fraction
             `((struct.get $fraction $num (local.tee $tmp-fraction))
               (struct.get $fraction $denom (local.get $tmp-fraction))))
            (unpack-complex
             `((struct.get $complex $real (local.tee $tmp-complex))
               (struct.get $complex $imag (local.get $tmp-complex))))
            (dispatch
             (lambda (type)
               `((return_call ,(symbol-append op '- type))))))
    (define (bad-operand-block body)
      (if check?
          `((block $bad-operand (ref eq) ,@body)
            (call $raise-type-error
                  (string.const ,subr)
                  (string.const ,expected-type)
                  (local.get ,x)
                  ,@source)
            (unreachable))
          body))
    (define (complex-block body)
      (if real?
          body
          `((block $complex (ref $complex) ,@body)
            ,@unpack-complex ,@(dispatch 'complex))))
    (define (fraction-block body)
      (if exact-integer?
          body
          `((block $fraction (ref $fraction) ,@body)
            ,@unpack-fraction ,@(dispatch 'fraction))))
    (define (flonum-block body)
      (if exact-integer?
          body
          `((block $flonum (ref $flonum) ,@body)
            ,@unpack-flonum ,@(dispatch 'flonum))))
    (define (bignum-block body)
      `((block $bignum (ref $bignum) ,@body)
        ,@unpack-bignum ,@(dispatch 'bignum)))
    (define (fixnum-block body)
      `((block $i31 (ref i31) ,@body)
        ,@unpack-fixnum ,@(dispatch 'fixnum)))
    (define (test-type type label) `(br_on_cast ,label (ref eq) (ref ,type)))
    (define (test-type/tail type label)
      (if check?
          `(,(test-type type label)
            (br $bad-operand))
          `((ref.cast ,type))))
    (define (type-checks types)
      (match types
        (((head-type head-label) ... (tail-type tail-label))
         (cons `(local.get ,x)
               (append (map test-type head-type head-label)
                       (test-type/tail tail-type tail-label ))))))
    (define (locals body)
      (define-syntax add-locals
        (syntax-rules ()
          ((_ body) body)
          ((_ body ((name type) test) . rest)
           (let ((locals (add-locals body . rest)))
             (if test
                 (cons '(local name type) locals)
                 locals)))))
      (add-locals
       body
       (($tmp-i32 i32)                  check?)
       (($tmp-bignum (ref extern))      (and check? (or s64? u64?)))
       (($tmp-fraction (ref $fraction)) (not exact-integer?))
       (($tmp-complex (ref $complex))   (not real?))))

    (locals
     (bad-operand-block
      (fixnum-block
       (bignum-block
        (flonum-block
         (fraction-block
          (complex-block
           (type-checks
            `((i31 $i31)
              ($bignum $bignum)
              ,@(if exact-integer? '() '(($flonum $flonum)))
              ,@(if exact-integer? '() '(($fraction $fraction)))
              ,@(if real? '() '(($complex $complex)))))))))))))

  (define* (dispatch-binary-numeric-operation
            op a b #:key
            (source propagate-source)
            (integer? #f)
            (exact? #f)
            (real? (or integer? exact?))
            (check? #t)
            (subr #f)
            (prelude '())
            (expected-type (cond
                            (integer? (if exact? "exact integer" "integer"))
                            (exact? "exact number")
                            (real? "real number")
                            (else "number")))
            (unpack-fixnum
             (if check?
                 `((if (i32.and (local.tee $tmp-i32 (i31.get_s))
                                (i32.const 1))
                       (then (br $bad-operand (ref.i31 (local.get $tmp-i32)))))
                   (i32.shr_s (local.get $tmp-i32) (i32.const 1)))
                 `((i32.shr_s (i31.get_s) (i32.const 1)))))
            (unpack-bignum `((struct.get $bignum $val)))
            (unpack-flonum
             (if (and integer? check?)
                 `((local.tee $tmp-flonum)
                   (if (i32.eqz
                        (call $f64-integer?
                              (local.tee $tmp-f64 (struct.get $flonum $val))))
                       (then (br $bad-operand (local.get $tmp-flonum))))
                   (local.get $tmp-f64))
                 `((struct.get $flonum $val))))
            (unpack-fraction
             `((struct.get $fraction $num (local.tee $tmp-fraction))
               (struct.get $fraction $denom (local.get $tmp-fraction))))
            (unpack-complex
             `((struct.get $complex $real (local.tee $tmp-complex))
               (struct.get $complex $imag (local.get $tmp-complex))))
            (unpacked-fixnum '(i32))
            (unpacked-bignum '((ref extern)))
            (unpacked-flonum '(f64))
            (unpacked-fraction '((ref eq) (ref eq)))
            (unpacked-complex '(f64 f64))
            (dispatch
             (lambda (a-type b-type)
               `((return_call ,(symbol-append op '- a-type '- b-type))))))
    (define (locals body)
      (define-syntax add-locals
        (syntax-rules ()
          ((_ body) body)
          ((_ body ((name type) test) . rest)
           (let ((locals (add-locals body . rest)))
             (if test
                 (cons '(local name type) locals)
                 locals)))))
      (add-locals
       body
       (($tmp-i32 i32)                  check?)
       (($tmp-f64 f64)                  (and integer? check? (not exact?)))
       (($tmp-flonum (ref $flonum))     (and integer? check? (not exact?)))
       (($tmp-scm (ref eq))             check?)
       (($tmp-fraction (ref $fraction)) (not integer?))
       (($tmp-complex (ref $complex))   (not real?))))
    (define (bad-operand-block body)
      (if check?
          `((block $bad-operand (ref eq) ,@body)
            (local.set $tmp-scm)
            (call $raise-type-error
                  (string.const ,subr)
                  (string.const ,expected-type)
                  (local.get $tmp-scm)
                  ,@source)
            (unreachable))
          body))
    (define (test-type type label) `(br_on_cast ,label (ref eq) (ref ,type)))
    (define (test-type/tail type label)
      (if check?
          `(,(test-type type label)
            (br $bad-operand))
          `((ref.cast ,type))))
    (define (type-checks x types)
      (match types
        (((head-type head-label) ... (tail-type tail-label))
         (cons `(local.get ,x)
               (append (map test-type head-type head-label)
                       (test-type/tail tail-type tail-label))))))

    (define (dispatch-b a-params a-type)
      (define (fixnum-block body)
        `((block $b-i31
                 (param ,@a-params) (result ,@a-params (ref i31))
                 ,@body)
          ,@unpack-fixnum
          ,@(dispatch a-type 'fixnum)))
      (define (bignum-block body)
        `((block $b-bignum
                 (param ,@a-params) (result ,@a-params (ref $bignum))
                 ,@body)
          ,@unpack-bignum
          ,@(dispatch a-type 'bignum)))
      (define (flonum-block body)
        (if exact?
            body
            `((block $b-flonum
                     (param ,@a-params) (result ,@a-params (ref $flonum))
                     ,@body)
              ,@unpack-flonum
              ,@(dispatch a-type 'flonum))))
      (define (fraction-block body)
        (if integer?
            body
            `((block $b-fraction
                     (param ,@a-params) (result ,@a-params (ref $fraction))
                      ,@body)
              ,@unpack-fraction
              ,@(dispatch a-type 'fraction))))
      (define (complex-block body)
        (if real?
            body
            `((block $b-complex
                     (param ,@a-params) (result ,@a-params (ref $complex))
                     ,@body)
              ,@unpack-complex
              ,@(dispatch a-type 'complex))))

      (fixnum-block
       (bignum-block
        (flonum-block
         (fraction-block
          (complex-block
           (type-checks b
                        `((i31 $b-i31)
                          ($bignum $b-bignum)
                          ,@(if exact? '() '(($flonum $b-flonum)))
                          ,@(if integer? '() '(($fraction $b-fraction)))
                          ,@(if real? '() '(($complex $b-complex)))))))))))

    (define (dispatch-a)
      (define (fixnum-block body)
        `((block $a-i31 (ref i31) ,@body)
          ,@unpack-fixnum
          ,@(dispatch-b unpacked-fixnum 'fixnum)))
      (define (bignum-block body)
        `((block $a-bignum (ref $bignum) ,@body)
          ,@unpack-bignum
          ,@(dispatch-b unpacked-bignum 'bignum)))
      (define (flonum-block body)
        (if exact?
            body
            `((block $a-flonum (ref $flonum) ,@body)
              ,@unpack-flonum
              ,@(dispatch-b unpacked-flonum 'flonum))))
      (define (fraction-block body)
        (if integer?
            body
            `((block $a-fraction (ref $fraction) ,@body)
              ,@unpack-fraction
              ,@(dispatch-b unpacked-fraction 'fraction))))
      (define (complex-block body)
        (if real?
            body
            `((block $a-complex (ref $complex) ,@body)
              ,@unpack-complex
              ,@(dispatch-b unpacked-complex 'complex))))

      (fixnum-block
       (bignum-block
        (flonum-block
         (fraction-block
          (complex-block
           (type-checks a
                        `((i31 $a-i31)
                          ($bignum $a-bignum)
                          ,@(if exact? '() '(($flonum $a-flonum)))
                          ,@(if integer? '() '(($fraction $a-fraction)))
                          ,@(if real? '() '(($complex $a-complex)))))))))))

    (locals (append prelude (bad-operand-block (dispatch-a)))))

  (define (commutative-arithmetic-operation-implementations op)
    `((func ,(symbol-append op '-bignum-fixnum)
            (param $a (ref extern)) (param $b i32)
            (result (ref eq))
            (return_call ,(symbol-append op '-fixnum-bignum)
                         (local.get $b) (local.get $a)))
      (func ,(symbol-append op '-flonum-fixnum)
            (param $a f64) (param $b i32)
            (result (ref eq))
            (return_call ,(symbol-append op '-fixnum-flonum)
                         (local.get $b) (local.get $a)))
      (func ,(symbol-append op '-flonum-bignum)
            (param $a f64) (param $b (ref extern))
            (result (ref eq))
            (return_call ,(symbol-append op '-bignum-flonum)
                         (local.get $b) (local.get $a)))
      (func ,(symbol-append op '-fraction-fixnum)
            (param $a-num (ref eq)) (param $a-denom (ref eq))
            (param $b i32)
            (result (ref eq))
            (return_call ,(symbol-append op '-fixnum-fraction)
                         (local.get $b)
                         (local.get $a-num) (local.get $a-denom)))
      (func ,(symbol-append op '-fraction-bignum)
            (param $a-num (ref eq)) (param $a-denom (ref eq))
            (param $b (ref extern))
            (result (ref eq))
            (return_call ,(symbol-append op '-bignum-fraction)
                         (local.get $b)
                         (local.get $a-num) (local.get $a-denom)))
      (func ,(symbol-append op '-fraction-flonum)
            (param $a-num (ref eq)) (param $a-denom (ref eq))
            (param $b f64)
            (result (ref eq))
            (return_call ,(symbol-append op '-flonum-fraction)
                         (local.get $b)
                         (local.get $a-num) (local.get $a-denom)))
      (func ,(symbol-append op '-complex-fixnum)
            (param $a-real f64) (param $a-imag f64)
            (param $b i32)
            (result (ref eq))
            (return_call ,(symbol-append op '-fixnum-complex)
                         (local.get $b)
                         (local.get $a-real) (local.get $a-imag)))
      (func ,(symbol-append op '-complex-bignum)
            (param $a-real f64) (param $a-imag f64)
            (param $b (ref extern))
            (result (ref eq))
            (return_call ,(symbol-append op '-bignum-complex)
                         (local.get $b)
                         (local.get $a-real) (local.get $a-imag)))
      (func ,(symbol-append op '-complex-flonum)
            (param $a-real f64) (param $a-imag f64)
            (param $b f64)
            (result (ref eq))
            (return_call ,(symbol-append op '-flonum-complex)
                         (local.get $b)
                         (local.get $a-real) (local.get $a-imag)))
      (func ,(symbol-append op '-complex-fraction)
            (param $a-real f64) (param $a-imag f64)
            (param $b-num (ref eq)) (param $b-denom (ref eq))
            (result (ref eq))
            (return_call ,(symbol-append op '-fraction-complex)
                         (local.get $b-num) (local.get $b-denom)
                         (local.get $a-real) (local.get $a-imag)))))

  (wat->wasm
   `((type $kvarargs
           (func (param $nargs i32)
                 (param $arg0 (ref eq))
                 (param $arg1 (ref eq))
                 (param $arg2 (ref eq))))

     (type $raw-bitvector (array (mut i32)))
     (type $raw-bytevector (array (mut i8)))
     (type $raw-scmvector (array (mut (ref eq))))

     (rec
      (type $heap-object
            (sub
             (struct
              (field $hash (mut i32)))))

      (type $extern-ref
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (ref null extern)))))
      (type $code-ref
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (ref func)))))

      (type $heap-number
            (sub $heap-object
              (struct
               (field $hash (mut i32)))))
      (type $bignum
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $val (ref extern)))))
      (type $flonum
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $val f64))))
      (type $complex
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $real f64)
               (field $imag f64))))
      (type $fraction
            (sub $heap-number
              (struct
               (field $hash (mut i32))
               (field $num (ref eq))
               (field $denom (ref eq)))))

      (type $pair
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $car (mut (ref eq)))
               (field $cdr (mut (ref eq))))))
      (type $mutable-pair
            (sub $pair
              (struct
               (field $hash (mut i32))
               (field $car (mut (ref eq)))
               (field $cdr (mut (ref eq))))))
      (type $vector
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-scmvector)))))
      (type $mutable-vector
            (sub $vector
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-scmvector)))))
      (type $bytevector
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-bytevector)))))
      (type $mutable-bytevector
            (sub $bytevector
              (struct
               (field $hash (mut i32))
               (field $vals (ref $raw-bytevector)))))
      (type $bitvector
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $len i32)
               (field $vals (ref $raw-bitvector)))))
      (type $mutable-bitvector
            (sub $bitvector
              (struct
               (field $hash (mut i32))
               (field $len i32)
               (field $vals (ref $raw-bitvector)))))
      (type $string
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $str (mut (ref string))))))
      (type $mutable-string
            (sub $string
              (struct
               (field $hash (mut i32))
               (field $str (mut (ref string))))))
      (type $proc
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $func (ref $kvarargs)))))
      (type $symbol
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $name (ref $string)))))
      (type $keyword
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $name (ref $symbol)))))
      (type $variable
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (mut (ref eq))))))
      (type $atomic-box
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (mut (ref eq))))))
      (type $hash-table
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $size (mut i32))
               (field $buckets (ref $raw-scmvector)))))
      (type $weak-table
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (ref extern)))))
      (type $fluid
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $init (ref eq)))))
      (type $dynamic-state
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $fluids (ref $hash-table)))))
      (type $syntax
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $expr (ref eq))
               (field $wrap (ref eq))
               (field $module (ref eq))
               (field $source (ref eq)))))
      (type $syntax-transformer
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $type (ref eq))
               (field $value (ref eq)))))
      (type $port
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $open? (mut (ref eq)))  ;; #f | #t
               (field $read (ref eq))         ;; #f | (bv, start, count) -> size
               (field $write (ref eq))        ;; #f | (bv, start, count) -> size
               (field $input-waiting? (ref eq))   ;; #f | () -> bool
               (field $seek (ref eq))         ;; #f | (offset, whence) -> offset
               (field $close (ref eq))        ;; #f | () -> ()
               (field $truncate (ref eq))     ;; #f | (length) -> ()
               (field $repr (ref $string))
               (field $filename (mut (ref eq)))      ;; #f | string
               (field $position (ref $mutable-pair))  ;; (line . column)
               (field $read-buf (mut (ref eq)))   ;; #f | #(bv cur end has-eof?)
               (field $write-buf (mut (ref eq)))  ;; #f | #(bv cur end)
               (field $read-buffering (mut (ref eq))) ;; #f | [1,size,1<<29)
               (field $r/w-random-access? (ref eq))   ;; #f | #t
               (field $fold-case? (mut (ref eq)))     ;; #f | #t
               (field $private-data (ref eq)))))      ;; whatever
      (type $struct
            (sub $heap-object
              (struct
               (field $hash (mut i32))
               ;; Vtable link is mutable so that we can tie the knot for top
               ;; types.
               (field $vtable (mut (ref null $vtable))))))
      ,@(map (lambda (nfields)
               `(type ,(struct-name nfields)
                      (sub ,(struct-name (1- nfields))
                           ,(struct-definition nfields))))
             (iota vtable-nfields 1))
      (type $vtable
            (sub ,(struct-name vtable-nfields)
                 (struct
                  (field $hash (mut i32))
                  (field $vtable (mut (ref null $vtable)))
                  ,@vtable-fields)))
      (type $vtable-vtable
            (sub $vtable
                 (struct
                  (field $hash (mut i32))
                  (field $vtable (mut (ref null $vtable)))
                  ,@vtable-fields)))
      (type $parameter
            (sub $proc
              (struct
               (field $hash (mut i32))
               (field $func (ref $kvarargs))
               (field $fluid (ref $fluid))
               (field $convert (ref $proc)))))

      (type $dyn (sub (struct)))
      (type $dynwind
            (sub $dyn
              (struct
               (field $wind (ref $proc))
               (field $unwind (ref $proc)))))
      (type $dynprompt
            (sub $dyn
              (struct
               (field $raw-sp i32)
               (field $scm-sp i32)
               (field $ret-sp i32)
               (field $unwind-only? i8)
               (field $tag (ref eq))
               (field $handler (ref $kvarargs)))))
      (type $dynfluid
            (sub $dyn
              (struct
               (field $fluid (ref $fluid))
               (field $val (mut (ref eq))))))
      (type $dynstate
            (sub $dyn
              (struct
               (field $fluids (mut (ref $hash-table)))))))

     (type $raw-retvector (array (mut (ref $kvarargs))))
     (type $raw-dynvector (array (mut (ref $dyn))))
     (type $cont
           (sub $proc
             (struct
              (field $hash (mut i32))
              (field $func (ref $kvarargs))
              (field $prompt (ref $dynprompt))
              (field $raw-stack (ref $raw-bytevector))
              (field $scm-stack (ref $raw-scmvector))
              (field $ret-stack (ref $raw-retvector))
              (field $dyn-stack (ref $raw-dynvector)))))

     (global $root-vtable (ref $vtable-vtable) (call $make-root-vtable))

     (global $empty-vector (ref $vector)
             (struct.new $vector
                         (i32.const 0) (array.new_fixed $raw-scmvector 0)))
     (func $make-root-vtable (result (ref $vtable-vtable))
           (local $ret (ref $vtable-vtable))
           (local.set $ret
                      (struct.new $vtable-vtable
                                  (i32.const 0)
                                  (ref.null $vtable)
                                  (ref.i31 (i32.const ,(ash vtable-nfields 1)))
                                  (ref.i31 (i32.const 1)) ; printer
                                  (ref.i31 (i32.const 1)) ; name
                                  (ref.i31 (i32.const 1)) ; constructor
                                  (ref.i31 (i32.const 13)) ; properties
                                  (global.get $empty-vector) ; parents
                                  (ref.i31 (i32.const 0)) ; mutable-fields
                                  (ref.i31 (i32.const 0)) ; compare
                                  (ref.i31 (i32.const 1)))) ; applicable?
           (struct.set $vtable-vtable $vtable (local.get $ret) (local.get $ret))
           ;; Rely on Scheme to initialize printer, name, etc...
           (local.get $ret))

     (func $struct-ref (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $val (ref eq))
           ;; Satisfy the validator by setting a default value.
           (local.set $val (ref.i31 (i32.const 1)))
           (if (call $fixnum? (local.get $arg2))
               (then
                ;; This is pretty gnarly, but we need to pick the
                ;; right struct type to cast to based on the field
                ;; index.
                (block $done (ref eq)
                       (block $out-of-bounds (ref eq)
                              ,@(let lp ((i 0))
                                  (define (block-name nfields)
                                    (string->symbol (format #f "$ref-field~a" nfields)))
                                  (if (= i max-struct-nfields*)
                                      `((local.get $arg1)
                                        (i32.shr_s (i31.get_s
                                                    (ref.cast i31 (local.get $arg2)))
                                                   (i32.const 1))
                                        (br_table ,@(map block-name
                                                         (iota max-struct-nfields*))
                                                  $out-of-bounds)
                                        (unreachable))
                                      `((block ,(block-name i) (ref eq)
                                               ,@(lp (1+ i)))
                                        (br_on_cast_fail $out-of-bounds (ref eq)
                                                         (ref ,(struct-name (1+ i))))
                                        (struct.get ,(struct-name (1+ i)) ,(+ i 2))
                                        (br $done)))))
                       (drop)
                       (call $raise-range-error
                             (string.const "struct-ref")
                             (local.get $arg2)
                             ,@no-source)
                       (unreachable))
                (local.set $val))
               (else
                (call $raise-type-error
                      (string.const "struct-ref")
                      (string.const "idx")
                      (local.get $arg2)
                      ,@no-source)
                (unreachable)))
           (i32.const 1)
           (local.get $val)
           (ref.i31 (i32.const 1))
           (ref.i31 (i32.const 1))
           (global.set $ret-sp (i32.sub (global.get $ret-sp) (i32.const 1)))
           (global.get $ret-sp)
           (table.get $ret-stack)
           (return_call_ref $kvarargs))
     (global $struct-ref-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $struct-ref)))

     (func $raise-exception (param $exn (ref eq))
           (return_call_ref $kvarargs
                            (i32.const 2)
                            (global.get $raise-exception)
                            (local.get $exn)
                            (ref.i31 (i32.const 1))
                            (struct.get $proc $func (global.get $raise-exception))))

     (func $raise-returned-value
           (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (if (i32.ne (local.get $nargs) (i32.const 1))
               (then (call $die0
                           (string.const "unexpected raise-exception return"))))
           (return_call $raise-exception (local.get $arg0)))

     (func $push-raise-returned-value
           (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
           (call $maybe-grow-ret-stack)
           (table.set $ret-stack
                      (i32.sub (global.get $ret-sp) (i32.const 1))
                      (ref.func $raise-returned-value)))

     (func $annotate-exception (param $exn (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (return_call_ref $kvarargs
                            (i32.const 5)
                            (global.get $annotate-with-source)
                            (local.get $exn)
                            (if (ref eq)
                                (ref.is_null (local.get $file))
                                (then (ref.i31 (i32.const 1)))
                                (else (struct.new
                                       $string (i32.const 0)
                                       (ref.as_non_null (local.get $file)))))
                            (global.set $arg3
                                        (call $i32->fixnum (local.get $line)))
                            (global.set $arg4
                                        (call $i32->fixnum (local.get $col)))
                            (struct.get $proc $func
                                        (global.get $annotate-with-source))))

     (func $raise-exception-with-annotation (param $exn (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           ;; FIXME: Annotate the exception.
           (call $push-raise-returned-value)
           (return_call $annotate-exception (local.get $exn)
                        (local.get $file) (local.get $line) (local.get $col)))

     (global $exception-source-file (mut (ref null string)) (ref.null string))
     (global $exception-source-line (mut i32) (i32.const 0))
     (global $exception-source-column (mut i32) (i32.const 0))
     (func $annotate-returned-exception
           (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (if (i32.ne (local.get $nargs) (i32.const 1))
               (then (call $die0
                           (string.const "unexpected make-exception return"))))
           (return_call $annotate-exception
                        (local.get $arg0)
                        (global.get $exception-source-file)
                        (global.get $exception-source-line)
                        (global.get $exception-source-column)))

     (func $push-annotate-returned-exception
           (param $file (ref null string)) (param $line i32) (param $col i32)
           ;; If line is 0, there is no source; nothing to do..
           (if (local.get $line)
               (then
                (global.set $exception-source-file (local.get $file))
                (global.set $exception-source-line (local.get $line))
                (global.set $exception-source-column (local.get $col))
                (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
                (call $maybe-grow-ret-stack)
                (table.set $ret-stack
                           (i32.sub (global.get $ret-sp) (i32.const 1))
                           (ref.func $annotate-returned-exception)))))

     (func $push-annotate-and-raise-returned-exception
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-raise-returned-value)
           (call $push-annotate-returned-exception
                 (local.get $file) (local.get $line) (local.get $col)))

     ;; FIXME: In all of these, we need to plumb $file, $line, and $col
     ;; to $annotate-with-source.
     (func $raise-type-error
           (param $subr (ref string))
           (param $what (ref string))
           (param $val (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (global.set $arg3 (struct.new $string (i32.const 0)
                                         (local.get $what)))
           (return_call_ref $kvarargs
                            (i32.const 4)
                            (global.get $make-type-error)
                            (local.get $val)
                            (struct.new $string (i32.const 0)
                                        (local.get $subr))
                            (struct.get $proc $func
                                        (global.get $make-type-error))))

     (func $raise-range-error
           (param $subr (ref string))
           (param $val (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (global.set $arg3 (ref.i31 (i32.const 1)))
           (global.set $arg4 (local.get $val))
           (return_call_ref $kvarargs
                            (i32.const 5)
                            (global.get $make-range-error)
                            (local.get $val)
                            (ref.i31 (i32.const 1))
                            (struct.get $proc $func
                                        (global.get $make-range-error))))

     (func $raise-arity-error
           (param $subr (ref null string))
           (param $val (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (return_call_ref $kvarargs
                            (i32.const 3)
                            (global.get $make-arity-error)
                            (local.get $val)
                            (if (ref eq)
                                (ref.is_null (local.get $subr))
                                (then (ref.i31 (i32.const 1)))
                                (else (struct.new $string (i32.const 0)
                                                  (ref.as_non_null
                                                   (local.get $subr)))))
                            (struct.get $proc $func
                                        (global.get $make-arity-error))))

     (func $raise-invalid-keyword-error (param $kw (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (return_call_ref
            $kvarargs
            (i32.const 2)
            (global.get $make-invalid-keyword-error)
            (local.get $kw)
            (ref.i31 (i32.const 1))
            (struct.get $proc $func
                        (global.get $make-invalid-keyword-error)))
           (unreachable))

     (func $raise-unrecognized-keyword-error (param $kw (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (return_call_ref
            $kvarargs
            (i32.const 2)
            (global.get $make-unrecognized-keyword-error)
            (local.get $kw)
            (ref.i31 (i32.const 1))
            (struct.get $proc $func
                        (global.get $make-unrecognized-keyword-error)))
           (unreachable))

     (func $raise-missing-keyword-argument-error (param $kw (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (return_call_ref
            $kvarargs
            (i32.const 2)
            (global.get $make-missing-keyword-argument-error)
            (local.get $kw)
            (ref.i31 (i32.const 1))
            (struct.get $proc $func
                        (global.get $make-missing-keyword-argument-error)))
           (unreachable))

     (func $raise-runtime-error-with-message
           (param $message (ref string))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (return_call_ref $kvarargs
                            (i32.const 2)
                            (global.get $make-runtime-error-with-message)
                            (struct.new $string
                                        (i32.const 0)
                                        (local.get $message))
                            (ref.i31 (i32.const 1))
                            (struct.get $proc $func
                                        (global.get $make-runtime-error-with-message))))

     (func $raise-runtime-error-with-message+irritants
           (param $message (ref string))
           (param $irritants (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (call $push-annotate-and-raise-returned-exception
                 (local.get $file) (local.get $line) (local.get $col))
           (return_call_ref $kvarargs
                            (i32.const 3)
                            (global.get $make-runtime-error-with-message+irritants)
                            (struct.new $string
                                        (i32.const 0)
                                        (local.get $message))
                            (local.get $irritants)
                            (struct.get $proc $func
                                        (global.get $make-runtime-error-with-message+irritants))))

     (func $string->bignum (import "rt" "bignum_from_string")
           (param (ref string))
           (result (ref extern)))
     (func $bignum-from-i32 (import "rt" "bignum_from_i32")
           (param i32)
           (result (ref extern)))
     (func $bignum-from-i64 (import "rt" "bignum_from_i64")
           (param i64)
           (result (ref extern)))
     (func $bignum-from-u64 (import "rt" "bignum_from_u64")
           (param i64)
           (result (ref extern)))
     (func $bignum-is-i64 (import "rt" "bignum_is_i64")
           (param (ref extern))
           (result i32))
     (func $bignum-is-u64 (import "rt" "bignum_is_u64")
           (param (ref extern))
           (result i32))
     (func $bignum-get-i64 (import "rt" "bignum_get_i64")
           (param (ref extern))
           (result i64))

     (func $bignum-add (import "rt" "bignum_add")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-add-i32 (import "rt" "bignum_add")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-sub (import "rt" "bignum_sub")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-sub-i32 (import "rt" "bignum_sub")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-mul (import "rt" "bignum_mul")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-mul-i32 (import "rt" "bignum_mul")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-lsh (import "rt" "bignum_lsh")
           (param (ref extern))
           (param i64)
           (result (ref extern)))
     (func $i32-lsh (import "rt" "bignum_lsh")
           (param i32)
           (param i64)
           (result (ref extern)))
     (func $bignum-rsh (import "rt" "bignum_rsh")
           (param (ref extern))
           (param i64)
           (result (ref extern)))
     (func $bignum-quo (import "rt" "bignum_quo")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-rem (import "rt" "bignum_rem")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-mod (import "rt" "bignum_mod")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-gcd (import "rt" "bignum_gcd")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))

     (func $bignum-logand-i32 (import "rt" "bignum_logand")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logand-bignum (import "rt" "bignum_logand")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-logior-i32 (import "rt" "bignum_logior")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logior-bignum (import "rt" "bignum_logior")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-logxor-i32 (import "rt" "bignum_logxor")
           (param (ref extern))
           (param i32)
           (result (ref extern)))
     (func $bignum-logxor-bignum (import "rt" "bignum_logxor")
           (param (ref extern))
           (param (ref extern))
           (result (ref extern)))
     (func $bignum-logcount (import "rt" "bignum_logcount")
           (param (ref extern))
           (result i32))

     (func $lt-fix-big (import "rt" "bignum_lt")
           (param i32)
           (param (ref extern))
           (result i32))
     (func $lt-big-fix (import "rt" "bignum_lt")
           (param (ref extern))
           (param i32)
           (result i32))
     (func $lt-big-big (import "rt" "bignum_lt")
           (param (ref extern))
           (param (ref extern))
           (result i32))
     (func $lt-big-flo (import "rt" "bignum_lt")
           (param (ref extern))
           (param f64)
           (result i32))
     (func $lt-flo-big (import "rt" "bignum_lt")
           (param f64)
           (param (ref extern))
           (result i32))

     (func $le-big-flo (import "rt" "bignum_le")
           (param (ref extern))
           (param f64)
           (result i32))
     (func $le-flo-big (import "rt" "bignum_le")
           (param f64)
           (param (ref extern))
           (result i32))

     (func $eq-big-big (import "rt" "bignum_eq")
           (param (ref extern))
           (param (ref extern))
           (result i32))
     (func $eq-big-flo (import "rt" "bignum_eq")
           (param (ref extern))
           (param f64)
           (result i32))

     (func $bignum->f64 (import "rt" "bignum_to_f64")
           (param (ref extern))
           (result f64))

     (func $f64-is-nan (param $x f64) (result i32)
           (i32.eqz (f64.eq (local.get $x) (local.get $x))))

     ,@(let ((sign-bit (ash -1 63))
             (exponent-bits (ash (1- (ash 1 11)) 52)))
         ;; An f64 has 1 sign bit, 11 exponent bits, and 52 signicand
         ;; bits.  If all exponent bits are set, it is an infinity if
         ;; all significand bits are 0, otherwise it is a nan.
         `((func $f64-is-infinite (param $x f64) (result i32)
                 (i64.eq
                  (i64.const ,exponent-bits)
                  (i64.and (i64.const ,(lognot sign-bit))
                           (i64.reinterpret_f64 (local.get $x)))))
           (func $f64-is-finite (param $x f64) (result i32)
                 (i64.ne
                  (i64.const ,exponent-bits)
                  (i64.and (i64.reinterpret_f64 (local.get $x))
                           (i64.const ,exponent-bits))))))

     (func $flonum->string (import "rt" "flonum_to_string")
           (param f64)
           (result (ref string)))

     (func $string-upcase (import "rt" "string_upcase")
           (param (ref string))
           (result (ref string)))
     (func $string-downcase (import "rt" "string_downcase")
           (param (ref string))
           (result (ref string)))

     (func $make-weak-map (import "rt" "make_weak_map")
           (result (ref extern)))
     (func $weak-map-get (import "rt" "weak_map_get")
           (param (ref extern) (ref eq) (ref eq))
           (result (ref eq)))
     (func $weak-map-set (import "rt" "weak_map_set")
           (param (ref extern) (ref eq) (ref eq)))
     (func $weak-map-delete (import "rt" "weak_map_delete")
           (param (ref extern) (ref eq))
           (result i32))

     ;; FIXME: These are very much temporary.
     (func $write-stdout (import "io" "write_stdout") (param (ref string)))
     (func $write-stderr (import "io" "write_stderr") (param (ref string)))
     (func $read-stdin (import "io" "read_stdin") (result (ref string)))
     (func $file-exists? (import "io" "file_exists")
           (param (ref string)) (result i32))
     (func $open-input-file (import "io" "open_input_file")
           (param (ref string)) (result (ref extern)))
     (func $open-output-file (import "io" "open_output_file")
           (param (ref string)) (result (ref extern)))
     (func $close-file (import "io" "close_file") (param (ref extern)))
     (func $read-file (import "io" "read_file")
           (param (ref extern)) (param i32) (result i32))
     (func $write-file (import "io" "write_file")
           (param (ref extern)) (param i32) (result i32))
     (func $seek-file (import "io" "seek_file")
           (param (ref extern)) (param i32) (param i32) (result i32))
     (func $file-random-access? (import "io" "file_random_access")
           (param (ref extern)) (result i32))
     (func $file-buffer-size (import "io" "file_buffer_size")
           (param (ref extern)) (result i32))
     (func $file-buffer-ref (import "io" "file_buffer_ref")
           (param (ref extern)) (param i32) (result i32))
     (func $file-buffer-set! (import "io" "file_buffer_set")
           (param (ref extern)) (param i32) (param i32))
     (func $delete-file (import "io" "delete_file") (param (ref string)))

     (func $fsqrt (import "rt" "fsqrt") (param f64) (result f64))
     (func $fsin (import "rt" "fsin") (param f64) (result f64))
     (func $fcos (import "rt" "fcos") (param f64) (result f64))
     (func $ftan (import "rt" "ftan") (param f64) (result f64))
     (func $fasin (import "rt" "fasin") (param f64) (result f64))
     (func $facos (import "rt" "facos") (param f64) (result f64))
     (func $fatan (import "rt" "fatan") (param f64) (result f64))
     (func $fatan2 (import "rt" "fatan2") (param f64 f64) (result f64))
     (func $flog (import "rt" "flog") (param f64) (result f64))
     (func $fexp (import "rt" "fexp") (param f64) (result f64))

     (func $jiffies-per-second (import "rt" "jiffies_per_second") (result i32))
     (func $current-jiffy (import "rt" "current_jiffy") (result f64))
     (func $current-second (import "rt" "current_second") (result f64))

     (func $die (import "rt" "die")
           (param (ref string) (ref eq)))
     (func $quit (import "rt" "quit") (param i32))

     (func $debug-str (import "debug" "debug_str")
           (param (ref string)))
     (func $debug-str-i32 (import "debug" "debug_str_i32")
           (param (ref string) i32))
     (func $debug-str-scm (import "debug" "debug_str_scm")
           (param (ref string) (ref eq)))
     (func $code-name (import "debug" "code_name")
           (param (ref func))
           (result (ref null string)))
     (func $code-source (import "debug" "code_source")
           (param (ref func))
           (result (ref null string) i32 i32))

     (func $extern-func? (import "ffi" "is_extern_func")
           (param (ref extern)) (result i32))
     (func $call-extern (import "ffi" "call_extern")
           (param (ref extern)) (param (ref eq)) (result (ref eq)))
     (func $procedure->extern (import "ffi" "procedure_to_extern")
           (param (ref eq)) (result (ref extern)))

     (func $die0 (param $reason (ref string))
           (call $die (local.get 0) (ref.i31 (i32.const 1))))

     ;; Thomas Wang's integer hasher, from
     ;; http://www.cris.com/~Ttwang/tech/inthash.htm.
     (func $integer-hash (param $v i32) (result i32)
           (local.set $v (i32.xor (i32.xor (local.get $v) (i32.const 61))
                                  (i32.shr_u (local.get $v) (i32.const 16))))
           (local.set $v (i32.add (local.get $v)
                                  (i32.shl (local.get $v) (i32.const 3))))
           (local.set $v (i32.xor (local.get $v)
                                  (i32.shr_u (local.get $v) (i32.const 4))))
           (local.set $v (i32.mul (local.get $v)
                                  (i32.const #x27d4eb2d)))
           (i32.xor (local.get $v)
                    (i32.shr_u (local.get $v) (i32.const 15))))

     (func $finish-heap-object-hash (param $hash i32) (result i32)
           (local.set $hash (call $integer-hash (local.get $hash)))
           (if i32 (local.get $hash)
               (then (local.get $hash))
               (else (call $integer-hash (i32.const 42)))))

     (global $hashq-counter (mut i32) (i32.const 0))
     (func $immediate-hashq (param $v (ref i31)) (result i32)
           (call $integer-hash (i31.get_u (local.get $v))))
     (func $heap-object-hashq (param $v (ref $heap-object)) (result i32)
           (local $tag i32)
           (local.set $tag (struct.get $heap-object $hash (local.get $v)))
           (loop $init-if-zero
             (block
              $done
              (br_if $done (local.get $tag))
              (global.set $hashq-counter
                          (i32.sub (global.get $hashq-counter) (i32.const 1)))
              (struct.set $heap-object $hash (local.get $v)
                          (local.tee $tag (call $integer-hash
                                                (global.get $hashq-counter))))
              ;; Check and retry if result is zero.
              (br $init-if-zero)))
           (local.get $tag))
     (func $hashq (param $v (ref eq)) (result i32)
           (if i32
               (ref.test i31 (local.get $v))
               (then
                (return_call $immediate-hashq
                             (ref.cast i31 (local.get $v))))
               (else
                (return_call $heap-object-hashq
                             (ref.cast $heap-object (local.get $v))))))

     ;; 32-bit murmur3 hashing function ported from C and specialized
     ;; for both bytevectors and bitvectors.
     (func $hash-bytevector (param $bv (ref $bytevector)) (result i32)
           (local $raw (ref $raw-bytevector))
           (local $len i32)
           (local $i i32)
           (local $h1 i32)
           (local.set $raw (struct.get $bytevector $vals (local.get $bv)))
           (local.set $len (array.len (local.get $raw)))
           (local.set $i (i32.const 4))
           (local.set $h1 (i32.const ,(u32->s32 #x9ad4da4)))
           ;; Hash most (potentially all) of the bytevector contents 4
           ;; bytes at a time.
           (loop $loop
                 (block $done
                        (br_if $done (i32.gt_s (local.get $i) (local.get $len)))
                        ;; Sigh, we can't directly read i32s from an
                        ;; (array i8) so we read 4 separate bytes and
                        ;; shift them.
                        (array.get_u $raw-bytevector
                                     (local.get $raw)
                                     (i32.sub (local.get $i) (i32.const 4)))
                        (i32.shl (array.get_u $raw-bytevector
                                              (local.get $raw)
                                              (i32.sub (local.get $i) (i32.const 3)))
                                 (i32.const 8))
                        (i32.or)
                        (i32.shl (array.get_u $raw-bytevector
                                              (local.get $raw)
                                              (i32.sub (local.get $i) (i32.const 2)))
                                 (i32.const 16))
                        (i32.or)
                        (i32.shl (array.get_u $raw-bytevector
                                              (local.get $raw)
                                              (i32.sub (local.get $i) (i32.const 1)))
                                 (i32.const 24))
                        (i32.or)
                        ;; Combine with hash from last iteration.
                        (i32.const ,(u32->s32 #xcc9e2d51))
                        (i32.mul)
                        (i32.const 15)
                        (i32.rotl)
                        (i32.const ,(u32->s32 #x1b873593))
                        (i32.mul)
                        (local.get $h1)
                        (i32.xor)
                        (i32.const 13)
                        (i32.rotl)
                        (i32.const 5)
                        (i32.mul)
                        (i32.const ,(u32->s32 #xe6546b64))
                        (i32.add)
                        (local.set $h1)
                        (local.set $i (i32.add (local.get $i) (i32.const 4)))
                        (br $loop)))
           ;; Handle the remaining 1-3 bytes when length isn't
           ;; divisible by 4.  Inner blocks fall through to the outer
           ;; blocks.
           (i32.const 0)
           (block $done (param i32) (result i32)
                  (block $1-byte (param i32) (result i32)
                         (block $2-bytes (param i32) (result i32)
                                (block $3-bytes (param i32) (result i32)
                                       (block (param i32) (result i32)
                                              (i32.and (local.get $len) (i32.const 3))
                                              (br_table $done $1-byte $2-bytes $3-bytes $done)
                                              (unreachable)))
                                (array.get_u $raw-bytevector
                                             (local.get $raw)
                                             (i32.sub (local.get $i) (i32.const 2)))
                                (i32.const 16)
                                (i32.shl)
                                (i32.xor))
                         (array.get_u $raw-bytevector
                                      (local.get $raw)
                                      (i32.sub (local.get $i) (i32.const 3)))
                         (i32.const 8)
                         (i32.shl)
                         (i32.xor))
                  (array.get_u $raw-bytevector
                               (local.get $raw)
                               (i32.sub (local.get $i) (i32.const 4)))
                  (i32.xor)
                  (i32.const ,(u32->s32 #xcc9e2d51))
                  (i32.mul)
                  (i32.const 15)
                  (i32.rotl)
                  (i32.const ,(u32->s32 #x1b873593))
                  (i32.mul))
           (local.get $h1)
           (i32.xor)
           (local.set $h1)
           ;; Finalize by incorporating bytevector length and mixing.
           (local.set $h1 (i32.xor
                           (local.get $h1)
                           (array.len (local.get $raw))))
           (local.set $h1 (i32.mul
                           (i32.xor
                            (local.get $h1)
                            (i32.shr_u (local.get $h1) (i32.const 16)))
                           (i32.const ,(u32->s32 #x85ebca6b))))
           (local.set $h1 (i32.mul
                           (i32.xor
                            (local.get $h1)
                            (i32.shr_u (local.get $h1) (i32.const 13)))
                           (i32.const ,(u32->s32 #xc2b2ae35))))
           (i32.xor (local.get $h1)
                    (i32.shr_u (local.get $h1) (i32.const 16))))

     (func $hash-bitvector (param $bv (ref $bitvector)) (result i32)
           (local $raw (ref $raw-bitvector))
           (local $len i32)
           (local $i i32)
           (local $h1 i32)
           (local.set $raw (struct.get $bitvector $vals (local.get $bv)))
           (local.set $len (array.len (local.get $raw)))
           (local.set $i (i32.const 0))
           (local.set $h1 (i32.const ,(u32->s32 #xa305e51)))
           ;; Hash bitvector contents.
           (loop $loop
                 (block $done
                        (br_if $done (i32.eq (local.get $i) (local.get $len)))
                        (array.get $raw-bitvector
                                   (local.get $raw)
                                   (local.get $i))
                        (i32.const ,(u32->s32 #xcc9e2d51))
                        (i32.mul)
                        (i32.const 15)
                        (i32.rotl)
                        (i32.const ,(u32->s32 #x1b873593))
                        (i32.mul)
                        (local.get $h1)
                        (i32.xor)
                        (i32.const 13)
                        (i32.rotl)
                        (i32.const 5)
                        (i32.mul)
                        (i32.const ,(u32->s32 #xe6546b64))
                        (i32.add)
                        (local.set $h1)
                        (local.set $i (i32.add (local.get $i) (i32.const 1)))
                        (br $loop)))
           ;; Finalize by incorporating bitvector length and mixing.
           (local.set $h1 (i32.xor
                           (local.get $h1)
                           (struct.get $bitvector $len (local.get $bv))))
           (local.set $h1 (i32.mul
                           (i32.xor
                            (local.get $h1)
                            (i32.shr_u (local.get $h1) (i32.const 16)))
                           (i32.const ,(u32->s32 #x85ebca6b))))
           (local.set $h1 (i32.mul
                           (i32.xor
                            (local.get $h1)
                            (i32.shr_u (local.get $h1) (i32.const 13)))
                           (i32.const ,(u32->s32 #xc2b2ae35))))
           (i32.xor (local.get $h1)
                    (i32.shr_u (local.get $h1) (i32.const 16))))

     (func $grow-raw-stack
           ;; Grow the stack by at least 50% and at least the needed
           ;; space.  Trap if we fail to grow.
           ;; additional_size = (current_size >> 1) | needed_size
           (if (i32.eq
                (memory.grow
                 $raw-stack
                 (i32.or (i32.shr_u (memory.size $raw-stack) (i32.const 1))
                         ;; Wasm pages are 64 kB.
                         (i32.sub (i32.add (i32.shr_u (global.get $raw-sp)
                                                      (i32.const 16))
                                           (i32.const 1))
                                  (memory.size $raw-stack))))
                (i32.const -1))
               (then (call $die0 (string.const "$grow-raw-stack")) (unreachable))))
     (func $maybe-grow-raw-stack
           (if (i32.lt_u (i32.shl (memory.size $raw-stack) (i32.const 16))
                         (global.get $raw-sp))
               (then (call $grow-raw-stack))))

     (func $grow-scm-stack
           ;; Grow as in $grow-raw-stack.
           (if (i32.eq
                (table.grow $scm-stack
                            (ref.i31 (i32.const 0))
                            (i32.or (i32.shr_u (table.size $scm-stack)
                                               (i32.const 1))
                                    (i32.sub (global.get $scm-sp)
                                             (table.size $scm-stack))))
                (i32.const -1))
               (then
                (call $die0 (string.const "$grow-scm-stack"))
                (unreachable))))
     (func $maybe-grow-scm-stack
           (if (i32.lt_u (table.size $scm-stack) (global.get $scm-sp))
               (then (call $grow-scm-stack))))

     (func $invalid-continuation (type $kvarargs)
           (call $die0 (string.const "$invalid-continuation"))
           (unreachable))
     (func $grow-ret-stack
           ;; Grow as in $grow-raw-stack.
           (if (i32.eq (table.grow $ret-stack
                                   (ref.func $invalid-continuation)
                                   (i32.or (i32.shr_u (table.size $ret-stack)
                                                      (i32.const 1))
                                           (i32.sub (global.get $ret-sp)
                                                    (table.size $ret-stack))))
                       (i32.const -1))
               (then
                (call $die0 (string.const "$grow-ret-stack"))
                (unreachable))))
     (func $maybe-grow-ret-stack
           (if (i32.lt_u (table.size $ret-stack) (global.get $ret-sp))
               (then (call $grow-ret-stack))))

     (func $grow-dyn-stack
           ;; Grow as in $grow-ret-stack.
           (if (i32.eq (table.grow $dyn-stack
                                   (ref.null $dyn)
                                   (i32.or (i32.shr_u (table.size $dyn-stack)
                                                      (i32.const 1))
                                           (i32.sub (global.get $dyn-sp)
                                                    (table.size $dyn-stack))))
                       (i32.const -1))
               (then
                (call $die0 (string.const "$grow-dyn-stack"))
                (unreachable))))
     (func $maybe-grow-dyn-stack
           (if (i32.lt_u (table.size $dyn-stack) (global.get $dyn-sp))
               (then (call $grow-dyn-stack))))

     (func $collect-rest-args (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (param $npositional i32)
           (result (ref eq))
           (local $ret (ref eq))
           (local.set $ret (ref.i31 (i32.const 13))) ;; null
           (block
            $done
            (block
             $nargs1
             (block
              $nargs2
              (block
               $nargs3
               (block
                $nargs4
                (block
                 $nargs5
                 (block
                  $nargs6
                  (block
                   $nargs7
                   (block
                    $nargs8
                    (block
                     $nargsN
                     (br_table $done
                               $nargs1
                               $nargs2
                               $nargs3
                               $nargs4
                               $nargs5
                               $nargs6
                               $nargs7
                               $nargs8
                               $nargsN
                               (local.get $nargs)))
                    (loop $lp
                      (if (i32.gt_u (local.get $nargs) (i32.const 8))
                          (then
                           (br_if $done (i32.le_u (local.get $nargs)
                                                  (local.get $npositional)))
                           (local.set
                            $ret
                            (struct.new
                             $pair
                             (i32.const 0)
                             (ref.as_non_null
                              (table.get
                               $argv
                               (i32.sub
                                (local.tee $nargs
                                           (i32.sub (local.get $nargs) (i32.const 1)))
                                (i32.const 8))))
                             (local.get $ret)))
                           (br $lp)))))
                   (br_if $done (i32.le_u (i32.const 8) (local.get $npositional)))
                   (local.set $ret
                              (struct.new $pair (i32.const 0)
                                          (global.get $arg7) (local.get $ret))))
                  (br_if $done (i32.le_u (i32.const 7) (local.get $npositional)))
                  (local.set $ret
                             (struct.new $pair (i32.const 0)
                                         (global.get $arg6) (local.get $ret))))
                 (br_if $done (i32.le_u (i32.const 6) (local.get $npositional)))
                 (local.set $ret
                            (struct.new $pair (i32.const 0)
                                        (global.get $arg5) (local.get $ret))))
                (br_if $done (i32.le_u (i32.const 5) (local.get $npositional)))
                (local.set $ret
                           (struct.new $pair (i32.const 0)
                                       (global.get $arg4) (local.get $ret))))
               (br_if $done (i32.le_u (i32.const 4) (local.get $npositional)))
               (local.set $ret
                          (struct.new $pair (i32.const 0)
                                      (global.get $arg3) (local.get $ret))))
              (br_if $done (i32.le_u (i32.const 3) (local.get $npositional)))
              (local.set $ret
                         (struct.new $pair (i32.const 0)
                                     (local.get $arg2) (local.get $ret))))
             (br_if $done (i32.le_u (i32.const 2) (local.get $npositional)))
             (local.set $ret
                        (struct.new $pair (i32.const 0)
                                    (local.get $arg1) (local.get $ret)))
             )
            (br_if $done (i32.le_u (i32.const 1) (local.get $npositional)))
            (local.set $ret
                       (struct.new $pair (i32.const 0)
                                   (local.get $arg0) (local.get $ret))))
           (local.get $ret))

     (func $values (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (block
            $done
            (local.set $arg0 (local.get $arg1))
            (local.set $arg1 (local.get $arg2))
            (br_if $done (i32.le_u (local.get $nargs) (i32.const 3)))
            (local.set $arg2 (global.get $arg3))
            (global.set $arg3 (global.get $arg4))
            (global.set $arg4 (global.get $arg5))
            (global.set $arg5 (global.get $arg6))
            (global.set $arg6 (global.get $arg7))
            (br_if $done (i32.le_u (local.get $nargs) (i32.const 8)))
            (global.set $arg7 (ref.as_non_null (table.get $argv (i32.const 0))))
            (table.copy $argv $argv (i32.const 0) (i32.const 1)
                        (i32.sub (local.get $nargs) (i32.const 9))))
           (i32.sub (local.get $nargs) (i32.const 1))
           (local.get $arg0)
           (local.get $arg1)
           (local.get $arg2)
           (global.set $ret-sp (i32.sub (global.get $ret-sp) (i32.const 1)))
           (global.get $ret-sp)
           (table.get $ret-stack)
           (return_call_ref $kvarargs))
     (global $values-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $values)))

     (global $append-primitive (mut (ref $proc))
             (struct.new $proc (i32.const 0) (ref.func $invalid-continuation)))

     (func $make-hash-table (result (ref $hash-table))
           (struct.new $hash-table (i32.const 0) (i32.const 0)
                       (array.new $raw-scmvector
                                  (ref.i31 (i32.const 13)) (i32.const 47))))

     (func $hashq-lookup (param $tab (ref $hash-table)) (param $k (ref eq))
           (result (ref null $pair))
           (local $idx i32)
           (local $buckets (ref $raw-scmvector))
           (local $chain (ref eq))
           (local $head (ref $pair))
           (local $link (ref $pair))
           (local.set $buckets
                      (struct.get $hash-table $buckets (local.get $tab)))
           (local.set $idx
                      (i32.rem_u (call $hashq (local.get $k))
                                 (array.len (local.get $buckets))))
           (local.set $chain
                      (array.get $raw-scmvector
                                 (local.get $buckets) (local.get $idx)))
           (loop $lp
             (if (i32.eqz (ref.test $pair (local.get $chain)))
                 (then (return (ref.null $pair)))
                 (else
                  (local.set $link (ref.cast $pair (local.get $chain)))
                  (local.set $head
                             (ref.cast $pair
                                       (struct.get $pair $car
                                                   (local.get $link))))
                  (if (ref.eq (struct.get $pair $car (local.get $head))
                              (local.get $k))
                      (then
                       (return (local.get $head)))
                      (else
                       (local.set $chain
                                  (struct.get $pair $cdr (local.get $link)))
                       (br $lp))))))
           (unreachable))

     (func $hashq-lookup/default
           (param $table (ref $hash-table))
           (param $key (ref eq))
           (param $default (ref eq))
           (result (ref eq))
           (local $handle (ref null $pair))
           (local.set $handle (call $hashq-lookup
                                    (local.get $table)
                                    (local.get $key)))
           (if (ref eq)
               (ref.is_null (local.get $handle))
               (then (local.get $default))
               (else (ref.as_non_null (local.get $handle)))))

     (func $hashq-insert (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $v (ref eq))
           (local $idx i32)
           (local $buckets (ref $raw-scmvector))
           (local.set $buckets (struct.get $hash-table $buckets (local.get $tab)))
           (local.set $idx (i32.rem_u (call $hashq (local.get $k))
                                      (array.len (local.get $buckets))))
           (array.set
            $raw-scmvector
            (local.get $buckets) (local.get $idx)
            (struct.new
             $pair (i32.const 0)
             (struct.new $pair (i32.const 0) (local.get $k) (local.get $v))
             (array.get $raw-scmvector (local.get $buckets) (local.get $idx))))
           (struct.set $hash-table $size
                       (local.get $tab)
                       (i32.add (struct.get $hash-table $size (local.get $tab))
                                (i32.const 1))))

     (func $hashq-ref (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $default (ref eq))
           (result (ref eq))
           (local $handle (ref null $pair))
           (local.set $handle
                      (call $hashq-lookup (local.get $tab) (local.get $k)))
           (if (ref eq)
               (ref.is_null (local.get $handle))
               (then (local.get $default))
               (else (struct.get $pair $cdr (local.get $handle)))))
     (func $hashq-update (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $v (ref eq)) (param $default (ref eq))
           (result (ref eq))
           (local $handle (ref null $pair))
           (local.set $handle
                      (call $hashq-lookup (local.get $tab) (local.get $k)))
           (if (ref eq)
               (ref.is_null (local.get $handle))
               (then
                (call $hashq-insert (local.get $tab) (local.get $k)
                      (local.get $v))
                (local.get $default))
               (else
                (struct.get $pair $cdr (local.get $handle))
                (struct.set $pair $cdr (local.get $handle)
                            (local.get $v)))))
     (func $hashq-set! (param $tab (ref $hash-table)) (param $k (ref eq))
           (param $v (ref eq))
           (call $hashq-update (local.get $tab) (local.get $k)
                 (local.get $v) (ref.i31 (i32.const 1)))
           (drop))

     (func $hashq-delete! (param $tab (ref $hash-table)) (param $k (ref eq))
           (local $idx i32)
           (local $buckets (ref $raw-scmvector))
           (local $chain (ref eq))
           (local $head (ref $pair))
           (local $link (ref $pair))
           (local $last (ref null $pair))
           (local.set $buckets
                      (struct.get $hash-table $buckets (local.get $tab)))
           (local.set $idx
                      (i32.rem_u (call $hashq (local.get $k))
                                 (array.len (local.get $buckets))))
           (local.set $chain
                      (array.get $raw-scmvector
                                 (local.get $buckets) (local.get $idx)))
           (loop $lp
             (if (i32.eqz (ref.test $pair (local.get $chain)))
                 (then (return))
                 (else
                  (local.set $link (ref.cast $pair (local.get $chain)))
                  (local.set $head
                             (ref.cast $pair
                                       (struct.get $pair $car
                                                   (local.get $link))))
                  (if (ref.eq (struct.get $pair $car (local.get $head))
                              (local.get $k))
                      (then
                       (struct.set $hash-table $size
                                   (local.get $tab)
                                   (i32.sub (struct.get $hash-table $size
                                                        (local.get $tab))
                                            (i32.const 1)))
                       (if (ref.is_null (local.get $last))
                           (then
                            (array.set $raw-scmvector
                                       (local.get $buckets)
                                       (local.get $idx)
                                       (struct.get $pair $cdr
                                                   (local.get $link)))
                            (return))
                           (else
                            (struct.set $pair $cdr
                                        (ref.as_non_null (local.get $last))
                                        (struct.get $pair $cdr
                                                    (local.get $link)))
                            (return))))
                      (else
                       (local.set $chain
                                  (struct.get $pair $cdr (local.get $link)))
                       (local.set $last (local.get $link))
                       (br $lp))))))
           (unreachable))

     ;; A specialized hash table, because it's not a hashq lookup.
     (type $symtab-entry
           (struct (field $sym (ref $symbol))
                   (field $next (ref null $symtab-entry))))
     (type $symtab (array (mut (ref null $symtab-entry))))
     (global $the-symtab (ref $symtab)
             (array.new $symtab (ref.null $symtab-entry) (i32.const 47)))

     ,(cond
       (import-abi?
        '(func $intern-symbol! (import "abi" "$intern-symbol!")
               (param $sym (ref $symbol)) (result (ref $symbol))))
       (else
        '(func $intern-symbol!
               (param $sym (ref $symbol)) (result (ref $symbol))
               (local $hash i32)
               (local $idx i32)
               (local $entry (ref null $symtab-entry))
               (local.set $hash (struct.get $heap-object $hash (local.get $sym)))
               (local.set $idx (i32.rem_u (local.get $hash)
                                          (array.len (global.get $the-symtab))))
               (local.set $entry
                          (array.get $symtab (global.get $the-symtab)
                                     (local.get $idx)))
               (block
                $insert
                (loop $lp
                  (br_if $insert (ref.is_null (local.get $entry)))
                  (block
                   $next
                   (br_if $next
                          (i32.ne (struct.get $symbol $hash
                                              (struct.get $symtab-entry $sym
                                                          (local.get $entry)))
                                  (local.get $hash)))
                   (br_if $next
                          (i32.eqz
                           (string.eq
                            (struct.get $string $str
                                        (struct.get $symbol $name
                                                    (struct.get $symtab-entry $sym
                                                                (local.get $entry))))
                            (struct.get $string $str
                                        (struct.get $symbol $name
                                                    (local.get $sym))))))
                   (return (struct.get $symtab-entry $sym (local.get $entry))))
                  (local.set $entry
                             (struct.get $symtab-entry $next (local.get $entry)))
                  (br $lp)))
               (array.set $symtab (global.get $the-symtab) (local.get $idx)
                          (struct.new $symtab-entry
                                      (local.get $sym)
                                      (array.get $symtab (global.get $the-symtab)
                                                 (local.get $idx))))
               (local.get $sym))))

     ;; For now, the Java string hash function, except over codepoints
     ;; rather than WTF-16 code units.
     (func $string-hash (param $str (ref string)) (result i32)
           (local $iter (ref stringview_iter))
           (local $hash i32)
           (local $codepoint i32)
           (local.set $iter (string.as_iter (local.get $str)))
           (block $done
                  (loop $lp
                    (local.set $codepoint (stringview_iter.next (local.get $iter)))
                    (br_if $done (i32.eq (i32.const -1) (local.get $codepoint)))
                    (local.set $hash
                               (i32.add (i32.mul (local.get $hash) (i32.const 31))
                                        (local.get $codepoint)))
                    (br $lp)))
           (local.get $hash))

     (func $string->symbol* (param $str (ref $string))
           (result (ref $symbol) i32)
           (local $fresh (ref $symbol))
           (local $interned (ref $symbol))
           (local.set $fresh
                      (struct.new $symbol
                                  (call $finish-heap-object-hash
                                        (call $string-hash
                                              (struct.get $string $str
                                                          (local.get $str))))
                                  (local.get $str)))
           (local.set $interned (call $intern-symbol! (local.get $fresh)))
           (local.get $interned)
           (ref.eq (local.get $interned) (local.get $fresh)))

     (func $string->symbol (param $str (ref $string)) (result (ref $symbol))
           (call $string->symbol* (local.get $str))
           (drop))

     (global $the-kwtab (ref $hash-table)
             (struct.new $hash-table (i32.const 0) (i32.const 0)
                         (array.new $raw-scmvector
                                    (ref.i31 (i32.const 13)) (i32.const 47))))
     ,(cond
       (import-abi?
        '(func $intern-keyword! (import "abi" "$intern-keyword!")
               (param $sym (ref $keyword)) (result (ref $keyword))))
       (else
        '(func $intern-keyword! (param $kw (ref $keyword)) (result (ref $keyword))
               (local $handle (ref null $pair))
               (local.set $handle
                          (call $hashq-lookup (global.get $the-kwtab)
                                (struct.get $keyword $name (local.get $kw))))
               (if (ref $keyword)
                   (ref.is_null (local.get $handle))
                   (then
                    (call $hashq-insert (global.get $the-kwtab)
                          (struct.get $keyword $name (local.get $kw))
                          (local.get $kw))
                    (local.get $kw))
                   (else
                    (ref.cast $keyword
                              (struct.get $pair $cdr (local.get $handle))))))))

     (func $symbol->keyword (param $sym (ref $symbol)) (result (ref $keyword))
           (call $intern-keyword!
                 (struct.new $keyword
                             (call $finish-heap-object-hash
                                   (struct.get $symbol $hash (local.get $sym)))
                             (local.get $sym))))

     (func $push-dyn (param $dyn (ref $dyn))
           (local $dyn-sp i32)
           (global.set $dyn-sp
                       (i32.add (local.tee $dyn-sp (global.get $dyn-sp))
                                (i32.const 1)))
           (call $maybe-grow-dyn-stack)
           (table.set $dyn-stack (local.get $dyn-sp) (local.get $dyn)))

     (func $wind-dynstate (param $dynstate (ref $dynstate))
           (local $fluids (ref $hash-table))
           (local.set $fluids (global.get $current-fluids))
           (global.set $current-fluids
                       (struct.get $dynstate $fluids (local.get $dynstate)))
           (struct.set $dynstate $fluids (local.get $dynstate)
                       (local.get $fluids)))

     (func $push-dynamic-state (param $state (ref $dynamic-state))
           (local $dynstate (ref $dynstate))
           (call $push-dyn
                 (local.tee $dynstate
                            (struct.new $dynstate
                                        (struct.get $dynamic-state $fluids
                                                    (local.get $state)))))
           (return_call $wind-dynstate (local.get $dynstate)))

     (func $pop-dynamic-state
           (local $sp i32)
           (global.set $dyn-sp
                       (local.tee $sp (i32.sub (global.get $dyn-sp)
                                               (i32.const 1))))
           (return_call $wind-dynstate
                        (ref.cast $dynstate
                                  (table.get $dyn-stack (local.get $sp)))))

     (func $wind-dynfluid (param $dynfluid (ref $dynfluid))
           (local $fluid (ref $fluid))
           (local.set $fluid
                      (struct.get $dynfluid $fluid (local.get $dynfluid)))
           (struct.set
            $dynfluid $val
            (local.get $dynfluid)
            (call $hashq-update (global.get $current-fluids)
                  (local.get $fluid)
                  (struct.get $dynfluid $val (local.get $dynfluid))
                  (struct.get $fluid $init (local.get $fluid)))))

     (func $push-fluid (param $fluid (ref $fluid)) (param $val (ref eq))
           (local $dynfluid (ref $dynfluid))
           (local.set $dynfluid
                      (struct.new $dynfluid
                                  (local.get $fluid) (local.get $val)))
           (call $push-dyn (local.get $dynfluid))
           (call $wind-dynfluid (local.get $dynfluid)))

     (func $pop-fluid
           (local $sp i32)
           (global.set $dyn-sp
                       (local.tee $sp (i32.sub (global.get $dyn-sp)
                                               (i32.const 1))))
           (call $wind-dynfluid
                 (ref.cast $dynfluid (table.get $dyn-stack (local.get $sp)))))

     (func $fluid-ref (param $fluid (ref $fluid)) (result (ref eq))
           (call $hashq-ref (global.get $current-fluids)
                 (local.get $fluid)
                 (struct.get $fluid $init (local.get $fluid))))

     (func $fluid-ref* (param $fluid (ref $fluid)) (param $depth i32)
           (result (ref eq))
           (local $sp i32)
           (local $dyn (ref $dyn))
           (if (local.get $depth)
               (then
                (local.set $sp (global.get $dyn-sp))
                (loop $lp
                  (if (local.get $sp)
                      (then
                       (local.set $sp (i32.sub (local.get $sp) (i32.const 1)))
                       (local.set $dyn (ref.as_non_null
                                        (table.get $dyn-stack (local.get $sp))))
                       (br_if $lp (i32.eqz
                                   (ref.test $dynfluid (local.get $dyn))))
                       (local.set $depth
                                  (i32.sub (local.get $depth) (i32.const 1)))
                       (br_if $lp (local.get $depth))
                       (return
                        (struct.get
                         $dynfluid $val
                         (ref.cast $dynfluid (local.get $dyn)))))
                      (else (return (ref.i31 (i32.const 1)))))))
               (else (return_call $fluid-ref (local.get $fluid))))
           (unreachable))

     (func $fluid-set! (param $fluid (ref $fluid)) (param $val (ref eq))
           (call $hashq-set! (global.get $current-fluids)
                 (local.get $fluid)
                 (local.get $val)))

     (func $find-prompt (param $tag (ref eq))
           (result (ref $dynprompt) i32)
           (local $dyn (ref $dyn))
           (local $prompt (ref $dynprompt))
           (local $sp i32)
           (local.set $sp (global.get $dyn-sp))
           (loop $lp
             (if (local.get $sp)
                 (then
                  (local.set $sp (i32.sub (local.get $sp) (i32.const 1)))
                  ;; FIXME: could br_on_cast_fail to $lp; need to fix
                  ;; the assembler.
                  (local.set $dyn (ref.as_non_null
                                   (table.get $dyn-stack (local.get $sp))))
                  (if (ref.test $dynprompt (local.get $dyn))
                      (then
                       (local.set $prompt
                                  (ref.cast $dynprompt (local.get $dyn)))
                       (if (ref.eq (struct.get $dynprompt $tag
                                               (local.get $prompt))
                                   (local.get $tag))
                           (then (return (local.get $prompt)
                                         (local.get $sp)))
                           (else (br $lp)))))
                  (br $lp))
                 (else
                  (call $raise-runtime-error-with-message+irritants
                        (string.const "prompt not found")
                        (struct.new $pair
                                    (i32.const 0)
                                    (local.get $tag)
                                    (ref.i31 (i32.const 13)))
                        ,@no-source))))
           (unreachable))

     (func $rewind
           (param $raw-sp-adjust i32)
           (param $scm-sp-adjust i32)
           (param $ret-sp-adjust i32)
           (param $dyn (ref $raw-dynvector))
           (param $i i32)
           (param $args (ref eq))
           (local $d (ref $dyn))
           (local $dynwind (ref $dynwind))
           (local $dynprompt (ref $dynprompt))
           (local $dynfluid (ref $dynfluid))
           (local $dynstate (ref $dynstate))
           (local $base i32)
           (loop $lp
             (if (i32.eq (local.get $i) (array.len (local.get $dyn)))
                 (then
                  (return_call $apply (i32.const 3)
                               (global.get $apply-primitive)
                               (global.get $values-primitive)
                               (local.get $args))))
             (local.set $d (array.get $raw-dynvector
                                      (local.get $dyn)
                                      (local.get $i)))
             (block
              $next
              (if (ref.test $dynwind (local.get $d))
                  (then
                   (local.set $dynwind (ref.cast $dynwind (local.get $d)))
                   (local.set $base (global.get $raw-sp))
                   (global.set $raw-sp (i32.add (local.get $base) (i32.const 16)))
                   (global.set $scm-sp (i32.add (global.get $scm-sp) (i32.const 2)))
                   (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
                   (call $maybe-grow-raw-stack)
                   (call $maybe-grow-scm-stack)
                   (call $maybe-grow-ret-stack)
                   (i32.store $raw-stack offset=0 (local.get $base)
                              (local.get $raw-sp-adjust))
                   (i32.store $raw-stack offset=4 (local.get $base)
                              (local.get $scm-sp-adjust))
                   (i32.store $raw-stack offset=8 (local.get $base)
                              (local.get $ret-sp-adjust))
                   (i32.store $raw-stack offset=12 (local.get $base)
                              (local.get $i))
                   (table.set $scm-stack
                              (i32.sub (global.get $scm-sp) (i32.const 2))
                              (local.get $dyn))
                   (table.set $scm-stack
                              (i32.sub (global.get $scm-sp) (i32.const 1))
                              (local.get $args))
                   (table.set $ret-stack
                              (i32.sub (global.get $ret-sp) (i32.const 1))
                              (ref.func $keep-rewinding))
                   (return_call_ref $kvarargs
                                    (i32.const 1)
                                    (struct.get $dynwind $wind
                                                (local.get $dynwind))
                                    (ref.i31 (i32.const 0))
                                    (ref.i31 (i32.const 0))
                                    (struct.get
                                     $proc $func
                                     (struct.get $dynwind $wind
                                                 (local.get $dynwind))))))
              (if (ref.test $dynprompt (local.get $d))
                  (then
                   (local.set $dynprompt (ref.cast $dynprompt (local.get $d)))
                   (local.set
                    $d
                    (struct.new
                     $dynprompt
                     (i32.add
                      (struct.get $dynprompt $raw-sp (local.get $dynprompt))
                      (local.get $raw-sp-adjust))
                     (i32.add
                      (struct.get $dynprompt $scm-sp (local.get $dynprompt))
                      (local.get $scm-sp-adjust))
                     (i32.add
                      (struct.get $dynprompt $ret-sp (local.get $dynprompt))
                      (local.get $ret-sp-adjust))
                     (struct.get_u $dynprompt $unwind-only?
                                   (local.get $dynprompt))
                     (struct.get $dynprompt $tag (local.get $dynprompt))
                     (struct.get $dynprompt $handler (local.get $dynprompt))))
                   (br $next)))
              (if (ref.test $dynfluid (local.get $d))
                  (then
                   (local.set $dynfluid (ref.cast $dynfluid (local.get $d)))
                   (call $wind-dynfluid (local.get $dynfluid))
                   (br $next)))
              (if (ref.test $dynstate (local.get $d))
                  (then
                   (local.set $dynstate (ref.cast $dynstate (local.get $d)))
                   (call $wind-dynstate (local.get $dynstate))
                   (br $next))
                  (else (unreachable))))
             (call $push-dyn (local.get $d))
             (local.set $i (i32.add (local.get $i) (i32.const 1)))
             (br $lp)))

     (func $restore-raw-stack (param $v (ref $raw-bytevector))
           (local $sp i32)
           (local $i i32)
           (local $len i32)
           (local.set $sp (global.get $raw-sp))
           (local.set $i (i32.const 0))
           (local.set $len (array.len (local.get $v)))
           (global.set $raw-sp (i32.add (local.get $sp) (local.get $len)))
           (call $maybe-grow-raw-stack)
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (i32.store8 $raw-stack
                              (i32.add (local.get $sp) (local.get $i))
                              (array.get_u $raw-bytevector
                                           (local.get $v)
                                           (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))))

     (func $restore-scm-stack (param $v (ref $raw-scmvector))
           (local $sp i32)
           (local $i i32)
           (local $len i32)
           (local.set $sp (global.get $scm-sp))
           (local.set $len (array.len (local.get $v)))
           (global.set $scm-sp (i32.add (local.get $sp) (local.get $len)))
           (call $maybe-grow-scm-stack)
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (table.set $scm-stack
                             (i32.add (local.get $sp) (local.get $i))
                             (array.get $raw-scmvector
                                        (local.get $v)
                                        (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))))

     (func $restore-ret-stack (param $v (ref $raw-retvector))
           (local $sp i32)
           (local $i i32)
           (local $len i32)
           (local.set $sp (global.get $ret-sp))
           (local.set $len (array.len (local.get $v)))
           (global.set $ret-sp (i32.add (local.get $sp) (local.get $len)))
           (call $maybe-grow-ret-stack)
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (table.set $ret-stack
                             (i32.add (local.get $sp) (local.get $i))
                             (array.get $raw-retvector
                                        (local.get $v)
                                        (local.get $i)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))))

     (func $compose-continuation (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $cont (ref $cont))
           (local $prompt (ref $dynprompt))
           (local $raw-sp-adjust i32)
           (local $scm-sp-adjust i32)
           (local $ret-sp-adjust i32)
           (local $args (ref eq))
           (local.set $cont (ref.cast $cont (local.get $arg0)))
           (local.set $prompt (struct.get $cont $prompt (local.get $cont)))
           (local.set $raw-sp-adjust
                      (i32.sub (global.get $raw-sp)
                               (struct.get $dynprompt $raw-sp
                                           (local.get $prompt))))
           (local.set $scm-sp-adjust
                      (i32.sub (global.get $scm-sp)
                               (struct.get $dynprompt $scm-sp
                                           (local.get $prompt))))
           (local.set $ret-sp-adjust
                      (i32.sub (global.get $ret-sp)
                               (struct.get $dynprompt $ret-sp
                                           (local.get $prompt))))
           (local.set $args
                      (call $collect-rest-args (local.get $nargs)
                            (local.get $arg0)
                            (local.get $arg1)
                            (local.get $arg2)
                            (i32.const 1)))
           (call $restore-raw-stack
                 (struct.get $cont $raw-stack (local.get $cont)))
           (call $restore-scm-stack
                 (struct.get $cont $scm-stack (local.get $cont)))
           (call $restore-ret-stack
                 (struct.get $cont $ret-stack (local.get $cont)))
           ;; Dyn stack is restored incrementally via $rewind.
           (return_call $rewind
                        (local.get $raw-sp-adjust)
                        (local.get $scm-sp-adjust)
                        (local.get $ret-sp-adjust)
                        (struct.get $cont $dyn-stack (local.get $cont))
                        (i32.const 0)
                        (local.get $args)))

     (func $capture-raw-stack (param $base-sp i32)
           (result (ref $raw-bytevector))
           (local $v (ref $raw-bytevector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $raw-sp) (local.get $base-sp)))
           (local.set $v (array.new_default $raw-bytevector
                                            (local.get $len)))
           (local.set $i (i32.const 0))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-bytevector
                             (local.get $v)
                             (local.get $i)
                             (i32.load8_u $raw-stack
                                          (i32.add (local.get $base-sp)
                                                   (local.get $i))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-scm-stack (param $base-sp i32)
           (result (ref $raw-scmvector))
           (local $v (ref $raw-scmvector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $scm-sp) (local.get $base-sp)))
           (local.set $v
                      (array.new $raw-scmvector
                                 (ref.i31 (i32.const 1))
                                 (local.get $len)))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-scmvector
                             (local.get $v)
                             (local.get $i)
                             (ref.as_non_null
                              (table.get $scm-stack
                                         (i32.add (local.get $base-sp)
                                                  (local.get $i)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-ret-stack (param $base-sp i32)
           (result (ref $raw-retvector))
           (local $v (ref $raw-retvector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $ret-sp) (local.get $base-sp)))
           (local.set $v
                      (array.new $raw-retvector
                                 (ref.func $invalid-continuation)
                                 (local.get $len)))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-retvector
                             (local.get $v)
                             (local.get $i)
                             (ref.as_non_null
                              (table.get $ret-stack
                                         (i32.add (local.get $base-sp)
                                                  (local.get $i)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-dyn-stack (param $base-sp i32)
           (result (ref $raw-dynvector))
           (local $v (ref $raw-dynvector))
           (local $i i32)
           (local $len i32)
           (local.set $len (i32.sub (global.get $dyn-sp) (local.get $base-sp)))
           (local.set $v
                      (array.new $raw-dynvector
                                 (struct.new $dyn)
                                 (local.get $len)))
           (loop $lp
             (if (i32.lt_u (local.get $i) (local.get $len))
                 (then
                  (array.set $raw-dynvector
                             (local.get $v)
                             (local.get $i)
                             (ref.as_non_null
                              (table.get $dyn-stack
                                         (i32.add (local.get $base-sp)
                                                  (local.get $i)))))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (br $lp))))
           (local.get $v))

     (func $capture-continuation (param $prompt (ref $dynprompt))
           (param $prompt-dyn-sp i32)
           (result (ref eq))
           (if (result (ref eq))
               (struct.get_u $dynprompt $unwind-only? (local.get $prompt))
               (then (ref.i31 (i32.const 1)))
               (else
                (struct.new
                 $cont
                 (i32.const 0)
                 (ref.func $compose-continuation)
                 (local.get $prompt)
                 (call $capture-raw-stack
                       (struct.get $dynprompt $raw-sp (local.get $prompt)))
                 (call $capture-scm-stack
                       (struct.get $dynprompt $scm-sp (local.get $prompt)))
                 (call $capture-ret-stack
                       ;; Increment to avoid including the prompt unwind
                       ;; continuation.  We rely on the compiler
                       ;; generating code for non-unwind-only prompt
                       ;; bodies that consists of just a closure call.
                       (i32.add
                        (struct.get $dynprompt $ret-sp (local.get $prompt))
                        (i32.const 1)))
                 (call $capture-dyn-stack
                       ;; Incremented to avoid including the prompt
                       ;; itself.
                       (i32.add (local.get $prompt-dyn-sp) (i32.const 1)))))))

     (func $keep-unwinding (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $tag (ref eq))
           (local $cont (ref eq))
           (local $args (ref eq))
           (local.set $tag
                      (ref.as_non_null
                       (table.get $scm-stack
                                  (i32.sub (global.get $scm-sp) (i32.const 3)))))
           (local.set $cont
                      (ref.as_non_null
                       (table.get $scm-stack
                                  (i32.sub (global.get $scm-sp) (i32.const 2)))))
           (local.set $args
                      (ref.as_non_null
                       (table.get $scm-stack
                                  (i32.sub (global.get $scm-sp) (i32.const 1)))))
           (global.set $scm-sp (i32.sub (global.get $scm-sp) (i32.const 3)))
           (return_call $unwind-to-prompt
                        (local.get $tag) (local.get $cont) (local.get $args)))

     (func $keep-rewinding (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $raw-sp-adjust i32)
           (local $scm-sp-adjust i32)
           (local $ret-sp-adjust i32)
           (local $i i32)
           (local $dyn (ref $raw-dynvector))
           (local $d (ref $dynwind))
           (local $args (ref eq))
           (global.set $raw-sp (i32.sub (global.get $raw-sp) (i32.const 16)))
           (local.set $raw-sp-adjust
                      (i32.load $raw-stack offset=0 (global.get $raw-sp)))
           (local.set $scm-sp-adjust
                      (i32.load $raw-stack offset=4 (global.get $raw-sp)))
           (local.set $ret-sp-adjust
                      (i32.load $raw-stack offset=8 (global.get $raw-sp)))
           (local.set $i
                      (i32.load $raw-stack offset=12 (global.get $raw-sp)))
           (global.set $scm-sp (i32.sub (global.get $scm-sp) (i32.const 2)))
           (local.set $dyn (ref.cast
                            $raw-dynvector
                            (table.get $scm-stack (global.get $scm-sp))))
           (local.set $args (ref.as_non_null
                             (table.get $scm-stack
                                        (i32.add (global.get $scm-sp)
                                                 (i32.const 1)))))
           (local.set $d (ref.cast $dynwind
                                   (array.get $raw-dynvector
                                              (local.get $dyn) (local.get $i))))
           (call $push-dyn (local.get $d))
           (return_call $rewind
                        (local.get $raw-sp-adjust)
                        (local.get $scm-sp-adjust)
                        (local.get $ret-sp-adjust)
                        (local.get $dyn)
                        (i32.add (local.get $i) (i32.const 1))
                        (local.get $args)))

     (func $unwind-to-prompt
           (param $tag (ref eq)) (param $cont (ref eq)) (param $args (ref eq))
           (local $prompt (ref $dynprompt))
           (local $dynwind (ref $dynwind))
           (local $dyn (ref $dyn))
           ;; During an abort-to-prompt that crosses a dynamic-wind,
           ;; after the dynamic-wind unwinder returns, it could be that
           ;; the dynamic stack is different from where the
           ;; abort-to-prompt started.  It could be that the prompt is
           ;; no longer in the continuation; that's why we look it up
           ;; again here.  More annoyingly, it could be that the prompt
           ;; becomes not unwind-only!  FIXME to check that if $cont is
           ;; #f, that the prompt is indeed still unwind-only.
           (call $find-prompt (local.get $tag))
           (drop) ;; prompt dyn-sp
           (local.set $prompt)
           (loop $lp
             (global.set $dyn-sp
                         (i32.sub (global.get $dyn-sp) (i32.const 1)))
             (local.set $dyn (ref.as_non_null
                              (table.get $dyn-stack (global.get $dyn-sp))))
             (if (ref.eq (local.get $dyn) (local.get $prompt))
                 (then
                  ;; Unwind control stacks.
                  (global.set $raw-sp (struct.get $dynprompt $raw-sp
                                                  (local.get $prompt)))
                  (global.set $scm-sp (struct.get $dynprompt $scm-sp
                                                  (local.get $prompt)))
                  (global.set $ret-sp (struct.get $dynprompt $ret-sp
                                                  (local.get $prompt)))
                  ;; Use apply + values to pass values to handler.
                  (global.set $ret-sp
                              (i32.add (global.get $ret-sp) (i32.const 1)))
                  (call $maybe-grow-ret-stack)
                  (table.set $ret-stack
                             (i32.sub (global.get $ret-sp) (i32.const 1))
                             (struct.get $dynprompt $handler
                                         (local.get $prompt)))
                  (throw $trampoline-tag
                         (i32.const 3)
                         (global.get $apply-primitive)
                         (global.get $values-primitive)
                         (struct.new $pair (i32.const 0)
                                     (local.get $cont)
                                     (local.get $args))
                         (struct.get $proc $func
                                     (ref.cast $proc
                                               (global.get $apply-primitive)))
                         (i32.const 1))))
             ;; Something else is on the stack; what is it?
             (if (ref.test $dynwind (local.get $dyn))
                 (then
                  (local.set $dynwind (ref.cast $dynwind (local.get $dyn)))
                  (global.set $scm-sp (i32.add (global.get $scm-sp) (i32.const 3)))
                  (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
                  (call $maybe-grow-scm-stack)
                  (call $maybe-grow-ret-stack)
                  (table.set $scm-stack
                             (i32.sub (global.get $scm-sp) (i32.const 3))
                             (local.get $tag))
                  (table.set $scm-stack
                             (i32.sub (global.get $scm-sp) (i32.const 2))
                             (local.get $cont))
                  (table.set $scm-stack
                             (i32.sub (global.get $scm-sp) (i32.const 1))
                             (local.get $args))
                  (table.set $ret-stack
                             (i32.sub (global.get $ret-sp) (i32.const 1))
                             (ref.func $keep-unwinding))
                  (return_call_ref $kvarargs
                                   (i32.const 1)
                                   (struct.get $dynwind $unwind
                                               (local.get $dynwind))
                                   (ref.i31 (i32.const 0))
                                   (ref.i31 (i32.const 0))
                                   (struct.get
                                    $proc $func
                                    (struct.get $dynwind $unwind
                                                (local.get $dynwind))))))
             (br_if $lp (ref.test $dynprompt (local.get $dyn)))
             (if (ref.test $dynfluid (local.get $dyn))
                 (then
                  (call $wind-dynfluid (ref.cast $dynfluid (local.get $dyn)))
                  (br $lp)))
             (if (ref.test $dynstate (local.get $dyn))
                 (then
                  (call $wind-dynstate (ref.cast $dynstate (local.get $dyn)))
                  (br $lp)))
             (unreachable)))

     (func $abort-to-prompt (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (if (i32.lt_u (local.get $nargs) (i32.const 2))
               (then
                (return_call $raise-arity-error
                             (string.const "abort-to-prompt")
                             (global.get $abort-to-prompt-primitive)
                             ,@no-source)))
           ;; $arg0 is the closure, $arg1 is tag, and the values are in
           ;; $arg2 and up, which we collect to a rest list.
           (return_call $unwind-to-prompt (local.get $arg1)
                        (call $capture-continuation
                              (call $find-prompt (local.get $arg1)))
                        (call $collect-rest-args (local.get $nargs)
                              (local.get $arg0)
                              (local.get $arg1)
                              (local.get $arg2)
                              (i32.const 2))))
     (global $abort-to-prompt-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $abort-to-prompt)))

     (func $maybe-grow-argv (param $size i32)
           (local $diff i32)
           (local.set $diff (i32.sub (local.get $size)
                                     (table.size $argv)))
           (if (i32.gt_s (local.get $diff) (i32.const 0))
               (then
                (table.grow $argv
                            (ref.null eq)
                            (local.get $diff))
                (drop))))

     (func $compute-npositional/kwargs (param $nargs i32)
           (param $arg0 (ref eq))
           (param $arg1 (ref eq))
           (param $arg2 (ref eq))
           (param $nreq i32)
           (result i32)
           (local $npos i32)
           (local.set $npos (local.get $nreq))
           (loop $lp
             (if (i32.lt_u (local.get $npos) (local.get $nargs))
                 (then
                  (if (i32.eqz
                       (ref.test $keyword
                                 (call $arg-ref
                                       (local.get $npos)
                                       (local.get $arg0)
                                       (local.get $arg1)
                                       (local.get $arg2))))
                      (then
                       (local.set $npos
                                  (i32.add (local.get $npos) (i32.const 1)))
                       (br $lp))))))
           (local.get $npos))

     (func $keyword->idx (param $kw (ref eq))
           (param $all-kws (ref eq))
           (result i32)
           (local $idx i32)
           (local $pair (ref $pair))
           (loop $lp
             (if (ref.test $pair (local.get $all-kws))
                 (then
                  (if (ref.eq (struct.get
                               $pair $car
                               (ref.cast $pair (local.get $all-kws)))
                              (local.get $kw))
                      (then (return (local.get $idx))))
                  (local.set $all-kws
                             (struct.get
                              $pair $cdr
                              (ref.cast $pair (local.get $all-kws))))
                  (local.set $idx
                             (i32.add (i32.const 1) (local.get $idx)))
                  (br $lp))))
           (i32.const -1))

     (func $arg-ref (param $n i32)
           (param $arg0 (ref eq))
           (param $arg1 (ref eq))
           (param $arg2 (ref eq))
           (result (ref eq))
           (block
            $n0
            (block
             $n1
             (block
              $n2
              (block
               $n3
               (block
                $n4
                (block
                 $n5
                 (block
                  $n6
                  (block
                   $n7
                   (block
                    $nv
                    (br_table $n0
                              $n1
                              $n2
                              $n3
                              $n4
                              $n5
                              $n6
                              $n7
                              $nv
                              (local.get $n)))
                   (return (ref.as_non_null
                            (table.get $argv (i32.sub (local.get $n) (i32.const 8))))))
                  (return (global.get $arg7)))
                 (return (global.get $arg6)))
                (return (global.get $arg5)))
               (return (global.get $arg4)))
              (return (global.get $arg3)))
             (return (local.get $arg2)))
            (return (local.get $arg1)))
           (return (local.get $arg0)))

     (func $collect-apply-args
           (param $nargs i32) (param $arg2 (ref eq))
           (result (ref eq))
           (local $ret (ref eq))
           (if (i32.le_u (local.get $nargs) (i32.const 3))
               (then
                (call $die0 (string.const "bad collect-apply-args call"))
                (unreachable)))
           (local.set $ret
                      (call $arg-ref
                            (local.tee $nargs
                                       (i32.sub (local.get $nargs)
                                                (i32.const 1)))
                            (ref.i31 (i32.const 1))
                            (ref.i31 (i32.const 1))
                            (local.get $arg2)))
           (loop $lp
             (if (i32.le_u (i32.const 3) (local.get $nargs))
                 (then
                  (local.set $ret
                             (struct.new
                              $pair
                              (i32.const 0)
                              (call $arg-ref
                                    (local.tee $nargs
                                               (i32.sub (local.get $nargs)
                                                        (i32.const 1)))
                                    (ref.i31 (i32.const 1))
                                    (ref.i31 (i32.const 1))
                                    (local.get $arg2))
                              (local.get $ret)))
                  (br $lp))))
           (local.get $ret))

     (func $apply-to-non-list (param $tail (ref eq))
           (call $raise-runtime-error-with-message+irritants
                 (string.const "apply to non-list")
                 (struct.new $pair
                             (i32.const 0)
                             (local.get $tail)
                             (ref.i31 (i32.const 13)))
                 ,@no-source))
     (func $struct-apply (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $proc (ref $proc))
           (block $continue (ref $proc)
                  (struct.get $struct/1 $field0
                              (ref.cast $struct/1 (local.get $arg0)))
                  (br_on_cast $continue (ref eq) (ref $proc))
                  ;; TODO: Source location would be nice.
                  (call $raise-type-error
                        (string.const "apply")
                        (string.const "procedure")
                        (local.get $arg0)
                        ,@no-source)
                  (unreachable))
           (local.set $proc)
           (return_call_ref $kvarargs
                            (local.get $nargs)
                            (local.get $proc)
                            (local.get $arg1)
                            (local.get $arg2)
                            (struct.get $proc $func (local.get $proc))))
     (global $struct-apply-primitive (ref $proc)
             (struct.new $proc (i32.const 0) (ref.func $struct-apply)))
     (func $get-callee-code (param $callee (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref $kvarargs))
           (local $parentv (ref $raw-scmvector))
           (block $done (ref $proc)
                  (br_on_cast $done (ref eq) (ref $proc)
                              (local.get $callee))
                  ;; An applicable struct is a struct whose vtable has
                  ;; the applicable flag set.
                  ;;
                  ;; Note that we can't call the func within the
                  ;; struct directly because arg0 will be the struct,
                  ;; not the procedure.  Instead, we pass along a
                  ;; trampoline that will DTRT.
                  (block $fail
                         (block $continue (ref $struct)
                                (br_on_cast $continue (ref eq) (ref $struct)
                                            (local.get $callee))
                                (drop)
                                (br $fail)
                                (unreachable))
                         (struct.get $struct $vtable)
                         (struct.get $vtable $applicable?)
                         (ref.i31 (i32.const 13))
                         (ref.eq)
                         (br_if $fail)
                         (br $done (global.get $struct-apply-primitive)))
                  (call $raise-type-error
                        (string.const "apply")
                        (string.const "procedure?")
                        (local.get $callee)
                        ,@propagate-source)
                  (unreachable))
           (struct.get $proc $func))

     (func $apply (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $args (ref eq))
           (if (i32.lt_u (local.get $nargs) (i32.const 3))
               (then
                (return_call $raise-arity-error
                             (string.const "apply")
                             (global.get $apply-primitive)
                             ,@no-source)))
           (local.set $arg0 (local.get $arg1))
           (local.set $args
                      (if (ref eq)
                          (i32.eq (local.get $nargs) (i32.const 3))
                          (then (local.get $arg2))
                          (else (call $collect-apply-args
                                      (local.get $nargs)
                                      (local.get $arg2)))))
           (if
            (ref.test $pair (local.get $args))
            (then
             (local.set $arg1
                        (struct.get $pair $car
                                    (ref.cast $pair (local.get $args))))
             (if
              (ref.test
               $pair
               (local.tee $args
                          (struct.get $pair $cdr
                                      (ref.cast $pair (local.get $args)))))
              (then
               (local.set $arg2
                          (struct.get $pair $car
                                      (ref.cast $pair (local.get $args))))
               (if
                (ref.test
                 $pair
                 (local.tee $args
                            (struct.get $pair $cdr
                                        (ref.cast $pair (local.get $args)))))
                (then
                 (global.set $arg3
                             (struct.get $pair $car
                                         (ref.cast $pair (local.get $args))))
                 (if
                  (ref.test
                   $pair
                   (local.tee $args
                              (struct.get $pair $cdr
                                          (ref.cast $pair (local.get $args)))))
                  (then
                   (global.set $arg4
                               (struct.get $pair $car
                                           (ref.cast $pair (local.get $args))))
                   (if
                    (ref.test
                     $pair
                     (local.tee $args
                                (struct.get $pair $cdr
                                            (ref.cast $pair (local.get $args)))))
                    (then
                     (global.set $arg5
                                 (struct.get $pair $car
                                             (ref.cast $pair (local.get $args))))
                     (if
                      (ref.test
                       $pair
                       (local.tee $args
                                  (struct.get $pair $cdr
                                              (ref.cast $pair (local.get $args)))))
                      (then
                       (global.set $arg6
                                   (struct.get $pair $car
                                               (ref.cast $pair (local.get $args))))
                       (if
                        (ref.test
                         $pair
                         (local.tee $args
                                    (struct.get $pair $cdr
                                                (ref.cast $pair (local.get $args)))))
                        (then
                         (global.set $arg7
                                     (struct.get $pair $car
                                                 (ref.cast $pair (local.get $args))))
                         (local.set $nargs (i32.const 8))
                         (loop $lp
                           (if
                            (ref.test
                             $pair
                             (local.tee $args
                                        (struct.get $pair $cdr
                                                    (ref.cast $pair (local.get $args)))))
                            (then
                             (if (i32.lt_u (table.size $argv)
                                           (i32.sub (local.get $nargs) (i32.const 7)))
                                 (then
                                  (table.grow $argv
                                              (struct.get $pair $car
                                                          (ref.cast $pair (local.get $args)))
                                              (i32.const 1))
                                  (drop))
                                 (else
                                  (table.set $argv
                                             (i32.sub (local.get $nargs) (i32.const 8))
                                             (struct.get $pair $car
                                                         (ref.cast $pair (local.get $args))))))
                             (local.set $nargs (i32.add (local.get $nargs) (i32.const 1)))
                             (br $lp)))))
                        (else (local.set $nargs (i32.const 7)))))
                      (else (local.set $nargs (i32.const 6)))))
                    (else (local.set $nargs (i32.const 5)))))
                  (else (local.set $nargs (i32.const 4)))))
                (else (local.set $nargs (i32.const 3)))))
              (else (local.set $nargs (i32.const 2)))))
            (else (local.set $nargs (i32.const 1))))
           (if (i32.eqz (ref.eq (local.get $args) (ref.i31 (i32.const 13))))
               (then (return_call $apply-to-non-list (local.get $args))))
           (return_call_ref $kvarargs
                            (local.get $nargs)
                            (local.get $arg0)
                            (local.get $arg1)
                            (local.get $arg2)
                            (if (ref $kvarargs)
                                (ref.test $proc (local.get $arg0))
                                (then (struct.get $proc $func
                                                  (ref.cast $proc (local.get $arg0))))
                                (else (call $get-callee-code (local.get $arg0)
                                            ,@no-source)))))
     (global $apply-primitive (ref eq)
             (struct.new $proc (i32.const 0) (ref.func $apply)))

     ;; TODO: write tests once `fixnum?' or similar is available
     (func $bignum->scm (param $val (ref extern)) (result (ref eq))
           (local $i64 i64)
           (if (call $bignum-is-i64 (local.get $val))
               (then
                (local.set $i64 (call $bignum-get-i64 (local.get $val)))
                (if (i32.and (i64.le_s (i64.const #x-20000000)
                                       (local.get $i64))
                             (i64.le_s (local.get $i64)
                                       (i64.const #x1FFFFFFF)))
                    (then
                     (return (ref.i31
                              (i32.shl
                               (i32.wrap_i64 (local.get $i64))
                               (i32.const 1))))))))
           (struct.new $bignum (i32.const 0) (local.get $val)))

     ;; Helper function for $f64->exact
     (func $decode-f64 (param $frac i64) (param $expt i32) (param $sign i32)
           (result (ref eq) (ref eq))
           (if (i32.eq (local.get $sign) (i32.const 1))
               (then (local.set $frac (i64.mul (local.get $frac) (i64.const -1)))))
           (if (result (ref eq) (ref eq))
               (i32.lt_s (local.get $expt) (i32.const 0))
               (then
                ;; Divide $frac by 1/(2**|expt|). Note: might not be
                ;; fully reduced.
                (call $s64->scm (local.get $frac))
                (call $lsh-fixnum
                      (i32.const 2)
                      (i64.mul (i64.const -1)
                               (i64.extend_i32_s
                                (i32.add
                                 (local.get $expt)
                                 (i32.const 1))))))
               (else
                ;; Multiply $frac by 2**expt
                (call $multiply-integers
                      (call $s64->scm (local.get $frac))
                      (call $lsh-fixnum
                            (i32.const 2)
                            (i64.extend_i32_s
                             (i32.sub (local.get $expt)
                                      (i32.const 1)))))
                ,(fixnum-immediate 1))))

     ;; Callers must ensure that the argument is a rational float (not
     ;; an infinity or NaN).
     ;; TODO: Optimize for conversion of $X to an integer.
     ;; (at least when it can be represeted with an i32 or i64).
     (func $f64->ratio (param $x f64) (result (ref eq) (ref eq))
           (local $bits i64)
           (local $raw-frac i64)        ; raw significand
           (local $raw-expt i32)        ; biased exponent
           (local $sign i32)

           ;; Split $X into three parts:
           ;; - the fraction [Knuth] or significand (52 bits, with an
           ;; implicit leading 1 bit),
           ;; - the exponent (with an offset of 1023; here, since we
           ;; represent the significand as an integer, the offset is
           ;; increased by 52 bits to 1075),
           ;; - and a sign bit.
           ;; Special cases:
           ;; (a) E = 0, F = 0 => (signed) zero;
           ;; (b) E = 0, F /= 0 => subnormal: interpret F as
           ;;     non-normalized with an exponent of -1074;
           ;; (c) E = #x7FF, F = 0 => (signed) infinity;
           ;; (d) E = #x7FF, F /= 0 => NaN.
           ;; Otherwise, $X represents (1+F)*(2**(E-1023)).

           (local.set $bits (i64.reinterpret_f64 (local.get $x)))

           (local.set $raw-frac
                      (i64.and (local.get $bits)
                               (i64.const #xFFFFFFFFFFFFF)))
           (local.set $raw-expt
                      (i32.wrap_i64
                       (i64.and (i64.shr_u (local.get $bits) (i64.const 52))
                                (i64.const #x7FF))))
           (local.set $sign
                      (i32.wrap_i64
                       (i64.shr_u (local.get $bits) (i64.const 63))))

           (if (i32.and (i32.eqz (local.get $raw-expt))
                        (i64.eqz (local.get $raw-frac)))
               (then                    ; zero (E = 0, F = 0)
                (return ,(fixnum-immediate 0)
                        ,(fixnum-immediate 1))))
           (if (i32.eqz (local.get $raw-expt))
               (then                    ; subnormal (E = 0, F /= 0)
                (return_call $decode-f64
                             (local.get $raw-frac)
                             (i32.const -1074)
                             (local.get $sign))))
           (if (i32.eq (local.get $raw-expt) (i32.const #x7FF))
               (then
                ;; nonrational (inf or NaN)
                (call $die0 (string.const "$decode-float bad arg"))
                (unreachable)))

           ;; normal (E /= 0, F /= #xFF)
           (return_call $decode-f64
                        (i64.or (local.get $raw-frac)
                                (i64.const ,(ash 1 52)))
                        (i32.sub (local.get $raw-expt)
                                 (i32.const 1075))
                        (local.get $sign)))

     (func $string-set! (param $str (ref $string)) (param $idx i32)
           (param $ch i32)
           (call $die0 (string.const "$string-set!")) (unreachable))

     ;; cf. compile-test in (hoot compile)
     (func $fixnum? (param $a (ref eq)) (result i32)
           (if (result i32)
               (ref.test i31 (local.get $a))
               (then (i32.eqz
                      (i32.and (i31.get_s (ref.cast i31 (local.get $a)))
                               (i32.const #b1))))
               (else (i32.const 0))))

     (func $fixnum->i32 (param $a (ref i31)) (result i32)
           (i32.shr_s (i31.get_s (local.get $a)) (i32.const 1)))

     (func $fixnum->i64 (param $a (ref i31)) (result i64)
           (i64.extend_i32_s (call $fixnum->i32 (local.get $a))))

     (func $fixnum->f64 (param $a (ref i31)) (result f64)
           (f64.convert_i32_s (call $fixnum->i32 (local.get $a))))

     (func $flonum->f64 (param $a (ref $flonum)) (result f64)
           (struct.get $flonum $val (local.get $a)))

     (func $i32->fixnum (param $a i32) (result (ref i31))
           (ref.i31 (i32.shl (local.get $a) (i32.const 1))))

     (func $i32->bignum (param $a i32) (result (ref eq))
           (struct.new $bignum
                       (i32.const 0)
                       (call $bignum-from-i64
                             (i64.extend_i32_s (local.get $a)))))

     (func $f64-integer? (param $a f64) (result i32)
           ;; Adapted from the f64-int test in (hoot compile). The
           ;; subtraction here detects infinities: (f64.trunc ±inf.0)
           ;; returns an infinity, and the subtraction then produces a
           ;; NaN. (This also detects NaNs correctly, as (f64.trunc
           ;; +nan.0) returns a NaN.)
           (f64.eq (f64.sub (f64.trunc (local.get $a)) (local.get $a))
                   (f64.const 0)))

     (func $fixnum->f64 (param $i i32) (result f64)
           (f64.convert_i32_s (local.get $i)))
     (func $integer->f64 (param $i (ref eq)) (result f64)
           (block
            $i31 (ref i31)
            (br_on_cast $i31 (ref eq) (ref i31) (local.get $i))
            (return_call $bignum->f64
                         (struct.get $bignum $val (ref.cast $bignum))))
           (return_call $fixnum->f64 (i32.shr_s (i31.get_s) (i32.const 1))))
     (func $fraction->f64 (param $num (ref eq)) (param $denom (ref eq))
           (result f64)
           (f64.div (call $integer->f64 (local.get $num))
                    (call $integer->f64 (local.get $denom))))

     (func $i64->scm (param $n i64) (result (ref eq))
           (if (ref eq)
               (i32.and (i64.ge_s (local.get $n) (i64.const ,min-fixnum))
                        (i64.le_s (local.get $n) (i64.const ,max-fixnum)))
               (then (return_call $i32->fixnum (i32.wrap_i64 (local.get $n))))
               (else (struct.new $bignum
                                 (i32.const 0)
                                 (call $bignum-from-i64 (local.get $n))))))
     (func $i32->scm (param $n i32) (result (ref eq))
           (return_call $i64->scm (i64.extend_i32_s (local.get $n))))

     (func $scm->f64 (param $a (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result f64)
           ,@(dispatch-unary-numeric-operation
              '$scm->f64 '$a
              #:real? #t
              #:subr "scm->f64"
              #:dispatch (match-lambda
                           ('flonum '((return)))
                           (tp
                            `((return_call ,(symbol-append '$ tp '->f64)))))))

     ;; Greatest common divisor: v. TAOCP II 4.5.2 Algorithm A (modern
     ;; Euclidean algorithm). TODO: use a modernized version of
     ;; Algorithm B
     (func $gcd-i32 (param $a i32) (param $b i32) (result i32)
           (local $r i32)
           ;; Ensure $a and $b are both positive
           (if (i32.lt_s (local.get $a) (i32.const 0))
               (then (local.set $a (i32.mul (local.get $a) (i32.const -1)))))
           (if (i32.lt_s (local.get $b) (i32.const 0))
               (then (local.set $b (i32.mul (local.get $b) (i32.const -1)))))
           (if (i32.eqz (local.get $a))
               (then (return (local.get $b))))
           (if (i32.eqz (local.get $b))
               (then (return (local.get $a))))
           (block $blk
                  (loop $lp
                    (br_if $blk (i32.eqz (local.get $b)))
                    (local.set $r (i32.rem_u (local.get $a)
                                             (local.get $b)))
                    (local.set $a (local.get $b))
                    (local.set $b (local.get $r))
                    (br $lp)))
           (return (local.get $a)))

     (func $negate-fixnum (param $n i32) (result (ref eq))
           (if (ref eq)
               (i32.eq (local.get $n) (i32.const ,min-fixnum))
               (then (return_call $i32->bignum (i32.const ,(- min-fixnum))))
               (else (return_call $i32->fixnum
                                  (i32.sub (i32.const 0) (local.get $n))))))
     (func $negate-bignum (param $n (ref extern)) (result (ref eq))
           (if (ref eq)
               (call $eq-big-flo (local.get $n) (f64.const ,(1+ max-fixnum)))
               (then ,(fixnum-immediate min-fixnum))
               (else
                (struct.new $bignum
                            (i32.const 0)
                            (call $bignum-mul-i32
                                  (local.get $n) (i32.const -1))))))
     (func $negate-integer (param $n (ref eq)) (result (ref eq))
           (block $i31 (ref i31)
                  (br_on_cast $i31 (ref eq) (ref i31) (local.get $n))
                  (return_call $negate-bignum
                               (struct.get $bignum $val (ref.cast $bignum))))
           (return_call $negate-fixnum (i32.shr_s (i31.get_s) (i32.const 1))))
     (func $negate-flonum (param $n f64) (result (ref eq))
           (struct.new $flonum (i32.const 0) (f64.neg (local.get $n))))
     (func $negate-fraction (param $num (ref eq)) (param $denom (ref eq))
           (result (ref eq))
           (struct.new $fraction (i32.const 0)
                       (call $negate-integer (local.get $num))
                       (call $negate-integer (local.get $denom))))
     (func $negate-complex (param $real f64) (param $imag f64) (result (ref eq))
           (struct.new $complex (i32.const 0)
                       (f64.neg (local.get $real)) (f64.neg (local.get $imag))))
     (func $negate (param $n (ref eq)) (result (ref eq))
           ,@(dispatch-unary-numeric-operation '$negate '$n #:check? #f))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Less-than.
     (func $<-fixnum-fixnum
           (param $a i32) (param $b i32) (result i32)
           (i32.lt_s (local.get $a) (local.get $b)))
     (func $<-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result i32)
           (return_call $lt-fix-big (local.get $a) (local.get $b)))
     (func $<-fixnum-flonum
           (param $a i32) (param $b f64) (result i32)
           (f64.lt (f64.convert_i32_s (local.get $a)) (local.get $b)))
     (func $<-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq)) (result i32)
           (return_call $<-fraction-fraction
                        (call $i32->fixnum (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $<-bignum-fixnum (param $a (ref extern)) (param $b i32)
           (result i32)
           (return_call $lt-big-fix (local.get $a) (local.get $b)))
     (func $<-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result i32)
           (return_call $lt-big-big (local.get $a) (local.get $b)))
     (func $<-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result i32)
           (return_call $lt-big-flo (local.get $a) (local.get $b)))
     (func $<-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq)) (result i32)
           (return_call $<-fraction-fraction
                        (struct.new $bignum (i32.const 0) (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $<-flonum-fixnum
           (param $a f64) (param $b i32) (result i32)
           (f64.lt (local.get $a) (f64.convert_i32_s (local.get $b))))
     (func $<-flonum-bignum
           (param $a f64) (param $b (ref extern)) (result i32)
           (return_call $lt-flo-big (local.get $a) (local.get $b)))
     (func $<-flonum-flonum
           (param $a f64) (param $b f64) (result i32)
           (f64.lt (local.get $a) (local.get $b)))
     (func $<-flonum-fraction
           (param $a f64) (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (if (call $f64-is-finite (local.get $a))
               (then (return_call $<-fraction-fraction
                                  (call $f64->ratio (local.get $a))
                                  (local.get $b-num) (local.get $b-denom))))
           (f64.lt (local.get $a) (f64.const 0)))
     (func $<-fraction-fixnum
           (param $a-num (ref eq)) (param $a-denom (ref eq)) (param $b i32)
           (result i32)
           (return_call $<-fraction-fraction
                        (local.get $a-num)
                        (local.get $a-denom)
                        (call $i32->fixnum (local.get $b))
                        ,(fixnum-immediate 1)))
     (func $<-fraction-bignum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b (ref extern))
           (result i32)
           (return_call $<-fraction-fraction
                        (local.get $a-num)
                        (local.get $a-denom)
                        (struct.new $bignum (i32.const 0) (local.get $b))
                        ,(fixnum-immediate 1)))
     (func $<-fraction-flonum
           (param $a-num (ref eq)) (param $a-denom (ref eq)) (param $b f64)
           (result i32)
           (if (call $f64-is-finite (local.get $b))
               (then
                (return_call $<-fraction-fraction
                             (local.get $a-num) (local.get $a-denom)
                             (call $f64->ratio (local.get $b)))))
           (f64.lt (f64.const 0) (local.get $b)))
     (func $<-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (return_call $<-integers
                        (call $multiply-integers
                              (local.get $a-num) (local.get $b-denom))
                        (call $multiply-integers
                              (local.get $b-num) (local.get $a-denom))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Less-than-or-equal.
     (func $<=-fixnum-fixnum
           (param $a i32) (param $b i32) (result i32)
           (i32.eqz (call $<-fixnum-fixnum (local.get $b) (local.get $a))))
     (func $<=-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result i32)
           (i32.eqz (call $<-bignum-fixnum (local.get $b) (local.get $a))))
     (func $<=-fixnum-flonum
           (param $a i32) (param $b f64) (result i32)
           (f64.le (f64.convert_i32_s (local.get $a)) (local.get $b)))
     (func $<=-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq)) (result i32)
           (i32.eqz
            (call $<-fraction-fixnum
                  (local.get $b-num) (local.get $b-denom) (local.get $a))))
     (func $<=-bignum-fixnum (param $a (ref extern)) (param $b i32)
           (result i32)
           (i32.eqz (call $<-fixnum-bignum (local.get $b) (local.get $a))))
     (func $<=-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result i32)
           (i32.eqz (call $<-bignum-bignum (local.get $b) (local.get $a))))
     (func $<=-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result i32)
           (return_call $le-big-flo (local.get $a) (local.get $b)))
     (func $<=-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq)) (result i32)
           (i32.eqz
            (call $<-fraction-bignum
                  (local.get $b-num) (local.get $b-denom) (local.get $a))))
     (func $<=-flonum-fixnum
           (param $a f64) (param $b i32) (result i32)
           (f64.le (local.get $a) (f64.convert_i32_s (local.get $b))))
     (func $<=-flonum-bignum
           (param $a f64) (param $b (ref extern)) (result i32)
           (return_call $le-flo-big (local.get $a) (local.get $b)))
     (func $<=-flonum-flonum
           (param $a f64) (param $b f64) (result i32)
           (f64.le (local.get $a) (local.get $b)))
     (func $<=-flonum-fraction
           (param $a f64) (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (if (call $f64-is-finite (local.get $a))
               (then
                (return_call $<=-fraction-fraction
                             (call $f64->ratio (local.get $a))
                             (local.get $b-num) (local.get $b-denom))))
           (f64.lt (local.get $a) (f64.const 0)))
     (func $<=-fraction-fixnum
           (param $a-num (ref eq)) (param $a-denom (ref eq)) (param $b i32)
           (result i32)
           (i32.eqz
            (call $<-fixnum-fraction
                  (local.get $b) (local.get $a-num) (local.get $a-denom))))
     (func $<=-fraction-bignum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b (ref extern))
           (result i32)
           (i32.eqz
            (call $<-bignum-fraction
                  (local.get $b) (local.get $a-num) (local.get $a-denom))))
     (func $<=-fraction-flonum
           (param $a-num (ref eq)) (param $a-denom (ref eq)) (param $b f64)
           (result i32)
           (if (call $f64-is-finite (local.get $b))
               (then
                (return_call $<=-fraction-fraction
                             (local.get $a-num) (local.get $a-denom)
                             (call $f64->ratio (local.get $b)))))
           (f64.lt (f64.const 0) (local.get $b)))
     (func $<=-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (i32.eqz
            (call $<-fraction-fraction
                  (local.get $b-num) (local.get $b-denom)
                  (local.get $a-num) (local.get $a-denom))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Numerical equality.
     (func $=-fixnum-fixnum
           (param $a i32) (param $b i32) (result i32)
           (i32.eq (local.get $a) (local.get $b)))
     (func $=-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result i32)
           (i32.const 0))
     (func $=-fixnum-flonum
           (param $a i32) (param $b f64) (result i32)
           (return_call $=-flonum-fixnum (local.get $b) (local.get $a)))
     (func $=-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq)) (result i32)
           (i32.const 0))
     (func $=-fixnum-complex (param $a i32)
           (param $b-real f64) (param $b-imag f64) (result i32)
           (return_call $=-complex-fixnum
                        (local.get $b-real) (local.get $b-imag) (local.get $a)))
     (func $=-bignum-fixnum (param $a (ref extern)) (param $b i32)
           (result i32)
           (i32.const 0))
     (func $=-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result i32)
           (call $eq-big-big (local.get $a) (local.get $b)))
     (func $=-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result i32)
           (return_call $=-flonum-bignum (local.get $b) (local.get $a)))
     (func $=-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq)) (result i32)
           (i32.const 0))
     (func $=-bignum-complex (param $a (ref extern))
           (param $b-real f64) (param $b-imag f64) (result i32)
           (return_call $=-complex-bignum
                        (local.get $b-real) (local.get $b-imag) (local.get $a)))
     (func $=-flonum-fixnum
           (param $a f64) (param $b i32) (result i32)
           (f64.eq (local.get $a) (f64.convert_i32_s (local.get $b))))
     (func $=-flonum-bignum
           (param $a f64) (param $b (ref extern)) (result i32)
           (return_call $eq-big-flo (local.get $b) (local.get $a)))
     (func $=-flonum-flonum
           (param $a f64) (param $b f64) (result i32)
           (f64.eq (local.get $a) (local.get $b)))
     (func $=-flonum-fraction
           (param $a f64) (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (return_call $=-fraction-flonum
                        (local.get $b-num) (local.get $b-denom) (local.get $a)))
     (func $=-flonum-complex (param $a f64)
           (param $b-real f64) (param $b-imag f64) (result i32)
           (return_call $=-complex-flonum
                        (local.get $b-real) (local.get $b-imag) (local.get $a)))
     (func $=-fraction-fixnum
           (param $a-num (ref eq)) (param $a-denom (ref eq)) (param $b i32)
           (result i32)
           (i32.const 0))
     (func $=-fraction-bignum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b (ref extern))
           (result i32)
           (i32.const 0))
     (func $=-fraction-flonum
           (param $a-num (ref eq)) (param $a-denom (ref eq)) (param $b f64)
           (result i32)
           (if (call $f64-is-finite (local.get $b))
               (then
                (return_call $=-fraction-fraction
                             (local.get $a-num) (local.get $a-denom)
                             (call $f64->ratio (local.get $b)))))
           (i32.const 0))
     (func $=-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (return_call $=-integers
                        (call $multiply-integers
                              (local.get $a-num) (local.get $b-denom))
                        (call $multiply-integers
                              (local.get $b-num) (local.get $a-denom))))

     (func $=-fraction-complex
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-real f64) (param $b-imag f64)
           (result i32)
           (return_call $=-complex-fraction
                        (local.get $b-real) (local.get $b-imag)
                        (local.get $a-num) (local.get $a-denom)))
     (func $=-complex-fixnum
           (param $a-real f64) (param $a-imag f64) (param $b i32)
           (result i32)
           (if (f64.eq (local.get $a-imag) (f64.const 0.0))
               (then (return_call $=-flonum-fixnum
                                  (local.get $a-real) (local.get $b))))
           (i32.const 0))
     (func $=-complex-bignum
           (param $a-real f64) (param $a-imag f64) (param $b (ref extern))
           (result i32)
           (if (f64.eq (local.get $a-imag) (f64.const 0.0))
               (then (return_call $=-flonum-bignum
                                  (local.get $a-real) (local.get $b))))
           (i32.const 0))
     (func $=-complex-flonum
           (param $a-real f64) (param $a-imag f64) (param $b f64)
           (result i32)
           (return_call $=-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (local.get $b) (f64.const 0.0)))
     (func $=-complex-fraction
           (param $a-real f64) (param $a-imag f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result i32)
           (if (f64.eq (local.get $a-imag) (f64.const 0.0))
               (then (return_call $=-flonum-fraction
                                  (local.get $a-real)
                                  (local.get $b-num) (local.get $b-denom))))
           (i32.const 0))
     (func $=-complex-complex
           (param $a-real f64) (param $a-imag f64)
           (param $b-real f64) (param $b-imag f64)
           (result i32)
           (i32.and (f64.eq (local.get $a-real) (local.get $b-real))
                    (f64.eq (local.get $a-imag) (local.get $b-imag))))

     (func $<-integers (param $a (ref eq)) (param $b (ref eq))
           (result i32)
           ,@(dispatch-binary-numeric-operation
              '$< '$a '$b #:exact? #t #:integer? #t #:check? #f))
     (func $=-integers (param $a (ref eq)) (param $b (ref eq))
           (result i32)
           ,@(dispatch-binary-numeric-operation
              '$= '$a '$b #:exact? #t #:integer? #t #:check? #f))

     (func $< (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result i32)
           ,@(dispatch-binary-numeric-operation '$< '$a '$b
                                                #:real? #t #:subr "<"))

     (func $<= (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result i32)
           ,@(dispatch-binary-numeric-operation '$<= '$a '$b
                                                #:real? #t #:subr "<="))

     (func $= (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result i32)
           ,@(dispatch-binary-numeric-operation '$= '$a '$b #:subr "="))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; eqv? between heap numbers
     (func $heap-numbers-equal? (param $a (ref eq)) (param $b (ref eq))
           (result i32)
           (local $tmp-fraction (ref $fraction))
           (local $tmp-complex-a (ref $complex))
           (local $tmp-complex-b (ref $complex))
           (block
            $nope (ref eq)
            (block
             $complex (ref $complex)
             (block
              $fraction (ref $fraction)
              (block
               $flonum (ref $flonum)
               (local.get $a)
               (br_on_cast $flonum (ref eq) (ref $flonum))
               (br_on_cast $fraction (ref eq) (ref $fraction))
               (br_on_cast $complex (ref eq) (ref $complex))
               (struct.get $bignum $val (ref.cast $bignum))
               (local.get $b)
               (br_on_cast_fail $nope (ref eq) (ref $bignum))
               (struct.get $bignum $val)
               (return_call $=-bignum-bignum))
              (struct.get $flonum $val)
              (i64.reinterpret_f64)
              (br_on_cast_fail $nope (ref eq) (ref $flonum) (local.get $b))
              (struct.get $flonum $val)
              (i64.reinterpret_f64)
              (return (i64.eq)))
             (struct.get $fraction $num (local.tee $tmp-fraction))
             (struct.get $fraction $denom (local.get $tmp-fraction))
             (br_on_cast_fail $nope (ref eq) (ref $fraction) (local.get $b))
             (struct.get $fraction $num (local.tee $tmp-fraction))
             (struct.get $fraction $denom (local.get $tmp-fraction))
             (return_call $=-fraction-fraction))
            (local.set $tmp-complex-a)
            (br_on_cast_fail $nope (ref eq) (ref $complex) (local.get $b))
            (local.set $tmp-complex-b)
            (return
             (i32.and
              (i64.eq
               (i64.reinterpret_f64
                (struct.get $complex $real (local.get $tmp-complex-a)))
               (i64.reinterpret_f64
                (struct.get $complex $real (local.get $tmp-complex-b))))
              (i64.eq
               (i64.reinterpret_f64
                (struct.get $complex $imag (local.get $tmp-complex-a)))
               (i64.reinterpret_f64
                (struct.get $complex $imag (local.get $tmp-complex-b)))))))
           (return (i32.const 0)))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Addition
     (func $add-fixnum-fixnum (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i32->scm (i32.add (local.get $a) (local.get $b))))
     (func $add-fixnum-bignum (param $a i32) (param $b (ref extern))
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-add-i32 (local.get $b) (local.get $a))))
     (func $add-fixnum-flonum (param $a i32) (param $b f64)
           (result (ref eq))
           (return_call $add-flonum-flonum
                        (f64.convert_i32_s (local.get $a))
                        (local.get $b)))
     (func $add-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $add-fraction-fraction
                        (call $i32->fixnum (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $add-fixnum-complex (param $a i32)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $add-complex-complex
                        (f64.convert_i32_s (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $add-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-add (local.get $a) (local.get $b))))
     (func $add-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result (ref eq))
           (return_call $add-flonum-flonum
                        (call $bignum->f64 (local.get $a))
                        (local.get $b)))
     (func $add-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $add-fraction-fraction
                        (struct.new $bignum (i32.const 0) (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $add-bignum-complex (param $a (ref extern))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $add-complex-complex
                        (call $bignum->f64 (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $add-flonum-flonum (param $a f64) (param $b f64)
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.add (local.get $a) (local.get $b))))
     (func $add-flonum-fraction (param $a f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $add-flonum-flonum
                        (local.get $a)
                        (call $fraction->f64
                              (local.get $b-num) (local.get $b-denom))))
     (func $add-flonum-complex (param $a f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $add-complex-complex
                        (local.get $a) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $add-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $divide-integers
                        (call $add-integers
                              (call $multiply-integers
                                    (local.get $a-num)
                                    (local.get $b-denom))
                              (call $multiply-integers
                                    (local.get $b-num)
                                    (local.get $a-denom)))
                        (call $multiply-integers
                              (local.get $a-denom) (local.get $b-denom))))
     (func $add-fraction-complex
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $add-complex-complex
                        (call $fraction->f64
                              (local.get $a-num) (local.get $a-denom))
                        (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $add-complex-complex
           (param $a-real f64) (param $a-imag f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (struct.new $complex
                       (i32.const 0)
                       (f64.add (local.get $a-real) (local.get $b-real))
                       (f64.add (local.get $a-imag) (local.get $b-imag))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Subtraction
     (func $sub-fixnum-fixnum (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i32->scm (i32.sub (local.get $a) (local.get $b))))
     (func $sub-fixnum-bignum (param $a i32) (param $b (ref extern))
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-add-i32
                              (call $bignum-mul-i32 (local.get $b) (i32.const -1))
                              (local.get $a))))
     (func $sub-fixnum-flonum (param $a i32) (param $b f64)
           (result (ref eq))
           (return_call $add-fixnum-flonum (local.get $a)
                        (f64.neg (local.get $b))))
     (func $sub-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $sub-fraction-fraction
                        (call $i32->fixnum (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $sub-fixnum-complex (param $a i32)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $sub-complex-complex
                        (f64.convert_i32_s (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $sub-bignum-fixnum (param $a (ref extern)) (param $b i32)
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-sub-i32 (local.get $a) (local.get $b))))
     (func $sub-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-sub (local.get $a) (local.get $b))))
     (func $sub-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result (ref eq))
           (return_call $add-bignum-flonum (local.get $a)
                        (f64.neg (local.get $b))))
     (func $sub-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $sub-fraction-fraction
                        (struct.new $bignum (i32.const 0) (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $sub-bignum-complex (param $a (ref extern))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $sub-complex-complex
                        (call $bignum->f64 (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $sub-flonum-fixnum (param $a f64) (param $b i32)
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.sub (local.get $a)
                                (f64.convert_i32_s (local.get $b)))))
     (func $sub-flonum-bignum (param $a f64) (param $b (ref extern))
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.sub (local.get $a)
                                (call $bignum->f64 (local.get $b)))))
     (func $sub-flonum-flonum (param $a f64) (param $b f64)
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.sub (local.get $a) (local.get $b))))
     (func $sub-flonum-fraction (param $a f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.sub
                        (local.get $a)
                        (call $fraction->f64
                              (local.get $b-num) (local.get $b-denom)))))
     (func $sub-flonum-complex (param $a f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $sub-complex-complex
                        (local.get $a) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $sub-fraction-fixnum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b i32)
           (result (ref eq))
           (return_call $sub-fraction-fraction
                        (local.get $a-num)
                        (local.get $a-denom)
                        (call $i32->fixnum (local.get $b))
                        ,(fixnum-immediate 1)))
     (func $sub-fraction-bignum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b (ref extern))
           (result (ref eq))
           (return_call $sub-fraction-fraction
                        (local.get $a-num)
                        (local.get $a-denom)
                        (struct.new $bignum (i32.const 0) (local.get $b))
                        ,(fixnum-immediate 1)))
     (func $sub-fraction-flonum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b f64)
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.sub
                        (call $fraction->f64
                              (local.get $a-num) (local.get $a-denom))
                        (local.get $b))))
     (func $sub-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $divide-integers
                        (call $sub-integers
                              (call $multiply-integers
                                    (local.get $a-num)
                                    (local.get $b-denom))
                              (call $multiply-integers
                                    (local.get $b-num)
                                    (local.get $a-denom)))
                        (call $multiply-integers
                              (local.get $a-denom) (local.get $b-denom))))
     (func $sub-fraction-complex
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $sub-complex-complex
                        (call $fraction->f64
                              (local.get $a-num) (local.get $a-denom))
                        (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $sub-complex-fixnum
           (param $a-real f64) (param $a-imag f64)
           (param $b i32)
           (result (ref eq))
           (return_call $sub-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (f64.convert_i32_s (local.get $b)) (f64.const 0)))
     (func $sub-complex-bignum
           (param $a-real f64) (param $a-imag f64)
           (param $b (ref extern))
           (result (ref eq))
           (return_call $sub-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (call $bignum->f64 (local.get $b)) (f64.const 0)))
     (func $sub-complex-flonum
           (param $a-real f64) (param $a-imag f64)
           (param $b f64)
           (result (ref eq))
           (return_call $sub-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (local.get $b) (f64.const 0)))
     (func $sub-complex-fraction
           (param $a-real f64) (param $a-imag f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $sub-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (call $fraction->f64
                              (local.get $b-num) (local.get $b-denom))
                        (f64.const 0)))
     (func $sub-complex-complex
           (param $a-real f64) (param $a-imag f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (struct.new $complex
                       (i32.const 0)
                       (f64.sub (local.get $a-real) (local.get $b-real))
                       (f64.sub (local.get $a-imag) (local.get $b-imag))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Multiplication
     (func $mul-fixnum-fixnum (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i64->scm
                        (i64.mul (i64.extend_i32_s (local.get $a))
                                 (i64.extend_i32_s (local.get $b)))))
     (func $mul-fixnum-bignum (param $a i32) (param $b (ref extern))
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-mul-i32 (local.get $b) (local.get $a))))
     (func $mul-fixnum-flonum (param $a i32) (param $b f64)
           (result (ref eq))
           (return_call $mul-flonum-flonum
                        (f64.convert_i32_s (local.get $a))
                        (local.get $b)))
     (func $mul-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $mul-fraction-fraction
                        (call $i32->fixnum (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $mul-fixnum-complex (param $a i32)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $mul-complex-complex
                        (f64.convert_i32_s (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $mul-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-mul (local.get $a) (local.get $b))))
     (func $mul-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result (ref eq))
           (return_call $mul-flonum-flonum
                        (call $bignum->f64 (local.get $a))
                        (local.get $b)))
     (func $mul-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $mul-fraction-fraction
                        (struct.new $bignum (i32.const 0) (local.get $a))
                        ,(fixnum-immediate 1)
                        (local.get $b-num)
                        (local.get $b-denom)))
     (func $mul-bignum-complex (param $a (ref extern))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $mul-complex-complex
                        (call $bignum->f64 (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $mul-flonum-flonum (param $a f64) (param $b f64)
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.mul (local.get $a) (local.get $b))))
     (func $mul-flonum-fraction (param $a f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $mul-flonum-flonum
                        (local.get $a)
                        (call $fraction->f64
                              (local.get $b-num) (local.get $b-denom))))
     (func $mul-flonum-complex (param $a f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $mul-complex-complex
                        (local.get $a) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $mul-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $divide-integers
                        (call $multiply-integers
                              (local.get $a-num) (local.get $b-num))
                        (call $multiply-integers
                              (local.get $a-denom) (local.get $b-denom))))
     (func $mul-fraction-complex
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $mul-complex-complex
                        (call $fraction->f64
                              (local.get $a-num) (local.get $a-denom))
                        (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $mul-complex-complex
           (param $a-real f64) (param $a-imag f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (struct.new $complex
                       (i32.const 0)
                       (f64.sub (f64.mul (local.get $a-real) (local.get $b-real))
                                (f64.mul (local.get $a-imag) (local.get $b-imag)))
                       (f64.add (f64.mul (local.get $a-real) (local.get $b-imag))
                                (f64.mul (local.get $a-imag) (local.get $b-real)))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Division
     (func $div-fixnum-fixnum (param $a i32) (param $b i32) (result (ref eq))
           (local $gcd i32)
           (if (i32.eq (local.get $a) (i32.const 0))
               (then (return ,(fixnum-immediate 0))))
           (if (i32.eq (local.get $b) (i32.const 1))
               (then (return_call $i32->fixnum (local.get $a))))
           (if (i32.lt_s (local.get $b) (i32.const 0))
               (then (return_call $divide-integers
                                  (call $negate-fixnum (local.get $a))
                                  (call $negate-fixnum (local.get $b)))))
           (if (i32.eqz (i32.rem_s (local.get $a) (local.get $b)))
               (then (return_call $i32->fixnum
                                  (i32.div_s (local.get $a) (local.get $b)))))
           (local.set $gcd (call $gcd-i32 (local.get $a) (local.get $b)))
           (struct.new $fraction
                       (i32.const 0)
                       (call $i32->fixnum
                             (i32.div_s (local.get $a) (local.get $gcd)))
                       (call $i32->fixnum
                             (i32.div_s (local.get $b) (local.get $gcd)))))
     (func $div-fixnum-bignum (param $a i32) (param $b (ref extern))
           (result (ref eq))
           (if (i32.eq (local.get $a) (i32.const 0))
               (then (return ,(fixnum-immediate 0))))
           (return_call $div-bignum-bignum
                        (call $bignum-from-i32 (local.get $a))
                        (local.get $b)))
     (func $div-fixnum-flonum (param $a i32) (param $b f64)
           (result (ref eq))
           (return_call $div-flonum-flonum (f64.convert_i32_s (local.get $a))
                        (local.get $b)))
     (func $div-fixnum-fraction (param $a i32)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $divide-integers
                        (call $multiply-integers
                              (call $i32->fixnum (local.get $a))
                              (local.get $b-denom))
                        (local.get $b-num)))
     (func $div-fixnum-complex (param $a i32)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $div-complex-complex
                        (f64.convert_i32_s (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $div-bignum-fixnum (param $a (ref extern)) (param $b i32)
           (result (ref eq))
           (if (i32.eq (local.get $b) (i32.const 1))
               (then (return (struct.new $bignum (i32.const 0) (local.get $a)))))
           (if (i32.lt_s (local.get $b) (i32.const 0))
               (then (return_call $divide-integers
                                  (call $negate-bignum (local.get $a))
                                  (call $negate-fixnum (local.get $b)))))
           (return_call $div-bignum-bignum
                        (local.get $a) (call $bignum-from-i32 (local.get $b))))
     (func $div-bignum-bignum (param $a (ref extern)) (param $b (ref extern))
           (result (ref eq))
           (local $gcd (ref extern))
           (if (call $lt-big-fix (local.get $b) (i32.const 0))
               (then (return_call $divide-integers
                                  (call $negate-bignum (local.get $a))
                                  (call $negate-bignum (local.get $b)))))
           (local.set $gcd (call $bignum-gcd (local.get $a) (local.get $b)))
           (if (call $eq-big-big (local.get $gcd) (local.get $b))
               (then (return
                      (call $bignum->scm
                            (call $bignum-quo (local.get $a) (local.get $b))))))
           (struct.new $fraction
                       (i32.const 0)
                       (call $bignum->scm
                             (call $bignum-quo (local.get $a) (local.get $gcd)))
                       (call $bignum->scm
                             (call $bignum-quo (local.get $b) (local.get $gcd)))))
     (func $div-bignum-flonum (param $a (ref extern)) (param $b f64)
           (result (ref eq))
           (return_call $div-flonum-flonum (call $bignum->f64 (local.get $a))
                        (local.get $b)))
     (func $div-bignum-fraction (param $a (ref extern))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $divide-integers
                        (call $multiply-integers
                              (struct.new $bignum (i32.const 0) (local.get $a))
                              (local.get $b-denom))
                        (local.get $b-num)))
     (func $div-bignum-complex (param $a (ref extern))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $div-complex-complex
                        (call $bignum->f64 (local.get $a)) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $div-flonum-fixnum (param $a f64) (param $b i32)
           (result (ref eq))
           (return_call $div-flonum-flonum (local.get $a)
                        (f64.convert_i32_s (local.get $b))))
     (func $div-flonum-bignum (param $a f64) (param $b (ref extern))
           (result (ref eq))
           (return_call $div-flonum-flonum
                        (local.get $a)
                        (call $bignum->f64 (local.get $b))))
     (func $div-flonum-flonum (param $a f64) (param $b f64)
           (result (ref eq))
           (struct.new $flonum
                       (i32.const 0)
                       (f64.div (local.get $a) (local.get $b))))
     (func $div-flonum-fraction (param $a f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $div-flonum-flonum
                        (local.get $a)
                        (call $fraction->f64
                              (local.get $b-num) (local.get $b-denom))))
     (func $div-flonum-complex (param $a f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $div-complex-complex
                        (local.get $a) (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $div-fraction-fixnum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b i32)
           (result (ref eq))
           (return_call $divide-integers
                        (local.get $a-num)
                        (call $multiply-integers
                              (local.get $a-denom)
                              (call $i32->fixnum (local.get $b)))))
     (func $div-fraction-bignum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b (ref extern))
           (result (ref eq))
           (return_call $divide-integers
                        (local.get $a-num)
                        (call $multiply-integers
                              (local.get $a-denom)
                              (struct.new $bignum (i32.const 0) (local.get $b)))))
     (func $div-fraction-flonum
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b f64)
           (result (ref eq))
           (return_call $div-flonum-flonum
                        (call $fraction->f64
                              (local.get $a-num) (local.get $a-denom))
                        (local.get $b)))
     (func $div-fraction-fraction
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $divide-integers
                        (call $multiply-integers
                              (local.get $a-num)
                              (local.get $b-denom))
                        (call $multiply-integers
                              (local.get $b-num)
                              (local.get $a-denom))))
     (func $div-fraction-complex
           (param $a-num (ref eq)) (param $a-denom (ref eq))
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (return_call $div-complex-complex
                        (call $fraction->f64
                              (local.get $a-num) (local.get $a-denom))
                        (f64.const 0)
                        (local.get $b-real) (local.get $b-imag)))
     (func $div-complex-fixnum
           (param $a-real f64) (param $a-imag f64)
           (param $b i32)
           (result (ref eq))
           (return_call $div-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (f64.convert_i32_s (local.get $b)) (f64.const 0)))
     (func $div-complex-bignum
           (param $a-real f64) (param $a-imag f64)
           (param $b (ref extern))
           (result (ref eq))
           (return_call $div-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (call $bignum->f64 (local.get $b)) (f64.const 0)))
     (func $div-complex-flonum
           (param $a-real f64) (param $a-imag f64)
           (param $b f64)
           (result (ref eq))
           (return_call $div-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (local.get $b) (f64.const 0)))
     (func $div-complex-fraction
           (param $a-real f64) (param $a-imag f64)
           (param $b-num (ref eq)) (param $b-denom (ref eq))
           (result (ref eq))
           (return_call $div-complex-complex
                        (local.get $a-real) (local.get $a-imag)
                        (call $fraction->f64
                              (local.get $b-num) (local.get $b-denom))
                        (f64.const 0)))
     (func $div-complex-complex
           (param $a-real f64) (param $a-imag f64)
           (param $b-real f64) (param $b-imag f64)
           (result (ref eq))
           (local $d f64)
           (local.set $d (f64.add (f64.mul (local.get $b-real)
                                           (local.get $b-real))
                                  (f64.mul (local.get $b-imag)
                                           (local.get $b-imag))))
           (struct.new $complex
                       (i32.const 0)
                       (f64.div (f64.add (f64.mul (local.get $a-real)
                                                  (local.get $b-real))
                                         (f64.mul (local.get $a-imag)
                                                  (local.get $b-imag)))
                                (local.get $d))
                       (f64.div (f64.sub (f64.mul (local.get $a-imag)
                                                  (local.get $b-real))
                                         (f64.mul (local.get $a-real)
                                                  (local.get $b-imag)))
                                (local.get $d))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Quotient
     (func $quo-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (if (i32.eq (local.get $b) (i32.const -1))
               (then (return_call $negate-fixnum (local.get $a))))
           (call $i32->fixnum (i32.div_s (local.get $a) (local.get $b))))
     (func $quo-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           ,(fixnum-immediate 0))
     (func $quo-fixnum-flonum
           (param $a i32) (param $b f64) (result (ref eq))
           (return_call $quo-flonum-flonum
                        (f64.convert_i32_s (local.get $a)) (local.get $b)))
     (func $quo-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (if (i32.eq (local.get $b) (i32.const -1))
               (then (return_call $negate-bignum (local.get $a))))
           (call $bignum->scm
                 (call $bignum-quo
                       (local.get $a) (call $bignum-from-i32 (local.get $b)))))
     (func $quo-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (call $bignum->scm
                 (call $bignum-quo (local.get $a) (local.get $b))))
     (func $quo-bignum-flonum
           (param $a (ref extern)) (param $b f64) (result (ref eq))
           (return_call $quo-flonum-flonum
                        (call $bignum->f64 (local.get $a)) (local.get $b)))
     (func $quo-flonum-fixnum
           (param $a f64) (param $b i32) (result (ref eq))
           (return_call $quo-flonum-flonum
                        (local.get $a) (f64.convert_i32_s (local.get $b))))
     (func $quo-flonum-bignum
           (param $a f64) (param $b (ref extern)) (result (ref eq))
           (return_call $quo-flonum-flonum
                        (local.get $a) (call $bignum->f64 (local.get $b))))
     (func $quo-flonum-flonum
           (param $a f64) (param $b f64) (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (f64.trunc (f64.div (local.get $a) (local.get $b)))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Remainder
     (func $rem-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (call $i32->fixnum (i32.rem_s (local.get $a) (local.get $b))))
     (func $rem-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           (return_call $rem-bignum-bignum
                        (call $bignum-from-i32 (local.get $a))
                        (local.get $b)))
     (func $rem-fixnum-flonum
           (param $a i32) (param $b f64) (result (ref eq))
           (return_call $rem-flonum-flonum
                        (f64.convert_i32_s (local.get $a)) (local.get $b)))
     (func $rem-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (return_call $rem-bignum-bignum
                        (local.get $a)
                        (call $bignum-from-i32 (local.get $b))))
     (func $rem-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (call $bignum->scm
                 (call $bignum-rem (local.get $a) (local.get $b))))
     (func $rem-bignum-flonum
           (param $a (ref extern)) (param $b f64) (result (ref eq))
           (return_call $rem-flonum-flonum
                        (call $bignum->f64 (local.get $a)) (local.get $b)))
     (func $rem-flonum-fixnum
           (param $a f64) (param $b i32) (result (ref eq))
           (return_call $rem-flonum-flonum
                        (local.get $a) (f64.convert_i32_s (local.get $b))))
     (func $rem-flonum-bignum
           (param $a f64) (param $b (ref extern)) (result (ref eq))
           (return_call $rem-flonum-flonum
                        (local.get $a) (call $bignum->f64 (local.get $b))))
     (func $rem-flonum-flonum
           (param $a f64) (param $b f64) (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (f64.sub (local.get $a)
                                (f64.mul (local.get $b)
                                         (f64.trunc
                                          (f64.div (local.get $a)
                                                   (local.get $b)))))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Modulo
     (func $mod-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (local $res i32)
           (local.set $res (i32.rem_s (local.get $a) (local.get $b)))
           (if (i32.ne (i32.lt_s (local.get $b) (i32.const 0))
                       (i32.lt_s (local.get $res) (i32.const 0)))
               (then
                (return_call $i32->scm
                             (i32.add (local.get $res) (local.get $b)))))
           (return_call $i32->fixnum (local.get $res)))
     (func $mod-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           (return_call $mod-bignum-bignum
                        (call $bignum-from-i32 (local.get $a))
                        (local.get $b)))
     (func $mod-fixnum-flonum
           (param $a i32) (param $b f64) (result (ref eq))
           (return_call $mod-flonum-flonum
                        (f64.convert_i32_s (local.get $a)) (local.get $b)))
     (func $mod-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (return_call $mod-bignum-bignum
                        (local.get $a)
                        (call $bignum-from-i32 (local.get $b))))
     (func $mod-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (call $bignum->scm
                 (call $bignum-mod (local.get $a) (local.get $b))))
     (func $mod-bignum-flonum
           (param $a (ref extern)) (param $b f64) (result (ref eq))
           (return_call $mod-flonum-flonum
                        (call $bignum->f64 (local.get $a)) (local.get $b)))
     (func $mod-flonum-fixnum
           (param $a f64) (param $b i32) (result (ref eq))
           (return_call $mod-flonum-flonum
                        (local.get $a) (f64.convert_i32_s (local.get $b))))
     (func $mod-flonum-bignum
           (param $a f64) (param $b (ref extern)) (result (ref eq))
           (return_call $mod-flonum-flonum
                        (local.get $a) (call $bignum->f64 (local.get $b))))
     (func $mod-flonum-flonum
           (param $a f64) (param $b f64) (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (f64.sub (local.get $a)
                                (f64.mul (local.get $b)
                                         (f64.floor
                                          (f64.div (local.get $a)
                                                   (local.get $b)))))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Bitwise AND.
     (func $logand-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i32->fixnum (i32.and (local.get $a (local.get $b)))))
     (func $logand-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           (return_call
            $i32->fixnum
            (i32.wrap_i64
             (call $bignum-get-i64
                   (call $bignum-logand-i32 (local.get $b) (local.get $a))))))
     (func $logand-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (return_call $logand-fixnum-bignum (local.get $b) (local.get $a)))
     (func $logand-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-logand-bignum (local.get $a) (local.get $b))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Bitwise OR.
     (func $logior-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i32->fixnum (i32.or (local.get $a (local.get $b)))))
     (func $logior-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-logior-i32 (local.get $b) (local.get $a))))
     (func $logior-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (return_call $logior-fixnum-bignum (local.get $b) (local.get $a)))
     (func $logior-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-logior-bignum (local.get $a) (local.get $b))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Bitwise XOR.
     (func $logxor-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i32->fixnum (i32.xor (local.get $a (local.get $b)))))
     (func $logxor-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-logxor-i32 (local.get $b) (local.get $a))))
     (func $logxor-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (return_call $logxor-fixnum-bignum (local.get $b) (local.get $a)))
     (func $logxor-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-logxor-bignum (local.get $a) (local.get $b))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Bitwise subtraction.
     (func $logsub-fixnum-fixnum
           (param $a i32) (param $b i32) (result (ref eq))
           (return_call $i32->fixnum
                        (i32.and (local.get $a)
                                 (i32.xor (local.get $b) (i32.const -1)))))
     (func $logsub-fixnum-bignum
           (param $a i32) (param $b (ref extern)) (result (ref eq))
           (return_call $logand-fixnum-bignum (local.get $a)
                        (call $bignum-logxor-i32
                              (local.get $b) (i32.const -1))))
     (func $logsub-bignum-fixnum
           (param $a (ref extern)) (param $b i32) (result (ref eq))
           (return_call $logand-bignum-fixnum (local.get $a)
                        (i32.xor (local.get $b) (i32.const -1))))
     (func $logsub-bignum-bignum
           (param $a (ref extern)) (param $b (ref extern)) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-logand-bignum
                              (local.get $a)
                              (call $bignum-logxor-i32
                                    (local.get $b) (i32.const -1)))))

     ,@(commutative-arithmetic-operation-implementations '$add)
     ,@(commutative-arithmetic-operation-implementations '$mul)

     (func $fixnum-add (param $a i32) (param $b i32) (result (ref eq))
           (return_call $add-fixnum-fixnum
                        (i32.shr_s (local.get $a) (i32.const 1))
                        (i32.shr_s (local.get $b) (i32.const 1))))
     (func $fixnum-sub (param $a i32) (param $b i32) (result (ref eq))
           (return_call $sub-fixnum-fixnum
                        (i32.shr_s (local.get $a) (i32.const 1))
                        (i32.shr_s (local.get $b) (i32.const 1))))
     (func $fixnum-mul (param $a i32) (param $b i32) (result (ref eq))
           (return_call $mul-fixnum-fixnum
                        (i32.shr_s (local.get $a) (i32.const 1))
                        (i32.shr_s (local.get $b) (i32.const 1))))

     (func $add-integers (param $a (ref eq)) (param $b (ref eq))
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$add '$a '$b #:check? #f #:exact? #t #:integer? #t))
     (func $sub-integers (param $a (ref eq)) (param $b (ref eq))
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$sub '$a '$b #:check? #f #:exact? #t #:integer? #t))
     (func $multiply-integers (param $a (ref eq)) (param $b (ref eq))
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$mul '$a '$b #:check? #f #:exact? #t #:integer? #t))
     (func $divide-integers (param $a (ref eq)) (param $b (ref eq))
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$div '$a '$b #:check? #f #:exact? #t #:integer? #t))
     (func $rem-integers (param $a (ref eq)) (param $b (ref eq))
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$rem '$a '$b #:check? #f #:exact? #t #:integer? #t))

     (func $add (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation '$add '$a '$b #:subr "+"))

     (func $sub (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation '$sub '$a '$b #:subr "-"))

     (func $mul (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation '$mul '$a '$b #:subr "*"))

     (func $div (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$div '$a '$b #:subr "/"
              #:prelude `((if (ref.eq (local.get $b) ,(fixnum-immediate 0))
                              (then
                               (call $raise-runtime-error-with-message
                                     (string.const "division by zero")
                                     ,@propagate-source)
                               (unreachable))))))

     (func $quo (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$quo '$a '$b #:subr "quotient" #:integer? #t
              #:prelude `((if (ref.eq (local.get $b) ,(fixnum-immediate 0))
                              (then
                               (call $raise-runtime-error-with-message
                                     (string.const "division by zero")
                                     ,@propagate-source)
                               (unreachable))))))

     (func $rem (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$rem '$a '$b #:subr "remainder" #:integer? #t
              #:prelude `((if (ref.eq (local.get $b) ,(fixnum-immediate 0))
                              (then
                               (call $raise-runtime-error-with-message
                                     (string.const "division by zero")
                                     ,@propagate-source)
                               (unreachable))))))

     (func $mod (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$mod '$a '$b #:subr "modulo" #:integer? #t
              #:prelude `((if (ref.eq (local.get $b) ,(fixnum-immediate 0))
                              (then
                               (call $raise-runtime-error-with-message
                                     (string.const "division by zero")
                                     ,@propagate-source)
                               (unreachable))))))

     (func $logand (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$logand '$a '$b #:subr "logand" #:exact? #t #:integer? #t))

     (func $logior (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$logior '$a '$b #:subr "logior" #:exact? #t #:integer? #t))

     (func $logxor (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$logxor '$a '$b #:subr "logxor" #:exact? #t #:integer? #t))

     (func $logsub (param $a (ref eq)) (param $b (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-binary-numeric-operation
              '$logsub '$a '$b #:subr "logsub" #:exact? #t #:integer? #t))

     (func $rsh-fixnum (param $a i32) (param $b i64) (result (ref eq))
           (return_call $i32->fixnum
                        (i32.shr_s (local.get $a)
                                   (if i32
                                       (i64.lt_u (local.get $b) (i64.const 30))
                                       (then (i32.wrap_i64 (local.get $b)))
                                       (else (i32.const 30))))))
     (func $rsh-bignum (param $a (ref extern)) (param $b i64) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-rsh (local.get $a) (local.get $b))))
     (func $rsh (param $a (ref eq)) (param $b i64)
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-unary-numeric-operation
              '$rsh '$a #:subr "rsh" #:exact-integer? #t
              #:dispatch (lambda (tp)
                           `((local.get $b)
                             (return_call ,(symbol-append '$rsh- tp))))))

     (func $lsh-fixnum
           (param $a i32) (param $b i64) (result (ref eq))
           (return_call $bignum->scm
                        (call $i32-lsh (local.get $a) (local.get $b))))
     (func $lsh-bignum
           (param $a (ref extern)) (param $b i64) (result (ref eq))
           (return_call $bignum->scm
                        (call $bignum-lsh (local.get $a) (local.get $b))))

     (func $lsh (param $a (ref eq)) (param $b i64)
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-unary-numeric-operation
              '$lsh '$a #:subr "lsh" #:exact-integer? #t
              #:dispatch (lambda (tp)
                           `((local.get $b)
                             (return_call ,(symbol-append '$lsh- tp))))))

     (func $inexact-fixnum (param $i i32) (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (call $fixnum->f64 (local.get $i))))
     (func $inexact-bignum (param $i (ref extern)) (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (call $bignum->f64 (local.get $i))))
     (func $inexact-fraction (param $num (ref eq)) (param $denom (ref eq))
           (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (call $fraction->f64
                             (local.get $num) (local.get $denom))))
     (func $inexact (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-unary-numeric-operation
              '$inexact '$x
              #:subr "inexact"
              #:unpack-flonum '()
              #:unpack-complex '()
              #:dispatch (match-lambda
                           ((or 'flonum 'complex) '((return)))
                           (tp
                            `((return_call ,(symbol-append '$inexact- tp)))))))

     (func $abs-fixnum (param $x i32) (result (ref eq))
           (if (i32.lt_s (local.get $x) (i32.const 0))
               (then (return_call $negate-fixnum (local.get $x))))
           (return_call $i32->fixnum (local.get $x)))
     (func $abs-bignum (param $x (ref extern)) (result (ref eq))
           (if (call $lt-big-fix (local.get $x) (i32.const 0))
               (then (return_call $negate-bignum (local.get $x))))
           (struct.new $bignum (i32.const 0) (local.get $x)))
     (func $abs-flonum (param $x f64) (result (ref eq))
           (struct.new $flonum (i32.const 0)
                       (f64.abs (local.get $x))))
     (func $abs-integer (param $x (ref eq)) (result (ref eq))
           (block $i31 (ref i31)
                  (br_on_cast $i31 (ref eq) (ref i31) (local.get $x))
                  (return_call $abs-bignum
                               (struct.get $bignum $val
                                           (ref.cast $bignum (local.get $x)))))
           (return_call $abs-fixnum (i32.shr_s (i31.get_s) (i32.const 1))))
     (func $abs-fraction (param $x-num (ref eq)) (param $x-denom (ref eq))
           (result (ref eq))
           (struct.new $fraction (i32.const 0)
                       (call $abs-integer (local.get $x-num))
                       (local.get $x-denom)))
     (func $abs (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-unary-numeric-operation '$abs '$x
                                               #:subr "abs" #:real? #t))

     (func $floor-flonum (param $x f64) (result (ref eq))
           (struct.new $flonum (i32.const 0) (f64.floor (local.get $x))))
     ;; floor of $M/$N, with $M, $N in Z and $N > 0 and both integers
     ;; normalized: (m - m mod n)/n, where m mod n = (% (+ (% m n) n) n)
     (func $floor-fraction
           (param $m (ref eq)) (param $n (ref eq)) (result (ref eq))
           (call $divide-integers
                 (call $sub-integers
                       (local.get $m)
                       (call $rem-integers
                             (call $add-integers
                                   (call $rem-integers
                                         (local.get $m) (local.get $n))
                                   (local.get $n))
                             (local.get $n)))
                 (local.get $n)))
     (func $floor (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,@(dispatch-unary-numeric-operation
              '$floor '$x
              #:real? #t
              #:subr "floor"
              #:unpack-fixnum `((if (i32.and (i31.get_s) (i32.const 1))
                                    (then (br $bad-operand (local.get $x))))
                                (local.get $x))
              #:unpack-bignum '()
              #:dispatch (match-lambda
                           ((or 'fixnum 'bignum) '((return)))
                           (tp
                            `((return_call ,(symbol-append '$floor- tp)))))))

     (func $ceiling-flonum (param $x f64) (result (ref eq))
           (struct.new $flonum (i32.const 0) (f64.ceil (local.get $x))))
     (func $ceiling-fraction
           (param $m (ref eq)) (param $n (ref eq)) (result (ref eq))
           (return_call $add-integers
                        (call $floor-fraction (local.get $m) (local.get $n))
                        ,(fixnum-immediate 1)))
     (func $ceiling (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           (local $tmp-fraction (ref $fraction))
           ,@(dispatch-unary-numeric-operation
              '$ceiling '$x
              #:real? #t
              #:subr "ceiling"
              #:unpack-fixnum `((if (i32.and (i31.get_s) (i32.const 1))
                                    (then (br $bad-operand (local.get $x))))
                                (local.get $x))
              #:unpack-bignum '()
              #:dispatch (match-lambda
                           ((or 'fixnum 'bignum) '((return)))
                           (tp
                            `((return_call ,(symbol-append '$ceiling- tp)))))))

     (func $sqrt (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref $flonum))
           ,(call-fmath '$fsqrt '(local.get $x)))

     (func $sin (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$fsin '(local.get $x)))
     (func $cos (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$fcos '(local.get $x)))
     (func $tan (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$ftan '(local.get $x)))
     (func $asin (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$fasin '(local.get $x)))
     (func $acos (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$facos '(local.get $x)))
     (func $atan (param $x (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$fatan '(local.get $x)))
     (func $atan2 (param $x (ref eq)) (param $y (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result (ref eq))
           ,(call-fmath '$fatan2 '(local.get $x) '(local.get $y)))

     (func $log (param $x f64) (result (ref eq))
           (struct.new $flonum (i32.const 0) (call $flog (local.get $x))))
     (func $exp (param $x f64) (result (ref eq))
           (struct.new $flonum (i32.const 0) (call $fexp (local.get $x))))

     (func $u64->bignum (param $i64 i64) (result (ref eq))
           (struct.new $bignum
                       (i32.const 0)
                       (call $bignum-from-u64 (local.get $i64))))
     (func $s64->bignum (param $i64 i64) (result (ref eq))
           (struct.new $bignum
                       (i32.const 0)
                       (call $bignum-from-i64 (local.get $i64))))

     (func $scm->s64 (param $a (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result i64)
           ,@(dispatch-unary-numeric-operation
              '$scm->s64 '$a
              #:s64? #t
              #:subr "scm->s64"
              #:dispatch (match-lambda
                           ('fixnum '((return (i64.extend_i32_s))))
                           ('bignum '((return_call $bignum-get-i64))))))
     (func $scm->u64 (param $a (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result i64)
           (local $tmp-i32 i32)
           (local $tmp-bignum (ref extern))
           ,@(dispatch-unary-numeric-operation
              '$scm->u64 '$a
              #:u64? #t
              #:subr "scm->u64"
              #:dispatch (match-lambda
                           ('fixnum '((return (i64.extend_i32_s))))
                           ('bignum '((return_call $bignum-get-i64))))))
     (func $scm->u64/truncate (param $a (ref eq))
           (param $file (ref null string)) (param $line i32) (param $col i32)
           (result i64)
           (local $tmp-i32 i32)
           ,@(dispatch-unary-numeric-operation
              '$scm->u64 '$a
              #:exact-integer? #t
              #:subr "scm->u64/truncate"
              #:dispatch (match-lambda
                           ('fixnum '((return (i64.extend_i32_s))))
                           ('bignum '((return_call $bignum-get-i64))))))
     (func $s64->scm (param $a i64) (result (ref eq))
           (if (result (ref eq))
               (i32.and (i64.ge_s (local.get $a) (i64.const ,(ash -1 29)))
                        (i64.lt_s (local.get $a) (i64.const ,(ash 1 29))))
               (then (ref.i31
                      (i32.shl (i32.wrap_i64 (local.get $a))
                               (i32.const 1))))
               (else (return_call $s64->bignum (local.get $a)))))
     (func $s32->scm (param $a i32) (result (ref eq))
           (if (ref eq)
               (i32.and (i32.ge_s (local.get $a) (i32.const ,(ash -1 29)))
                        (i32.lt_s (local.get $a) (i32.const ,(ash 1 29))))
               (then (call $i32->fixnum (local.get $a)))
               (else (return_call $s64->bignum (i64.extend_i32_s (local.get $a))))))

     (func $string->wtf8
           (param $str (ref string)) (result (ref $raw-bytevector))
           (local $vu0 (ref $raw-bytevector))
           (local.set $vu0
                      (array.new_default
                       $raw-bytevector
                       (string.measure_wtf8 (local.get $str))))
           (string.encode_wtf8_array (local.get $str)
                                     (local.get $vu0)
                                     (i32.const 0))
           (local.get $vu0))

     (func $wtf8->string
           (param $bv (ref $raw-bytevector)) (result (ref string))
           (string.new_lossy_utf8_array (local.get $bv)
                                        (i32.const 0)
                                        (array.len (local.get $bv))))

     (func $set-fluid-and-return-prev (param $nargs i32)
           (param $arg0 (ref eq)) (param $arg1 (ref eq))
           (param $arg2 (ref eq))
           (local $fluid (ref $fluid))
           (local $prev (ref eq))
           (if (i32.eqz (local.get $nargs))
               (then
                (return_call $raise-arity-error
                             (string.const "[parameter conversion result]")
                             (ref.i31 (i32.const 1))
                             ,@no-source)))
           (global.set $scm-sp (i32.sub (global.get $scm-sp) (i32.const 1)))
           (local.set $fluid
                      (ref.cast $fluid
                                (table.get $scm-stack (global.get $scm-sp))))
           (local.set $prev (call $fluid-ref (local.get $fluid)))
           (call $fluid-set! (local.get $fluid) (local.get $arg0))
           (global.set $ret-sp (i32.sub (global.get $ret-sp) (i32.const 1)))
           (return_call_ref $kvarargs
                            (i32.const 1)
                            (local.get $prev)
                            (ref.i31 (i32.const 1))
                            (ref.i31 (i32.const 1))
                            (table.get $ret-stack (global.get $ret-sp))))
     (func $parameter (param $nargs i32) (param $arg0 (ref eq))
           (param $arg1 (ref eq)) (param $arg2 (ref eq))
           (local $parameter (ref $parameter))
           (local.set $parameter (ref.cast $parameter (local.get $arg0)))
           (if (i32.eq (local.get $nargs) (i32.const 1))
               (then
                (global.set $ret-sp
                            (i32.sub (global.get $ret-sp) (i32.const 1)))
                (return_call_ref $kvarargs
                                 (i32.const 1)
                                 (call $fluid-ref
                                       (struct.get $parameter $fluid
                                                   (local.get $parameter)))
                                 (ref.i31 (i32.const 1))
                                 (ref.i31 (i32.const 1))
                                 (table.get $ret-stack (global.get $ret-sp)))))
           (if (i32.ne (local.get $nargs) (i32.const 2))
               (then
                (return_call $raise-arity-error
                             (string.const "[parameter]")
                             (local.get $arg0)
                             ,@no-source)))
           (global.set $scm-sp (i32.add (global.get $scm-sp) (i32.const 1)))
           (call $maybe-grow-scm-stack)
           (global.set $ret-sp (i32.add (global.get $ret-sp) (i32.const 1)))
           (call $maybe-grow-ret-stack)
           (table.set $scm-stack (i32.sub (global.get $scm-sp) (i32.const 1))
                      (struct.get $parameter $fluid (local.get $parameter)))
           (table.set $ret-stack (i32.sub (global.get $ret-sp) (i32.const 1))
                      (ref.func $set-fluid-and-return-prev))
           (return_call_ref $kvarargs
                            (i32.const 2)
                            (struct.get $parameter $convert
                                        (local.get $parameter))
                            (local.get $arg1)
                            (ref.i31 (i32.const 1))
                            (struct.get $proc $func
                                        (struct.get $parameter $convert
                                                    (local.get $parameter)))))

     (table ,@(maybe-import '$argv) 0 (ref null eq))
     (table ,@(maybe-import '$scm-stack) 0 (ref null eq))
     (table ,@(maybe-import '$ret-stack) 0 (ref null $kvarargs))
     (table ,@(maybe-import '$dyn-stack) 0 (ref null $dyn))

     (memory ,@(maybe-import '$raw-stack) 0)

     (tag ,@(maybe-import '$trampoline-tag)
          (param $nargs i32)
          (param $arg0 (ref eq))
          (param $arg1 (ref eq))
          (param $arg2 (ref eq))
          (param $func (ref $kvarargs))
          (param $nreturns i32))

     (global ,@(maybe-import '$arg3) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg4) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg5) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg6) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$arg7) (mut (ref eq)) ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$ret-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$scm-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$raw-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$dyn-sp) (mut i32) ,@maybe-init-i32-zero)
     (global ,@(maybe-import '$current-fluids) (mut (ref $hash-table))
             ,@maybe-init-hash-table)
     (global ,@(maybe-import '$raise-exception) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$with-exception-handler) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$current-input-port) (mut (ref eq))
             ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$current-output-port) (mut (ref eq))
             ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$current-error-port) (mut (ref eq))
             ,@maybe-init-i31-zero)
     (global ,@(maybe-import '$default-prompt-tag) (mut (ref eq))
             ,@maybe-init-i31-zero)

     (global ,@(maybe-import '$make-size-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-index-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-range-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-start-offset-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-end-offset-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-type-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-unimplemented-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-assertion-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-not-seekable-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-runtime-error-with-message) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-runtime-error-with-message+irritants) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-match-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-arity-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-invalid-keyword-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-unrecognized-keyword-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-missing-keyword-argument-error) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$make-syntax-violation) (mut (ref $proc))
             ,@maybe-init-proc)
     (global ,@(maybe-import '$annotate-with-source) (mut (ref $proc))
             ,@maybe-init-proc))))

(define (memoize f)
  (define cache (make-hash-table))
  (lambda args
    (match (hash-ref cache args)
      (#f (call-with-values (lambda () (apply f args))
            (lambda res
              (hash-set! cache args res)
              (apply values res))))
      (res (apply values res)))))

(define compute-stdlib/memoized (memoize compute-stdlib))
