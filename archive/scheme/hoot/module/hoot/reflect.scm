;;; WebAssembly reflection
;;; Copyright (C) 2023, 2024 David Thompson <dave@spritely.institute>
;;; Copyright (C) 2023 Igalia, S.L.
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
;;; Reflection for Hoot-compiled WASM modules.
;;;
;;; Code:

(define-module (hoot reflect)
  #:use-module (hoot compile)
  #:use-module (hoot config)
  #:use-module (hoot finalization)
  #:use-module (hoot promises)
  #:use-module (hoot scheduler)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 weak-vector)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module ((srfi srfi-19) #:hide (current-time))
  #:use-module (wasm canonical-types)
  #:use-module (wasm parse)
  #:use-module (wasm types)
  #:use-module (wasm vm)
  #:export (hoot-object?
            hoot-complex?
            hoot-complex-real
            hoot-complex-imag
            hoot-fraction?
            hoot-fraction-num
            hoot-fraction-denom
            hoot-pair?
            mutable-hoot-pair?
            hoot-pair-car
            hoot-pair-cdr
            hoot-vector?
            mutable-hoot-vector?
            hoot-vector-length
            hoot-vector-ref
            hoot-bytevector?
            mutable-hoot-bytevector?
            hoot-bytevector-length
            hoot-bytevector-ref
            hoot-bitvector?
            mutable-hoot-bitvector?
            hoot-bitvector-length
            hoot-bitvector-ref
            hoot-symbol?
            hoot-symbol-name
            hoot-keyword?
            hoot-keyword-name
            mutable-hoot-string?
            mutable-hoot-string->string
            hoot-procedure?
            hoot-variable?
            hoot-atomic-box?
            hoot-hash-table?
            hoot-weak-table?
            hoot-fluid?
            hoot-dynamic-state?
            hoot-syntax?
            hoot-syntax-transformer?
            hoot-port?
            hoot-struct?
            hoot-print

            hoot-module?
            hoot-module-reflector
            hoot-module-instance

            reflector?
            reflector-instance
            reflector-abi

            call-with-fake-clock

            hoot-instantiate
            hoot-apply
            hoot-apply-async
            hoot-load
            compile-call
            compile-value))

(define (s64? x)
  (and (exact-integer? x) (< (- (ash -1 63) 1) x (ash 1 63))))

(define (u64? x)
  (and (exact-integer? x) (< -1 x (- (ash 1 64) 1))))

(define-record-type <reflector>
  (make-reflector instance abi)
  reflector?
  (instance reflector-instance)
  (abi reflector-abi))

(set-record-type-printer! <reflector>
                          (lambda (r port)
                            (format port "#<reflector instance: ~a>"
                                    (reflector-instance r))))

(define-record-type <hoot-module>
  (make-hoot-module reflector instance)
  hoot-module?
  (reflector hoot-module-reflector)
  (instance hoot-module-instance))

(define-record-type <hoot-complex>
  (make-hoot-complex reflector obj real imag)
  hoot-complex?
  (reflector hoot-complex-reflector)
  (obj hoot-complex-obj)
  (real hoot-complex-real)
  (imag hoot-complex-imag))

(define-record-type <hoot-fraction>
  (make-hoot-fraction reflector obj num denom)
  hoot-fraction?
  (reflector hoot-fraction-reflector)
  (obj hoot-fraction-obj)
  (num hoot-fraction-num)
  (denom hoot-fraction-denom))

(define-record-type <hoot-pair>
  (make-hoot-pair reflector obj)
  hoot-pair?
  (reflector hoot-pair-reflector)
  (obj hoot-pair-obj))

(define-record-type <mutable-hoot-pair>
  (make-mutable-hoot-pair reflector obj)
  mutable-hoot-pair?
  (reflector mutable-hoot-pair-reflector)
  (obj mutable-hoot-pair-obj))

(define-record-type <hoot-vector>
  (make-hoot-vector reflector obj)
  hoot-vector?
  (reflector hoot-vector-reflector)
  (obj hoot-vector-obj))

(define-record-type <mutable-hoot-vector>
  (make-mutable-hoot-vector reflector obj)
  mutable-hoot-vector?
  (reflector mutable-hoot-vector-reflector)
  (obj mutable-hoot-vector-obj))

(define-record-type <hoot-bytevector>
  (make-hoot-bytevector reflector obj)
  hoot-bytevector?
  (reflector hoot-bytevector-reflector)
  (obj hoot-bytevector-obj))

(define-record-type <mutable-hoot-bytevector>
  (make-mutable-hoot-bytevector reflector obj)
  mutable-hoot-bytevector?
  (reflector mutable-hoot-bytevector-reflector)
  (obj mutable-hoot-bytevector-obj))

(define-record-type <hoot-bitvector>
  (make-hoot-bitvector reflector obj)
  hoot-bitvector?
  (reflector hoot-bitvector-reflector)
  (obj hoot-bitvector-obj))

(define-record-type <mutable-hoot-bitvector>
  (make-mutable-hoot-bitvector reflector obj)
  mutable-hoot-bitvector?
  (reflector mutable-hoot-bitvector-reflector)
  (obj mutable-hoot-bitvector-obj))

(define-record-type <mutable-hoot-string>
  (make-mutable-hoot-string reflector obj)
  mutable-hoot-string?
  (reflector mutable-hoot-string-reflector)
  (obj mutable-hoot-string-obj))

(define-record-type <hoot-symbol>
  (make-hoot-symbol reflector obj)
  hoot-symbol?
  (reflector hoot-symbol-reflector)
  (obj hoot-symbol-obj))

(define-record-type <hoot-keyword>
  (make-hoot-keyword reflector obj)
  hoot-keyword?
  (reflector hoot-keyword-reflector)
  (obj hoot-keyword-obj))

(define-record-type <hoot-variable>
  (make-hoot-variable reflector obj)
  hoot-variable?
  (reflector hoot-variable-reflector)
  (obj hoot-variable-obj))

(define-record-type <hoot-atomic-box>
  (make-hoot-atomic-box reflector obj)
  hoot-atomic-box?
  (reflector hoot-atomic-box-reflector)
  (obj hoot-atomic-box-obj))

(define-record-type <hoot-hash-table>
  (make-hoot-hash-table reflector obj)
  hoot-hash-table?
  (reflector hoot-hash-table-reflector)
  (obj hoot-hash-table-obj))

(define-record-type <hoot-weak-table>
  (make-hoot-weak-table reflector obj)
  hoot-weak-table?
  (reflector hoot-weak-table-reflector)
  (obj hoot-weak-table-obj))

(define-record-type <hoot-fluid>
  (make-hoot-fluid reflector obj)
  hoot-fluid?
  (reflector hoot-fluid-reflector)
  (obj hoot-fluid-obj))

(define-record-type <hoot-dynamic-state>
  (make-hoot-dynamic-state reflector obj)
  hoot-dynamic-state?
  (reflector hoot-dynamic-state-reflector)
  (obj hoot-dynamic-state-obj))

(define-record-type <hoot-syntax>
  (make-hoot-syntax reflector obj)
  hoot-syntax?
  (reflector hoot-syntax-reflector)
  (obj hoot-syntax-obj))

(define-record-type <hoot-syntax-transformer>
  (make-hoot-syntax-transformer reflector obj)
  hoot-syntax-transformer?
  (reflector hoot-syntax-transformer-reflector)
  (obj hoot-syntax-transformer-obj))

(define-record-type <hoot-port>
  (make-hoot-port reflector obj)
  hoot-port?
  (reflector hoot-port-reflector)
  (obj hoot-port-obj))

(define-record-type <hoot-struct>
  (make-hoot-struct reflector obj)
  hoot-struct?
  (reflector hoot-struct-reflector)
  (obj hoot-struct-obj))

;; The Hoot procedure type is defined using Guile's low-level struct
;; API so that we can use applicable structs, allowing Hoot procedures
;; to be called as if they were native ones.
(define <hoot-procedure>
  (make-struct/no-tail <applicable-struct-vtable> 'pwpwpw))

(define (hoot-procedure? obj)
  (and (struct? obj) (eq? (struct-vtable obj) <hoot-procedure>)))

(define (make-hoot-procedure reflector obj)
  (define (hoot-apply . args)
    (hoot-call reflector obj args))
  (make-struct/no-tail <hoot-procedure> hoot-apply reflector obj))

(define (hoot-object? obj)
  (or (hoot-complex? obj)
      (hoot-fraction? obj)
      (hoot-pair? obj)
      (mutable-hoot-pair? obj)
      (hoot-vector? obj)
      (mutable-hoot-vector? obj)
      (hoot-bytevector? obj)
      (mutable-hoot-bytevector? obj)
      (hoot-bitvector? obj)
      (mutable-hoot-bitvector? obj)
      (mutable-hoot-string? obj)
      (hoot-procedure? obj)
      (hoot-symbol? obj)
      (hoot-keyword? obj)
      (hoot-variable? obj)
      (hoot-atomic-box? obj)
      (hoot-hash-table? obj)
      (hoot-weak-table? obj)
      (hoot-fluid? obj)
      (hoot-dynamic-state? obj)
      (hoot-syntax? obj)
      (hoot-syntax-transformer? obj)
      (hoot-port? obj)
      (hoot-struct? obj)))

(define-syntax-rule (~ reflector name args ...)
  ((wasm-instance-export-ref (reflector-instance reflector) name) args ...))

(define (hoot-pair-car pair)
  (match pair
    ((or ($ <hoot-pair> reflector obj)
         ($ <mutable-hoot-pair> reflector obj))
     (wasm->guile reflector (~ reflector "car" obj)))))

(define (hoot-pair-cdr pair)
  (match pair
    ((or ($ <hoot-pair> reflector obj)
         ($ <mutable-hoot-pair> reflector obj))
     (wasm->guile reflector (~ reflector "cdr" obj)))))

(define (hoot-vector-length vec)
  (match vec
    ((or ($ <hoot-vector> reflector obj)
         ($ <mutable-hoot-vector> reflector obj))
     (~ reflector "vector_length" obj))))

(define (hoot-vector-ref vec idx)
  (match vec
    ((or ($ <hoot-vector> reflector obj)
         ($ <mutable-hoot-vector> reflector obj))
     (wasm->guile reflector (~ reflector "vector_ref" obj idx)))))

(define (hoot-bytevector-length bv)
  (match bv
    ((or ($ <hoot-bytevector> reflector obj)
         ($ <mutable-hoot-bytevector> reflector obj))
     (~ reflector "bytevector_length" obj))))

(define (hoot-bytevector-ref bv idx)
  (match bv
    ((or ($ <hoot-bytevector> reflector obj)
         ($ <mutable-hoot-bytevector> reflector obj))
     (~ reflector "bytevector_ref" obj idx))))

(define (hoot-bitvector-length bv)
  (match bv
    ((or ($ <hoot-bitvector> reflector obj)
         ($ <mutable-hoot-bitvector> reflector obj))
     (~ reflector "bitvector_length" obj))))

(define (hoot-bitvector-ref bv idx)
  (match bv
    ((or ($ <hoot-bitvector> reflector obj)
         ($ <mutable-hoot-bitvector> reflector obj))
     (~ reflector "bitvector_ref" obj idx))))

(define (hoot-symbol-name sym)
  (match sym
    (($ <hoot-symbol> reflector obj)
     (~ reflector "symbol_name" obj))))

(define (hoot-keyword-name kw)
  (match kw
    (($ <hoot-keyword> reflector obj)
     (~ reflector "keyword_name" obj))))

(define (mutable-hoot-string->string str)
  (match str
    (($ <mutable-hoot-string> reflector obj)
     (~ reflector "string_value" obj))))

;; UH OH: This doesn't detect cycles!
(define (hoot-print obj port)
  (match obj
    ((or #t #f () #nil (? number?) (? eof-object?)
         (? unspecified?) (? char?) (? string?))
     (write obj port))
    ((? hoot-complex?)
     (let ((real (hoot-complex-real obj))
           (imag (hoot-complex-imag obj)))
       (hoot-print real port)
       (when (and (>= imag 0.0) (not (nan? imag)) (not (inf? imag)))
         (display "+" port))
       (hoot-print imag port)
       (display "i" port)))
    ((? hoot-fraction?)
     (hoot-print (hoot-fraction-num obj) port)
     (display "/" port)
     (hoot-print (hoot-fraction-denom obj) port))
    ((or (? hoot-pair?) (? mutable-hoot-pair?))
     (display "(" port)
     (hoot-print (hoot-pair-car obj) port)
     (let loop ((cdr (hoot-pair-cdr obj)))
       (match cdr
         (() #t)
         ((or (? hoot-pair?) (? mutable-hoot-pair?))
          (display " " port)
          (hoot-print (hoot-pair-car cdr) port)
          (loop (hoot-pair-cdr cdr)))
         (obj
          (display " . " port)
          (hoot-print obj port))))
     (display ")" port))
    ((or (? hoot-vector?) (? mutable-hoot-vector?))
     (let ((k (hoot-vector-length obj)))
       (display "#(" port)
       (unless (= k 0)
         (do ((i 0 (+ i 1)))
             ((= i (- k 1)))
           (hoot-print (hoot-vector-ref obj i) port)
           (display " " port))
         (hoot-print (hoot-vector-ref obj (- k 1)) port))
       (display ")" port)))
    ((or (? hoot-bytevector?) (? mutable-hoot-bytevector?))
     (let ((k (hoot-bytevector-length obj)))
       (display "#vu8(" port)
       (unless (= k 0)
         (do ((i 0 (+ i 1)))
             ((= i (- k 1)))
           (display (hoot-bytevector-ref obj i) port)
           (display " " port))
         (display (hoot-bytevector-ref obj (- k 1)) port))
       (display ")" port)))
    ((or (? hoot-bitvector?)
         (? mutable-hoot-bitvector?))
     (let ((k (hoot-bitvector-length obj)))
       (display "#*" port)
       (do ((i 0 (+ i 1)))
           ((= i k))
         (display (hoot-bitvector-ref obj i) port))))
    ((? mutable-hoot-string?)
     (write (mutable-hoot-string->string obj) port))
    ((? hoot-symbol?)
     (display (hoot-symbol-name obj) port))
    ((? hoot-keyword?)
     (format port "#:~a" (hoot-keyword-name obj)))
    ((? hoot-procedure?) (display "#<procedure>" port))
    ((? hoot-variable?) (display "#<variable>" port))
    ((? hoot-atomic-box?) (display "#<atomic-box>" port))
    ((? hoot-hash-table?) (display "#<hash-table>" port))
    ((? hoot-weak-table?) (display "#<weak-table>" port))
    ((? hoot-fluid?) (display "#<fluid>" port))
    ((? hoot-dynamic-state?) (display "#<dynamic-state>" port))
    ((? hoot-syntax?) (display "#<syntax>" port))
    ((? hoot-syntax-transformer?) (display "#<syntax-transformer>" port))
    ((? hoot-port?) (display "#<port>" port))
    ((? hoot-struct?) (display "#<struct>" port))))

(define (hoot-print-record obj port)
  (display "#<hoot " port)
  (hoot-print obj port)
  (display ">" port))

(for-each (lambda (rtd) (set-record-type-printer! rtd hoot-print-record))
          (list <hoot-complex>
                <hoot-fraction>
                <hoot-pair>
                <mutable-hoot-pair>
                <hoot-vector>
                <mutable-hoot-vector>
                <hoot-bytevector>
                <mutable-hoot-bytevector>
                <hoot-bitvector>
                <mutable-hoot-bitvector>
                <mutable-hoot-string>
                <hoot-procedure>
                <hoot-symbol>
                <hoot-keyword>
                <hoot-variable>
                <hoot-atomic-box>
                <hoot-hash-table>
                <hoot-weak-table>
                <hoot-fluid>
                <hoot-dynamic-state>
                <hoot-syntax>
                <hoot-syntax-transformer>
                <hoot-port>
                <hoot-struct>))

(define (wasm->guile reflector x)
  (match (~ reflector "describe" x)
    ("fixnum" (~ reflector "fixnum_value" x))
    ("char" (integer->char (~ reflector "char_value" x)))
    ("string" (~ reflector "string_value" x))
    ("mutable-string" (make-mutable-hoot-string reflector x))
    ("true" #t)
    ("false" #f)
    ("eof" (eof-object))
    ("nil" #nil)
    ("null" '())
    ("unspecified" *unspecified*)
    ("flonum" (~ reflector "flonum_value" x))
    ("bignum" (~ reflector "bignum_value" x))
    ("complex"
     (make-hoot-complex reflector x
                        (~ reflector "complex_real" x)
                        (~ reflector "complex_imag" x)))
    ("fraction"
     (make-hoot-fraction reflector x
                         (wasm->guile reflector (~ reflector "fraction_num" x))
                         (wasm->guile reflector (~ reflector "fraction_denom" x))))
    ("symbol" (make-hoot-symbol reflector x))
    ("keyword" (make-hoot-keyword reflector x))
    ("pair" (make-hoot-pair reflector x))
    ("mutable-pair" (make-mutable-hoot-pair reflector x))
    ("vector" (make-hoot-vector reflector x))
    ("mutable-vector" (make-mutable-hoot-vector reflector x))
    ("bytevector" (make-hoot-bytevector reflector x))
    ("mutable-bytevector" (make-mutable-hoot-bytevector reflector x))
    ("bitvector" (make-hoot-bitvector reflector x))
    ("mutable-bitvector" (make-mutable-hoot-bitvector reflector x))
    ("procedure" (make-hoot-procedure reflector x))
    ("variable" (make-hoot-variable reflector x))
    ("atomic-box" (make-hoot-atomic-box reflector x))
    ("hash-table" (make-hoot-hash-table reflector x))
    ("weak-table" (make-hoot-weak-table reflector x))
    ("fluid" (make-hoot-fluid reflector x))
    ("dynamic-state" (make-hoot-dynamic-state reflector x))
    ("syntax" (make-hoot-syntax reflector x))
    ("syntax-transformer" (make-hoot-syntax-transformer reflector x))
    ("port" (make-hoot-port reflector x))
    ("struct" (make-hoot-struct reflector x))
    ("extern-ref" (~ reflector "extern_value" x))
    ("func-ref" (~ reflector "func_value" x))))

(define (guile->wasm reflector x)
  (match x
    ((and (? number?) (? inexact?)) (~ reflector "scm_from_f64" x))
    ((? exact-integer?)
     (if (<= (~ reflector "scm_most_negative_fixnum")
             x
             (~ reflector "scm_most_positive_fixnum"))
         (~ reflector "scm_from_fixnum" x)
         (~ reflector "scm_from_bignum" x)))
    ((and (? number?) (? exact?)) (~ reflector "scm_from_fraction" (numerator x) (denominator x)))
    ((? complex?) (~ reflector "scm_from_complex" (real-part x) (imag-part x)))
    (#t (~ reflector "scm_true"))
    (#f (~ reflector "scm_false"))
    (() (if (eq? x #nil)
            (~ reflector "scm_nil")
            (~ reflector "scm_null")))
    ((? unspecified?) (~ reflector "scm_unspecified"))
    ((? eof-object?) (~ reflector "scm_eof"))
    ((? char?) (~ reflector "scm_from_char" (char->integer x)))
    ((? string?) (~ reflector "scm_from_string" x))
    ((or ($ <hoot-complex> _ obj)
         ($ <hoot-fraction> _ obj)
         ($ <hoot-pair> _ obj)
         ($ <mutable-hoot-pair> _ obj)
         ($ <hoot-vector> _ obj)
         ($ <mutable-hoot-vector> _ obj)
         ($ <hoot-bytevector> _ obj)
         ($ <mutable-hoot-bytevector> _ obj)
         ($ <hoot-bitvector> _ obj)
         ($ <mutable-hoot-bitvector> _ obj)
         ($ <mutable-hoot-string> _ obj)
         ($ <hoot-procedure> _ _ obj)
         ($ <hoot-symbol> _ obj)
         ($ <hoot-keyword> _ obj)
         ($ <hoot-variable> _ obj)
         ($ <hoot-atomic-box> _ obj)
         ($ <hoot-hash-table> _ obj)
         ($ <hoot-weak-table> _ obj)
         ($ <hoot-fluid> _ obj)
         ($ <hoot-dynamic-state> _ obj)
         ($ <hoot-syntax> _ obj)
         ($ <hoot-syntax-transformer> _ obj)
         ($ <hoot-port> _ obj)
         ($ <hoot-struct> _ obj))
     obj)
    (_ (~ reflector "scm_from_extern" x))))

(define $wtf8 (canonicalize-type! (make-array-type #t 'i8)))

(define (wtf8->string wtf8)
  (let* ((k (wasm-array-length wtf8))
         (bv (make-bytevector k)))
    (do ((i 0 (+ i 1)))
        ((= i k))
      (bytevector-u8-set! bv i (wasm-array-ref-unsigned wtf8 i)))
    (utf8->string bv)))

(define (string->wtf8 str)
  (let* ((bv (string->utf8 str))
         (k (bytevector-length bv))
         (array (make-wasm-array $wtf8 k 0)))
    (do ((i 0 (+ i 1)))
        ((= i k))
      (wasm-array-set! array i (bytevector-u8-ref bv i)))
    array))

(define (rsh a b)
  (ash a (- b)))

(define (bignum->i64 x)
  (max (min x (1- (ash 1 63))) (ash -1 63)))

(define (u64->bignum x)
  (logand x #xffffFFFFffffFFFF))

(define-record-type <clock>
  (make-clock jiffies-per-second current-jiffy current-second)
  clock?
  (jiffies-per-second clock-jiffies-per-second)
  (current-jiffy %clock-current-jiffy)
  (current-second %clock-current-second))
(define (clock-current-jiffy clock)
  ((%clock-current-jiffy clock)))
(define (clock-current-second clock)
  ((%clock-current-second clock)))
(define real-clock
  (make-clock internal-time-units-per-second
              get-internal-real-time
              current-time))
(define current-clock (make-parameter real-clock))
(define (call-with-fake-clock jiffies-per-second current-jiffy current-second thunk)
  (let ((fake (make-clock jiffies-per-second current-jiffy current-second)))
    (parameterize ((current-clock fake))
      (thunk))))

(define current-scheduler (make-parameter #f))
(define-syntax-rule (assert-scheduler x)
  (if (scheduler? x)
      x
      (raise-exception
       (make-exception-with-message "not in async context"))))
(define (async-invoke thunk)
  (scheduler-run! (assert-scheduler (current-scheduler)) thunk))
(define (async-invoke-later thunk delay)
  (scheduler-delay! (assert-scheduler (current-scheduler)) thunk delay))

(define (make-regexp* pattern flags)
  (let ((flags (map (match-lambda
                      (#\i regexp/icase)
                      (#\m regexp/newline))
                    (string->list flags))))
    (apply make-regexp pattern flags)))

(define (async-read port)
  (make-promise
   (lambda (resolve reject)
     (scheduler-run! (assert-scheduler (current-scheduler))
                     (lambda ()
                       (match (get-bytevector-some port)
                         ((? eof-object?)
                          (resolve #vu8()))
                         (bv (resolve bv))))))))

(define (async-write port bv)
  (make-promise
   (lambda (resolve reject)
     (scheduler-run! (assert-scheduler (current-scheduler))
                     (lambda ()
                       (put-bytevector port bv)
                       (force-output port)
                       (resolve #t))))))

(define (async-close port)
  (make-promise
   (lambda (resolve reject)
     (scheduler-run! (assert-scheduler (current-scheduler))
                     (lambda ()
                       (close port)
                       (resolve #t))))))

(define %runtime-imports
  `(("rt" .
     (("bignum_from_string" . ,string->number)
      ("bignum_from_i32" . ,identity)
      ("bignum_from_i64" . ,identity)
      ("bignum_from_u64" . ,u64->bignum)
      ("bignum_to_f64" . ,exact->inexact)
      ("bignum_is_i64" . ,s64?)
      ("bignum_is_u64" . ,u64?)
      ("bignum_get_i64" . ,bignum->i64)
      ("bignum_add" . ,+)
      ("bignum_sub" . ,-)
      ("bignum_mul" . ,*)
      ("bignum_lsh" . ,ash)
      ("bignum_rsh" . ,rsh)
      ("bignum_quo" . ,quotient)
      ("bignum_rem" . ,remainder)
      ("bignum_mod" . ,modulo)
      ("bignum_gcd" . ,gcd)
      ("bignum_logand" . ,logand)
      ("bignum_logior" . ,logior)
      ("bignum_logxor" . ,logxor)
      ("bignum_logcount" . ,logcount)
      ("bignum_lt" . ,<)
      ("bignum_le" . ,<=)
      ("bignum_eq" . ,=)
      ("flonum_to_string" . ,number->string)
      ("string_upcase" . ,string-upcase)
      ("string_downcase" . ,string-downcase)
      ("make_weak_ref" . ,weak-vector)
      ("weak_ref_deref" . ,(lambda (ref fail)
                             (or (weak-vector-ref ref 0) fail)))
      ("make_weak_map" . ,make-weak-key-hash-table)
      ("weak_map_get" . ,hashq-ref)
      ("weak_map_set" . ,hashq-set!)
      ("weak_map_delete" . ,hashq-remove!)
      ("fsqrt" . ,sqrt)
      ("fsin" . ,sin)
      ("fcos" . ,cos)
      ("ftan" . ,tan)
      ("fasin" . ,asin)
      ("facos" . ,acos)
      ("fatan" . ,atan)
      ("fatan2" . ,atan)
      ("flog" . ,log)
      ("fexp" . ,exp)
      ("jiffies_per_second" . ,(lambda ()
                                 (clock-jiffies-per-second (current-clock))))
      ("current_jiffy" . ,(lambda ()
                            (clock-current-jiffy (current-clock))))
      ("current_second" . ,(lambda ()
                             (exact->inexact
                              (clock-current-second (current-clock)))))
      ("make_date" . ,(lambda (year month day hour min sec ms)
                        (make-date (* ms (expt 10 6)) sec min hour day month year 0)))
      ("ms_utc_to_date" . ,(lambda (ms)
                             (call-with-values
                                 (lambda () (truncate/ (inexact->exact ms) 1000))
                               (lambda (sec ms)
                                 (time-utc->date
                                  (make-time time-utc (* ms 1000) sec))))))
      ("date_year" . ,date-year)
      ("date_month" . ,date-month)
      ("date_day" . ,date-day)
      ("date_weekday" . ,date-week-day)
      ("date_hour" . ,date-hour)
      ("date_min" . ,date-minute)
      ("date_sec" . ,date-second)
      ("date_ms" . ,(lambda (date) (* (date-nanosecond date) (expt 10 6))))
      ("date_timezone_offset" . ,date-zone-offset)
      ("date_locale" . ,(lambda (date) (date->string date "~c")))
      ;; FIXME: SRFI-19 doesn't have a way to generate just the locale
      ;; time or date, so we just return ISO-8601 style here.
      ("date_locale_date" . ,(lambda (date) (date->string date "~1")))
      ("date_locale_time" . ,(lambda (date) (date->string date "~3")))
      ("async_invoke" . ,async-invoke)
      ("async_invoke_later" . ,async-invoke-later)
      ("promise_on_completed" . ,on)
      ("promise_complete" . ,(lambda (callback val) (callback val)))
      ("wtf8_to_string" . ,wtf8->string)
      ("string_to_wtf8" . ,string->wtf8)
      ("make_regexp" . ,make-regexp*)
      ("regexp_exec" . ,regexp-exec)
      ("regexp_match_string" . ,match:string)
      ("regexp_match_start" . ,match:start)
      ("regexp_match_end" . ,match:end)
      ("regexp_match_count" . ,match:count)
      ("regexp_match_substring" . ,match:substring)
      ("die" . ,(lambda (key . args)
                  (apply throw (string->symbol key) args)))
      ("quit" . ,(lambda (status) (error "Hoot exited with status" status)))
      ("stream_make_chunk" . ,(lambda (len) (make-bytevector len)))
      ("stream_chunk_length" . ,(lambda (bv) (bytevector-length bv)))
      ("stream_chunk_ref" . ,(lambda (bv idx) (bytevector-u8-ref bv idx)))
      ("stream_chunk_set" . ,(lambda (bv idx val) (bytevector-u8-set! bv idx val)))
      ("stream_get_reader" . ,(lambda (port) port))
      ("stream_read" . ,async-read)
      ("stream_result_chunk" . ,(lambda (ret) ret))
      ("stream_result_done" . ,(lambda (ret) (if (zero? (bytevector-length ret)) 1 0)))
      ("stream_get_writer" . ,(lambda (port) port))
      ("stream_write" . ,async-write)
      ("stream_close_writer" . ,async-close)))
    ("io" .
     (("write_stdout" . ,(lambda (str)
                           (put-string (current-output-port) str)
                           (force-output (current-output-port))))
      ("write_stderr" . ,(lambda (str)
                           (put-string (current-error-port) str)
                           (force-output (current-error-port))))
      ("read_stdin" . ,(lambda ()
                         (match (get-line (current-input-port))
                           ((? eof-object?) "")
                           (line (string-append line "\n")))))
      ("file_exists" . ,file-exists?)
      ("open_input_file" . ,(lambda (filename)
                              (list (open-input-file filename)
                                    (make-bytevector 1024))))
      ("open_output_file" . ,(lambda (filename)
                              (list (open-output-file filename)
                                    (make-bytevector 1024))))
      ("close_file" . ,(match-lambda
                         ((port _) (close-port port))))
      ("read_file" . ,(lambda (handle count)
                        (match handle
                          ((port bv)
                           (match (get-bytevector-n! port bv 0 count)
                             ((? eof-object?) 0)
                             (n n))))))
      ("write_file" . ,(lambda (handle count)
                         (match handle
                           ((port bv)
                            (put-bytevector port bv 0 count)
                            count))))
      ("seek_file" . ,(lambda (handle offset whence)
                        (match handle
                          ((port _)
                           (seek port offset whence)))))
      ("file_random_access" . ,(lambda (handle) #t))
      ("file_buffer_size" . ,(match-lambda
                               ((_ bv) (bytevector-length bv))))
      ("file_buffer_ref" . ,(lambda (handle i)
                              (match handle
                                ((_ bv) (bytevector-u8-ref bv i)))))
      ("file_buffer_set" . ,(lambda (handle i x)
                              (match handle
                                ((_ bv) (bytevector-u8-set! bv i x)))))
      ("delete_file" . ,delete-file)
      ("stream_stdin" . ,(lambda () (current-input-port)))
      ("stream_stdout" . ,(lambda () (current-output-port)))
      ("stream_stderr" . ,(lambda () (current-error-port)))))))

(define (make-abi-imports instance)
  `(("abi" . ,(map (lambda (name)
                     (cons name (wasm-instance-export-ref instance name)))
                   (wasm-instance-export-names instance)))))

(define *all-instances* (make-weak-key-hash-table))
(define (all-instances)
  (hash-map->list (lambda (k v) k) *all-instances*))

(define (code-origin code)
  ;; O(n) and not cached, as there is a cycle between instances and
  ;; their funcs, and Guile doesn't currently have ephemeron tables.
  (let check-instances ((instances (all-instances)))
    (match instances
      (() (values #f #f))
      ((instance . instances)
       (let ((get-code (wasm-instance-export-ref instance "%instance-code")))
         (if get-code
             (let check-funcs ((i 0))
               (match (get-code i)
                 ((? wasm-null?) (check-instances instances))
                 (func (if (equal? func code)
                           (values instance i)
                           (check-funcs (1+ i))))))
             (values #f #f)))))))

(define (code-meta code getter not-found)
  ;; O(n) and not cached, as there is a cycle between instances and
  ;; their funcs, and Guile doesn't currently have ephemeron tables.
  (call-with-values (lambda () (code-origin code))
    (lambda (instance idx)
      (if instance
          (match (wasm-instance-export-ref instance getter)
            (#f (not-found))
            (getter (getter idx)))
          (not-found)))))

(define (code-name code)
  (code-meta code "%instance-code-name" (lambda () #f)))
(define (code-source code)
  (code-meta code "%instance-code-source" (lambda () (values #f 0 0))))

(define *finalization-registries* (make-weak-key-hash-table))
(define (poll-finalization-registries!)
  (hash-for-each (lambda (registry _)
                   (poll-finalization-registry! registry))
                 *finalization-registries*))

(define* (hoot-instantiate scheme-wasm #:optional (imports '())
                           (reflector (force reflect-wasm)))
  (define (debug-str str)
    (format #t "debug: ~a\n" str))
  (define (debug-str-i32 str x)
    (format #t "debug: ~a: ~s\n" str x))
  (define (debug-str-scm str x)
    (format #t "debug: ~a: ~s\n" str (wasm->guile reflector x)))
  (define debug-imports
    `(("debug" .
       (("debug_str" . ,debug-str)
        ("debug_str_i32" . ,debug-str-i32)
        ("debug_str_scm" . ,debug-str-scm)
        ("code_name" . ,code-name)
        ("code_source" . ,code-source)))))
  (define (procedure->extern proc)
    (let ((proc* (wasm->guile reflector proc)))
      (lambda args
        (call-with-values (lambda () (apply proc* args))
          ;; Always return a single value because that's what most
          ;; other host environments, like JavaScript, support.
          (case-lambda
            (() *unspecified*)
            ((val) val)
            ((val . rest) val))))))
  (define (call-external proc args)
    (let* ((args* (let lp ((args (wasm->guile reflector args)))
                    (if (null? args)
                        '()
                        (cons (hoot-pair-car args)
                              (lp (hoot-pair-cdr args))))))
          (result (apply proc args*)))
      (guile->wasm reflector result)))
  (define ffi-imports
    `(("ffi" .
       (("is_extern_func" . ,procedure?)
        ("call_extern" . ,call-external)
        ("procedure_to_extern" . ,procedure->extern)))))
  (define (make-finalization-registry* proc)
    (let ((registry (make-finalization-registry proc)))
      (hashq-set! *finalization-registries* registry #t)
      registry))
  (define* (finalization-registry-register!* registry obj held-value
                                             #:optional unregister-token)
    (finalization-registry-register! registry obj
                                     (wasm->guile reflector held-value)
                                     unregister-token))
  (define finalization-imports
    `(("finalization" .
       (("make_finalization_registry" . ,make-finalization-registry)
        ("finalization_registry_register" . ,finalization-registry-register!*)
        ("finalization_registry_register_with_token" . ,finalization-registry-register!*)
        ("finalization_registry_unregister" . ,finalization-registry-unregister!)))))
  (define (instantiate wasm abi-imports)
    (instantiate-wasm (validate-wasm wasm)
                      #:imports (append imports
                                        abi-imports
                                        debug-imports
                                        ffi-imports
                                        finalization-imports)))
  (define (instantiate-module)
    ;; You can either pass an existing reflector and import its ABI, or
    ;; pass a parsed reflection WASM module and create a new reflector.
    (if (reflector? reflector)
        (let ((imports (append %runtime-imports (reflector-abi reflector))))
          (make-hoot-module reflector (instantiate scheme-wasm imports)))
        (let* ((instance (instantiate scheme-wasm %runtime-imports))
               (abi (make-abi-imports instance))
               (imports (append %runtime-imports abi)))
          (set! reflector (make-reflector (instantiate reflector imports) abi))
          (make-hoot-module reflector instance))))
  (let ((module (instantiate-module)))
    (hashq-set! *all-instances* (hoot-module-instance module) #t)
    module))

(define (hoot-call reflector f args)
  (let ((argv (~ reflector "make_vector"
                           (+ (length args) 1)
                           (~ reflector "scm_false"))))
    (~ reflector "vector_set" argv 0 f)
    (let loop ((args args) (i 1))
      (match args
        (() #t)
        ((arg . rest)
         (~ reflector "vector_set" argv i (guile->wasm reflector arg))
         (loop rest (+ i 1)))))
    (let* ((results (~ reflector "call" f argv))
           (n-results (~ reflector "vector_length" results)))
      (apply values
             (let loop ((i 0))
               (if (= i n-results)
                   '()
                   (let ((result (~ reflector "vector_ref" results i)))
                     (cons (wasm->guile reflector result)
                           (loop (+ i 1))))))))))

(define (hoot-apply proc . args)
  (match proc
    (($ <hoot-procedure> _ reflector obj)
     (hoot-call reflector obj args))))

(define (hoot-call-async reflector f args)
  (let ((scheduler (make-scheduler (%clock-current-jiffy (current-clock)))))
    ;; Simple event loop that ticks the scheduler and polls all of the
    ;; live finalization registries until either the promise is
    ;; resolved/rejected or there are no tasks in the scheduler.
    (define (await promise)
      (define done? #f)
      (define results #f)
      (on promise
          (lambda vals
            (set! done? #t)
            (set! results vals))
          (match-lambda*
            (((? exception? err))
             (raise-exception err))
            (irritants
             (raise-exception
              (make-exception (make-exception-with-message "rejected promise")
                              (make-exception-with-irritants irritants))))))
      (let lp ()
        (cond
         (done?
          (apply values results))
         ((scheduler-empty? scheduler)
          (raise-exception
           (make-exception-with-message "awaited promise unresolved")))
         (else
          (scheduler-tick! scheduler)
          (poll-finalization-registries!)
          (lp)))))
    (parameterize ((current-scheduler scheduler))
      (await
       (make-promise
        (lambda (resolve reject)
          (hoot-call reflector f
                     (cons* (lambda (val)
                              (resolve (wasm->guile reflector val)))
                            (lambda (err)
                              (reject (wasm->guile reflector err)))
                            args))))))))

(define (hoot-apply-async proc . args)
  (match proc
    (($ <hoot-procedure> _ reflector obj)
     (hoot-call-async reflector obj args))))

(define (hoot-load module)
  (match module
    (($ <hoot-module> reflector instance)
     (let* (($load (wasm-instance-export-ref instance "$load")))
       ((wasm->guile reflector (wasm-global-ref $load)))))))

(define reflect-wasm
  (delay
    (call-with-input-file (in-vicinity %reflect-wasm-dir "reflect.wasm")
      parse-wasm)))

(define* (compile-value exp #:key
                        (imports %default-program-imports)
                        (wasm-imports '())
                        (load-path '()))
  (hoot-load
   (hoot-instantiate (compile exp
                              #:imports imports
                              #:extend-load-library
                              (library-load-path-extension load-path))
                     wasm-imports
                     (force reflect-wasm))))

(define* (compile-call proc-exp
                       #:key
                       (imports %default-program-imports)
                       (wasm-imports '())
                       (load-path '())
                       #:rest rest)
  (let* ((extend (library-load-path-extension load-path))
         (proc-module (hoot-instantiate (compile proc-exp
                                                 #:imports imports
                                                 #:extend-load-library extend)
                                        wasm-imports
                                        (force reflect-wasm)))
         (proc (hoot-load proc-module))
         (reflector (hoot-module-reflector proc-module))
         ;; Filter kwargs from argument expressions.
         (arg-exps (let loop ((rest rest))
                     (match rest
                       (() '())
                       (((? keyword?) _ . rest)
                        (loop rest))
                       ((x . rest)
                        (cons x (loop rest))))))
         (args (map (lambda (exp)
                      (hoot-load
                       (hoot-instantiate (compile exp
                                                  #:imports imports
                                                  #:extend-load-library extend
                                                  #:import-abi? #t
                                                  #:export-abi? #f)
                                         wasm-imports
                                         reflector)))
                    arg-exps)))
    (apply proc args)))
