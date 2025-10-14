;;; Hoot foreign function interface
;;; Copyright (C) 2023, 2024 David Thompson <dave@spritely.institute>
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
;;; Foreign function interface for declaring Wasm host imports and
;;; handling (ref extern) values.
;;;
;;; Code:

(library (hoot ffi)
  (export external?
          external-null?
          external-non-null?
          external-function?
          call-external
          procedure->external
          define-external-type
          define-foreign)
  (import (hoot cond-expand)
          (hoot external)
          (hoot inline-wasm)
          (hoot not)
          (hoot procedures)
          (hoot strings)
          (hoot gensym)
          (hoot errors)
          (hoot eq)
          (hoot syntax)
          (hoot pairs)
          (only (hoot lists) map)
          (hoot numbers)
          (hoot records)
          (hoot syntax-objects)
          (hoot write))

  (define (external-non-null? extern)
    (not (external-null? extern)))

  (define (procedure->external proc)
    (check-type proc procedure? 'procedure->external)
    (%inline-wasm
     '(func (param $f (ref $proc)) (result (ref eq))
            (struct.new $extern-ref
                        (i32.const 0)
                        (call $procedure->extern (local.get $f))))
     proc))

  (define (external-function? obj)
    (and (external? obj)
         (external-non-null? obj)
         (%inline-wasm
          '(func (param $obj (ref extern)) (result (ref eq))
                 (if (ref eq)
                     (i32.eqz (call $extern-func? (local.get $obj)))
                     (then (ref.i31 (i32.const 1)))
                     (else (ref.i31 (i32.const 17)))))
          obj)))

  (define (call-external func . args)
    (check-type func external-function? 'call-external)
    (%inline-wasm
     '(func (param $func (ref extern)) (param $args (ref eq)) (result (ref eq))
            (call $call-extern (local.get $func) (local.get $args)))
     func args))

  ;; We are not using (hoot hashtables) here to avoid a dependency
  ;; cycle.
  (define (make-weak-map)
    (%inline-wasm
     '(func (result (ref eq))
            (struct.new $extern-ref
                        (i32.const 0)
                        (call $make-weak-map)))))
  (define (weak-map-ref weak-map key)
    (%inline-wasm
     '(func (param $weak-map (ref $extern-ref))
            (param $key (ref eq))
            (result (ref eq))
            (call $weak-map-get
                  (ref.as_non_null
                   (struct.get $extern-ref $val (local.get $weak-map)))
                  (local.get $key)
                  (ref.i31 (i32.const 1))))
     weak-map key))
  (define (weak-map-set! weak-map key value)
    (%inline-wasm
     '(func (param $weak-map (ref $extern-ref))
            (param $key (ref eq))
            (param $value (ref eq))
            (call $weak-map-set
                  (ref.as_non_null
                   (struct.get $extern-ref $val (local.get $weak-map)))
                  (local.get $key)
                  (local.get $value)))
     weak-map key value))

  ;; Analagous to Guile's define-wrapped-pointer-type.
  (define-syntax define-external-type
    (lambda (exp)
      (syntax-case exp ()
        ((_ name pred wrap unwrap print)
         (with-syntax ((%wrap (datum->syntax exp (gensym "wrap"))))
           #'(begin
               (define-record-type name
                 #:printer print
                 (%wrap extern)
                 pred
                 (extern unwrap))
               (define wrap
                 (cond-expand
                  (guile-vm %wrap)
                  (hoot
                   ;; Use a weak map so that if two externs are eq?
                   ;; then their wrappers are also eq?
                   (let ((table (make-weak-map)))
                     (lambda (extern)
                       (or (weak-map-ref table extern)
                           (let ((wrapped (%wrap extern)))
                             (weak-map-set! table extern wrapped)
                             wrapped))))))))))
        ((_ name pred wrap unwrap)
         #'(define-external-type name pred wrap unwrap
             (lambda (obj port)
               (display "#<" port)
               (display 'name port)
               (display ">" port)))))))

  (define-syntax define-foreign
    (lambda (stx)
      (define (type-check exp proc-name)
        (define (check param predicate)
          #`(check-type #,param #,predicate '#,proc-name))
        (syntax-case exp (i32 i64 f32 f64 ref null eq string extern)
          ((x i32) (check #'x #'exact-integer?))
          ((x i64) (check #'x #'exact-integer?))
          ((x f32) (check #'x #'real?))
          ((x f64) (check #'x #'real?))
          ((x (ref eq)) #'#t)
          ((x (ref extern)) (check #'x #'external-non-null?))
          ((x (ref null extern)) (check #'x #'external?))
          ((x (ref string)) (check #'x #'string?))
          ((x type) (syntax-violation 'define-foreign "unsupported param type"
                                      stx #'type))))
      (define (import-result-types exp)
        (syntax-case exp (none)
          (none #'())
          (type #'((result type)))))
      (define (result-types exp)
        (syntax-case exp (none i32 i64 f32 f64 ref null string extern)
          (none #'())
          (i32 #'((result i64)))
          (i64 #'((result i64)))
          (f32 #'((result f64)))
          (f64 #'((result f64)))
          ((ref string) #'((result (ref eq))))
          ((ref null string) #'((result (ref eq))))
          ((ref extern) #'((result (ref eq))))
          ((ref null extern) #'((result (ref eq))))
          ((ref eq) #'((result (ref eq))))
          (type (syntax-violation 'define-foreign "unsupported result type"
                                  stx #'type))))
      (define (locals exp)
        (syntax-case exp (none i32 i64 f32 f64 ref null string extern)
          (none #'())
          (i32 #'())
          (i64 #'())
          (f32 #'())
          (f64 #'())
          ((ref string) #'())
          ((ref null string) #'((local $maybe-string (ref null string))))
          ((ref extern) #'())
          ((ref null extern) #'())
          ((ref eq) #'())
          (type (syntax-violation 'define-foreign "unsupported result type"
                                  stx #'type))))
      (define (lift-result exp)
        (syntax-case exp (none i32 i64 f32 f64 ref null string extern)
          ((x none) #'(x))
          ((x i32) #'((i64.extend_i32_s x)))
          ((x i64) #'(x))
          ((x f32) #'((f64.promote_f32 x)))
          ((x f64) #'(x))
          ((x (ref string)) #'((struct.new $string (i32.const 0) x)))
          ((x (ref null string))
           #'((local.set $maybe-string x)
              (if (ref eq)
                  (ref.is_null (local.get $maybe-string))
                  (then (ref.i31 (i32.const 1)))
                  (else (struct.new $string (i32.const 0)
                                    (ref.as_non_null
                                     (local.get $maybe-string)))))))
          ((x (ref extern)) #'((struct.new $extern-ref (i32.const 0) x)))
          ((x (ref null extern)) #'((struct.new $extern-ref (i32.const 0) x)))
          ((x (ref eq)) #'((ref.cast $heap-object x)))
          (type (syntax-violation 'define-foreign "unsupported result type"
                                  stx #'type))))
      (define (fresh-wasm-id prefix)
        (datum->syntax stx (gensym prefix)))
      (define (fresh-wasm-ids prefix lst)
        (map (lambda (_) (fresh-wasm-id prefix)) lst))
      (syntax-case stx (->)
        ((_ proc-name mod name ptype ... -> rtype)
         (and (string? (syntax->datum #'mod)) (string? (syntax->datum #'name)))
         (with-syntax ((iname (fresh-wasm-id "$import-"))
                       ((pname ...) (fresh-wasm-ids "$param-" #'(ptype ...))))
           #`(begin
               (cond-expand
                (guile-vm)
                (hoot
                 (%wasm-import
                  '(func iname (import mod name)
                         (param ptype) ...
                         #,@(import-result-types #'rtype)))))
               (define (proc-name pname ...)
                 #,@(map (lambda (exp) (type-check exp #'proc-name))
                         #'((pname ptype) ...))
                 (%inline-wasm
                  '(func (param pname ptype) ...
                         #,@(result-types #'rtype)
                         #,@(locals #'rtype)
                         #,@(lift-result
                             #'((call iname (local.get pname) ...) rtype)))
                  pname ...)))))))))
