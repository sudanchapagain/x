;;; Error constructors
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
;;; Exception constructors for common errors.
;;;
;;; Code:

(library (hoot errors)
  (export make-size-error
          make-index-error
          make-range-error
          make-start-offset-error
          make-end-offset-error
          make-type-error
          make-unimplemented-error
          make-assertion-error
          make-not-seekable-error
          make-runtime-error-with-message
          make-runtime-error-with-message+irritants
          make-match-error
          make-arity-error
          make-invalid-keyword-error
          make-unrecognized-keyword-error
          make-missing-keyword-argument-error
          make-syntax-violation

          raise
          raise-continuable
          raise-exception
          with-exception-handler

          error
          assert
          check-size
          check-index
          check-range
          check-type)
  (import (only (hoot primitives) %raise-exception %exact-integer? %< %<=)
          (hoot inline-wasm)
          (hoot syntax))

  (define-syntax-rule (define-error-constructor (name arg ...) global)
    (define (name arg ...)
      ((%inline-wasm '(func (result (ref eq)) (global.get global))) arg ...)))

  (define-error-constructor (make-size-error val max who)
    $make-size-error)
  (define-error-constructor (make-index-error val size who)
    $make-index-error)
  (define-error-constructor (make-range-error val min max who)
    $make-range-error)
  (define-error-constructor (make-start-offset-error val size who)
    $make-start-offset-error)
  (define-error-constructor (make-end-offset-error val size who)
    $make-end-offset-error)
  (define-error-constructor (make-type-error val who what)
    $make-type-error)
  (define-error-constructor (make-unimplemented-error who)
    $make-unimplemented-error)
  (define-error-constructor (make-assertion-error expr who)
    $make-assertion-error)
  (define-error-constructor (make-not-seekable-error port who)
    $make-not-seekable-error)
  (define-error-constructor (make-runtime-error-with-message msg)
    $make-runtime-error-with-message)
  (define-error-constructor (make-runtime-error-with-message+irritants msg irritants)
    $make-runtime-error-with-message+irritants)
  (define-error-constructor (make-match-error v)
    $make-match-error)
  (define-error-constructor (make-arity-error v who)
    $make-arity-error)
  (define-error-constructor (make-invalid-keyword-error kw)
    $make-invalid-keyword-error)
  (define-error-constructor (make-unrecognized-keyword-error kw)
    $make-unrecognized-keyword-error)
  (define-error-constructor (make-missing-keyword-argument-error kw)
    $make-missing-keyword-argument-error)
  (define-error-constructor (make-syntax-violation who message form subform)
    $make-syntax-violation)

  (define (raise exn) (%raise-exception exn))
  (define (raise-continuable exn)
    ((%inline-wasm '(func (result (ref eq))
                          (global.get $raise-exception)))
     exn #:continuable? #t))
  (define raise-exception
    (case-lambda*
     ((exn) (%raise-exception exn))
     ((exn #:key continuable?)
      (if continuable?
          (raise-continuable exn)
          (%raise-exception exn)))))

  (define* (with-exception-handler handler thunk #:key (unwind? #f) (unwind-for-type #t))
    ((%inline-wasm
      '(func (result (ref eq))
             (global.get $with-exception-handler)))
     handler thunk #:unwind? unwind? #:unwind-for-type unwind-for-type))

  (define error
    (case-lambda
     ((msg)
      (raise (make-runtime-error-with-message msg)))
     ((msg . args)
      (raise (make-runtime-error-with-message+irritants msg args)))))

  (define-syntax-rule (assert expr who)
    (unless expr
      (raise (make-assertion-error 'expr who))))
  (define-syntax-rule (check-size x max who)
    (unless (and (%exact-integer? x) (%<= 0 x) (%<= x max))
      (raise (make-size-error x max who))))
  (define-syntax-rule (check-index x size who)
    (unless (and (%exact-integer? x) (%<= 0 x) (%< x size))
      (raise (make-index-error x size who))))
  (define-syntax-rule (check-range x min max who)
    (unless (and (%exact-integer? x) (%<= min x) (%<= x max))
      (raise (make-range-error x min max who))))
  (define-syntax-rule (check-type x predicate who)
    (unless (predicate x)
      (raise (make-type-error x who 'predicate)))))
