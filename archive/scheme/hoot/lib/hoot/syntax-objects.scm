;;; Syntax objects
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
;;; Syntax objects.
;;;
;;; Code:

(library (hoot syntax-objects)
  (export syntax?
          syntax-expression
          syntax-wrap
          syntax-module
          syntax-sourcev
          make-syntax
          syntax->datum
          datum->syntax)
  (import (only (hoot primitives)
                guile:syntax?
                guile:syntax-expression
                guile:syntax-wrap
                guile:syntax-module
                guile:syntax-sourcev
                guile:make-syntax
                %datum->syntax
                %syntax->datum)
          (hoot inline-wasm)
          (hoot syntax)
          (hoot errors)
          (hoot eq)
          (hoot pairs)
          (hoot lists)
          (hoot vectors)
          (hoot cond-expand))

  (cond-expand
   (guile-vm
    (define syntax? guile:syntax?)
    (define syntax-expression guile:syntax-expression)
    (define syntax-wrap guile:syntax-wrap)
    (define syntax-module guile:syntax-module)
    (define syntax-sourcev guile:syntax-sourcev)
    (define make-syntax guile:make-syntax)
    (define syntax->datum %syntax->datum)
    (define datum->syntax %datum->syntax))
   (else
    (define (syntax? x)
      (%inline-wasm
       '(func (param $obj (ref eq)) (result (ref eq))
              (if (ref eq)
                  (ref.test $syntax (local.get $obj))
                  (then (ref.i31 (i32.const 17)))
                  (else (ref.i31 (i32.const 1)))))
       x))
    (define (syntax-expression stx)
      (check-type stx syntax? 'syntax-expression)
      (%inline-wasm
       '(func (param $stx (ref $syntax)) (result (ref eq))
              (struct.get $syntax $expr (local.get $stx)))
       stx))
    (define (syntax-wrap stx)
      (check-type stx syntax? 'syntax-wrap)
      (%inline-wasm
       '(func (param $stx (ref $syntax)) (result (ref eq))
              (struct.get $syntax $wrap (local.get $stx)))
       stx))
    (define (syntax-module stx)
      (check-type stx syntax? 'syntax-module)
      (%inline-wasm
       '(func (param $stx (ref $syntax)) (result (ref eq))
              (struct.get $syntax $module (local.get $stx)))
       stx))
    (define (syntax-sourcev stx)
      (check-type stx syntax? 'syntax-sourcev)
      (%inline-wasm
       '(func (param $stx (ref $syntax)) (result (ref eq))
              (struct.get $syntax $source (local.get $stx)))
       stx))
    (define (make-syntax expr wrap module source)
      (%inline-wasm
       '(func (param $expr (ref eq)) (param $wrap (ref eq))
              (param $module (ref eq)) (param $source (ref eq))
              (result (ref eq))
              (struct.new $syntax
                          (i32.const 0)
                          (local.get $expr) (local.get $wrap)
                          (local.get $module) (local.get $source)))
       expr wrap module source))
    (define (syntax->datum stx)
      (let strip ((x stx))
        (cond
         ((syntax? x) (strip (syntax-expression x)))
         ((pair? x)
          (cons (strip (car x)) (strip (cdr x))))
         ((vector? x)
          (list->vector (strip (vector->list x))))
         (else x))))
    (define* (datum->syntax id datum #:key source)
      (define empty-wrap '(()))
      (make-syntax datum
                   (if id
                       (syntax-wrap id)
                       empty-wrap)
                   (if id
                       (syntax-module id)
                       #f)
                   (if (and (vector? source) (eq? 3 (vector-length source)))
                       source
                       (and source (syntax-sourcev source))))))))
