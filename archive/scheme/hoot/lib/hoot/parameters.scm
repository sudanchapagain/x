;;; Parameters
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
;;; Parameters.
;;;
;;; Code:

(library (hoot parameters)
  (export make-parameter parameter? parameterize)
  (import (hoot cond-expand)
          (hoot errors)
          (hoot fluids)
          (hoot inline-wasm)
          (only (hoot primitives)
                guile:make-parameter guile:parameter?
                guile:parameter-fluid guile:parameter-converter)
          (hoot syntax))

  (cond-expand
   (guile-vm
    (define make-parameter guile:make-parameter)
    (define parameter? guile:parameter?)
    (define parameter-fluid guile:parameter-fluid)
    (define parameter-convert guile:parameter-converter))
   (else
    (define* (make-parameter init #:optional (conv (lambda (x) x)))
      (let ((fluid (make-fluid (conv init))))
        (%inline-wasm
         '(func (param $fluid (ref eq))
                (param $convert (ref eq))
                (result (ref eq))
                (struct.new $parameter
                            (i32.const 0)
                            (ref.func $parameter)
                            (ref.cast $fluid (local.get $fluid))
                            (ref.cast $proc (local.get $convert))))
         fluid conv)))

    (define (parameter? x)
      (%inline-wasm
       '(func (param $x (ref eq)) (result (ref eq))
              (if (ref eq)
                  (ref.test $parameter (local.get $x))
                  (then (ref.i31 (i32.const 17)))
                  (else (ref.i31 (i32.const 1)))))
       x))
    (define (parameter-fluid x)
      (%inline-wasm
       '(func (param $param (ref $parameter)) (result (ref eq))
              (struct.get $parameter $fluid (local.get $param)))
       x))
    (define (parameter-convert x)
      (%inline-wasm
       '(func (param $param (ref $parameter)) (result (ref eq))
              (struct.get $parameter $convert (local.get $param)))
       x))))
               
  (define-syntax parameterize
    (lambda (x)
      (syntax-case x ()
        ((_ ((parameter value) ...) body body* ...)
         (with-syntax (((p ...) (generate-temporaries #'(parameter ...))))
           #'(let ((p parameter) ...)
               (check-type p parameter? 'parameterize)
               ...
               (with-fluids (((parameter-fluid p) ((parameter-convert p) value))
                             ...)
                 body body* ...))))))))
