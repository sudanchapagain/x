;;; Fluids
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
;;; Fluids and dynamic states.
;;;
;;; Code:

(library (hoot fluids)
  (export make-fluid
          fluid?
          fluid-ref
          fluid-set!
          with-fluid*
          with-fluids)
  (import (only (hoot primitives)
                %fluid-ref %fluid-set! %with-fluid*
                guile:make-fluid guile:fluid?)
          (hoot cond-expand)
          (hoot inline-wasm)
          (hoot syntax))

  (cond-expand
   (guile-vm
    (define make-fluid guile:make-fluid)
    (define fluid? guile:fluid?))
   (else
    (define* (make-fluid #:optional default-value)
      (%inline-wasm '(func (param $default (ref eq)) (result (ref eq))
                           (struct.new $fluid (i32.const 0)
                                       (local.get $default)))
                    default-value))
    ;; FIXME: We should just add support for the fluid? CPS primitive
    ;; to the backend and emit this code directly.
    (define (fluid? x)
      (%inline-wasm
       '(func (param $x (ref eq)) (result (ref eq))
              (if (ref eq)
                  (ref.test $fluid (local.get $x))
                  (then (ref.i31 (i32.const 17)))
                  (else (ref.i31 (i32.const 1)))))
       x))))

  (define (fluid-ref x) (%fluid-ref x))
  (define (fluid-set! x y) (%fluid-set! x y))
  (define (with-fluid* fluid val thunk) (%with-fluid* fluid val thunk))

  (define-syntax with-fluids
    (lambda (stx)
      (define (emit-with-fluids bindings body)
        (syntax-case bindings ()
          (()
           body)
          (((f v) . bindings)
           #`(with-fluid* f v
               (lambda ()
                 #,(emit-with-fluids #'bindings body))))))
      (syntax-case stx ()
        ((_ ((fluid val) ...) exp exp* ...)
         (with-syntax (((fluid-tmp ...) (generate-temporaries #'(fluid ...)))
                       ((val-tmp ...) (generate-temporaries #'(val ...))))
           #`(let ((fluid-tmp fluid) ...)
               (let ((val-tmp val) ...)
                 #,(emit-with-fluids #'((fluid-tmp val-tmp) ...)
                                     #'(let () exp exp* ...))))))))))
