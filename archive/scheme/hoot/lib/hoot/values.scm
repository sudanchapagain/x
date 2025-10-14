;;; Multiple values
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
;;; Multiple values.
;;;
;;; Code:

(library (hoot values)
  (export call-with-values (rename %values values)
          define-values let-values let*-values)
  (import (only (hoot primitives)
                %values %call-with-values
                %cons %car %cdr)
          (hoot apply)
          (hoot syntax))

  (define-syntax call-with-values
    (lambda (stx)
      (syntax-case stx (lambda)
        ((_ producer (lambda args body0 body ...))
         #'(%call-with-values producer (lambda args body0 body ...)))
        ((_ producer consumer)
         #'(%call-with-values producer (lambda args (apply consumer args))))
        (id (identifier? #'id)
            #'(lambda (producer consumer)
                (let ((p producer) (c consumer))
                  (%call-with-values p (lambda args (apply c args)))))))))

  (define-syntax list
    (syntax-rules ()
      ((_) '())
      ((_ head . tail) (%cons head (list . tail)))))

  (define-syntax define-values
    (lambda (stx)
      (syntax-case stx ()
        ((_ () expr)
         #'(%call-with-values (lambda () expr)
                              (lambda () (%values))))
        ((_ (val) expr)
         #'(define val
             (%call-with-values (lambda () expr)
                                (lambda (x) x))))
        ((_ (val ...) expr)
         (with-syntax (((vals) (generate-temporaries '(vals))))
           #'(begin
               (define vals
                 (%call-with-values (lambda () expr)
                                    (lambda (val ...) (list val ...))))
               (define val
                 (let ((x (%car vals)))
                   (set! vals (%cdr vals))
                   x))
               ...))))))

  (define-syntax let*-values
    (syntax-rules ()
      ((_ () . body)
       (let () . body))
      ((_ ((vars expr) . clauses) . body)
       (%call-with-values (lambda () expr)
                          (lambda vars
                            (let*-values clauses . body))))))

  (define-syntax let-values
    (lambda (stx)
      (syntax-case stx ()
        ((_ ((vars expr) ...) . body)
         (with-syntax (((thunk ...) (generate-temporaries #'(expr ...))))
           #'(let ((thunk (lambda () expr))
                   ...)
               (let*-values ((vars (thunk)) ...)
                 . body))))))))
