;;; Pairs
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
;;; Pairs, and null?.
;;;
;;; Code:

(library (hoot pairs)
  (export null?
          pair?
          cons
          car
          cdr
          set-car!
          set-cdr!
          cons*
          list
          
          caar cdar cadr cddr
          caaar cadar caadr caddr cdaar cddar cdadr cdddr
          caaaar caadar caaadr caaddr cadaar caddar cadadr cadddr
          cdaaar cdadar cdaadr cdaddr cddaar cdddar cddadr cddddr)
  (import (only (hoot primitives)
                %null? %pair? %cons %car %cdr %set-car! %set-cdr!)
          (hoot apply)
          (hoot syntax))

  (define (null? x) (%null? x))
  (define (pair? p) (%pair? p))
  (define (cons x y) (%cons x y))
  (define (car x) (%car x))
  (define (cdr x) (%cdr x))

  (define (set-car! x y) (%set-car! x y))
  (define (set-cdr! x y) (%set-cdr! x y))

  (define (%generic-cons* head . tail)
    (if (null? tail)
        head
        (cons head (apply %generic-cons* tail))))
  (define-syntax cons*
    (lambda (stx)
      (syntax-case stx ()
        ((_) #'(%generic-cons*))
        ((_ a) #'a)
        ((_ a . b) #'(%cons a (cons* . b)))
        (f (identifier? #'f) #'%generic-cons*))))

  (define (list . args) args)

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cdar x) (cdr (car x)))
  (define (cddr x) (cdr (cdr x)))
  (define (caaar x) (car (car (car x))))
  (define (cadar x) (car (cdr (car x))))
  (define (caadr x) (car (car (cdr x))))
  (define (caddr x) (car (cdr (cdr x))))
  (define (cdaar x) (cdr (car (car x))))
  (define (cddar x) (cdr (cdr (car x))))
  (define (cdadr x) (cdr (car (cdr x))))
  (define (cdddr x) (cdr (cdr (cdr x))))
  (define (caaaar x) (car (car (car (car x)))))
  (define (caadar x) (car (car (cdr (car x)))))
  (define (caaadr x) (car (car (car (cdr x)))))
  (define (caaddr x) (car (car (cdr (cdr x)))))
  (define (cadaar x) (car (cdr (car (car x)))))
  (define (caddar x) (car (cdr (cdr (car x)))))
  (define (cadadr x) (car (cdr (car (cdr x)))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))
  (define (cdaaar x) (cdr (car (car (car x)))))
  (define (cdadar x) (cdr (car (cdr (car x)))))
  (define (cdaadr x) (cdr (car (car (cdr x)))))
  (define (cdaddr x) (cdr (car (cdr (cdr x)))))
  (define (cddaar x) (cdr (cdr (car (car x)))))
  (define (cdddar x) (cdr (cdr (cdr (car x)))))
  (define (cddadr x) (cdr (cdr (car (cdr x)))))
  (define (cddddr x) (cdr (cdr (cdr (cdr x))))))
