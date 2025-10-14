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
;;; Tests for library-group.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (ice-9 match)
             (test utils)
             ((language tree-il) #:select (tree-il->scheme))
             (hoot library-group))

(test-begin "test-library-group")

(define* (parse-and-expand exp #:key (includes '()))
  (define (include-file file)
    (or (assoc-ref includes file)
        (error "library-group include clause forbidden" file)))
  (tree-il->scheme
   (expand-library-group
    (parse-library-group exp #:include-file include-file #:features '(a b c d))
    #:primitives '(hoot primitives))))

(define-syntax-rule (test-library-group exp expanded (file form ...) ...)
  (test-equal 'exp 'expanded
              (parse-and-expand 'exp #:includes '((file form ...) ...))))

(define-syntax-rule (test-invalid-library-group exp (file form ...) ...)
  (test-assert
   'exp
   (catch #t
          (lambda ()
            (parse-and-expand 'exp #:includes '((file form ...) ...))
            #f)
          (lambda _ #t))))

(test-invalid-library-group 42)
(test-invalid-library-group ())
(test-invalid-library-group '())

(test-library-group
 (library-group)
 (if #f #f))

(test-library-group
 (library-group
  (library (foo)
    (export a)
    (import (only (hoot primitives) define))
    (define a 42))
  (import (foo))
  a)
 (let ()
   (define a 42)
   a))

(test-library-group
 (library-group
  (library (foo)
    (export a)
    (import (only (hoot primitives) define))
    (define a 42))
  (library (bar)
    (export b)
    (import (only (hoot primitives) define))
    (define b 10))
  (import (foo) (bar)
          (rename (only (hoot primitives) %+)
                  (%+ +)))
  (+ a b))
 (let ()
   (define a 42)
   (define b 10)
   (+ a b)))

(test-library-group
 (library-group
  (library (foo)
    (export a)
    (import (only (hoot primitives) define))
    (define a 42))
  (library (bar)
    (export a)
    (import (only (hoot primitives) define))
    (define a 10))
  (import (foo)
    (rename (bar) (a b))
    (rename (only (hoot primitives) %+)
            (%+ +)))
  (+ a b))
 (let ()
   (define a-1 42)
   (define a 10)
   (+ a-1 a)))

(test-invalid-library-group
 (library-group
  (library (foo)
    (export a)
    (import (only (hoot primitives) define))
    (define a 42))
  (library (bar)
    (export a)
    (import (only (hoot primitives) define))
    (define a 10))
  #:untrusted
  (import (foo)
    (rename (bar) (a b))
    (rename (only (hoot primitives) %+)
            (%+ +)))
  (+ a b)))

(test-library-group
 (library-group
  (library (foo)
    (export a)
    (import (only (hoot primitives) define))
    (define a 42))
  (library (bar)
    (export a)
    (import (only (hoot primitives) define))
    (define a 10))
  (library (plus)
    (export +)
    (import (only (hoot primitives) define %+))
    (define (+ a b) (%+ a b)))
  #:untrusted
  (import (foo)
    (rename (bar) (a b))
    (plus))
  (+ a b))
 (let ()
   (define a-1 42)
   (define a 10)
   (define (+-1 a b) (+ a b))
   (+-1 a-1 a)))

(test-library-group
 (library-group
  (library (ctplus)
    (export (rename ctplus +))
    (import (hoot primitives))
    (define-syntax ctplus
      (lambda (stx)
        (syntax-case stx ()
          ((_ a b)
           (%+ (%syntax->datum #'a)
               (%syntax->datum #'b)))))))
  (import (ctplus))
  (+ 42 10))
 (let ()
   (define _ (if #f #f)) ;; The ctplus binding, not residualized.
   52))

(test-library-group
 (library-group
  (library (ct10)
    (export ten)
    (import (hoot primitives))
    (define ten 10))
  (library (ctplus10)
    (export ctplus10)
    (import (hoot primitives) (ct10))
    (define-syntax ctplus10
      (lambda (stx)
        (syntax-case stx ()
          ((_ a)
           (%+ (%syntax->datum #'a) ten))))))
  (import (ctplus10))
  (ctplus10 42))
 (let ()
   (define ten 10)
   (define _ (if #f #f)) ;; The ctplus10 binding, not residualized.
   52))

(test-library-group
 (library-group
  (library (inc)
    (export inc)
    (import (hoot primitives))
    (define-syntax 1+
      (lambda (stx)
        (syntax-case stx ()
          ((_ x) #'(%+ x 1)))))
    (define (inc x) (1+ x)))
  (import (inc))
  (inc 42))
 ;; A very silly tree-il->scheme rendering, but it is correct.
 (let inc ((x 42))
   (+ x 1)))

(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define bar 42)
   bar)
 ("foo" (library (foo)
          (export bar)
          (import (hoot primitives))
          (define bar 42))))

;; Basic guile module.
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define bar 42)
   bar)
 ("foo"
  (define-module (foo)
    #:use-module (hoot primitives)
    #:pure
    #:export (bar))
  (define bar 42)))

;; Renaming export.
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define baz 42)
   baz)
 ("foo"
  (define-module (foo)
    #:use-module (hoot primitives)
    #:pure
    #:export ((baz . bar)))
  (define baz 42)))

;; Selecting a specific imports.
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define bar (+ 42 69))
   bar)
 ("foo"
  (define-module (foo)
    #:use-module ((hoot primitives) #:select (define %+))
    #:pure
    #:export (bar))
  (define bar (%+ 42 69))))

;; Renaming a specific imports.
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define bar (+ 42 69))
   bar)
 ("foo"
  (define-module (foo)
    #:use-module ((hoot primitives) #:select (define (%+ . +)))
    #:pure
    #:export (bar))
  (define bar (+ 42 69))))

;; Prefix.
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define bar (+ 42 69))
   bar)
 ("foo"
  (define-module (foo)
    #:use-module ((hoot primitives) #:select (define (%+ . +)) #:prefix base:)
    #:pure
    #:export (bar))
  (base:define bar (base:+ 42 69))))

;; symbol-prefix-proc
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define bar (+ 42 69))
   bar)
 ("foo"
  (define-module (foo)
    #:use-module ((hoot primitives) #:select (define (%+ . +))
                  #:renamer (symbol-prefix-proc 'base:))
    #:pure
    #:export (bar))
  (base:define bar (base:+ 42 69))))

;; Hiding definitions.
(test-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 (let ()
   (define (%* n y)
     (if (eq? y 1) n (+ n (%* n (- y 1)))))
   (define bar (%* 42 10)) bar)
 ("foo"
  (define-module (foo)
    #:use-module ((hoot primitives) #:hide (%*))
    #:pure
    #:export (bar))
  (define (%* n y) (if (%eq? y 1) n (%+ n (%* n (%- y 1)))))
  (define bar (%* 42 10))))

;; The (guile) module, added for impure modules, is not yet supported.
(test-invalid-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 ("foo"
  (define-module (foo)
    #:export (bar))
  (define bar 42)))

;; The (guile) module, added for impure modules, is not yet supported.
(test-invalid-library-group
 (library-group
  (include "foo")
  (import (foo))
  bar)
 ("foo"
  (define-module (foo)
    #:export (bar))
  (define bar 42)))

;; R7RS libraries.
(test-library-group
 (library-group
  (define-library (ct10)
    (export ten)
    (import (hoot primitives))
    (begin (define ten 10)))
  (define-library (ctplus10)
    (export ctplus10)
    (import (hoot primitives))
    (import (ct10))
    (begin (define-syntax ctplus10
             (lambda (stx)
               (syntax-case stx ()
                 ((_ a)
                  (%+ (%syntax->datum #'a) ten)))))))
  (import (ctplus10))
  (ctplus10 42))
 (let ()
   (define ten 10)
   (define _ (if #f #f)) ;; The ctplus10 binding, not residualized.
   52))

;; R7RS libraries.
(test-library-group
 (library-group
  (define-library (conditional)
    (export x)
    (import (hoot primitives))
    (cond-expand
     (q (begin (define x 42)))
     (a (begin (define x 69)))))
  (import (conditional))
  x)
 (let ()
   (define x 69)
   x))

(test-library-group
 (library-group
  (define-library (foo)
    (export x)
    (import (hoot primitives))
    (begin (define x 42)))
  (define-library (bar)
    (export x)
    (import (hoot primitives))
    (begin (define x 69)))
  (define-library (conditional)
    (export y)
    (cond-expand
     (a (import (hoot primitives) (foo)))
     (q (import (hoot primitives) (bar))))
    (begin (define y x)))
  (import (conditional))
  y)
 (let ()
   (define x 42)
   (define x-1 69)
   (define y x)
   y))

(test-library-group
 (library-group
  (define-library (conditional)
    (export x)
    (import (hoot primitives))
    (include-library-definitions "foo"))
  (import (conditional))
  x)
 (let ()
   (define x 69)
   x)
 ("foo"
  (cond-expand
   (q (begin (define x 42)))
   (a (begin (define x 69))))))

;; Duplicate definitions are allowed in Guile modules.
(test-library-group
 (library-group
  (include "guile")
  (include "foo")
  (include "bar")
  (use-modules (foo) (bar))
  baz)
 (let ()
   (define _ (if #f #f))
   (define baz 42)
   (define baz 84)
   baz)
 ("guile"
  (define-module (guile)
    #:pure
    #:use-module ((hoot primitives) #:select (define))
    #:re-export (define)))
 ("foo"
  (define-module (foo)
    #:export (baz))
  (define baz 42))
 ("bar"
  (define-module (bar)
    #:export (baz))
  (define baz 84)))

(test-end* "test-library-group")
