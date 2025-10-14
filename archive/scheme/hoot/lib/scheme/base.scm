;;; R7RS (scheme base)
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
;;; A definition of R7RS (scheme base).
;;;
;;; Code:

(library (scheme base)
  (export *
          +
          -
          ...
          _
          /
          <
          <=
          =
          =>
          >
          >=
          abs
          and
          append
          apply
          assoc
          assq
          assv
          begin
          binary-port?
          boolean=?
          boolean?
          bytevector
          bytevector-append
          bytevector-copy
          bytevector-copy!
          bytevector-length
          bytevector-u8-ref
          bytevector-u8-set!
          bytevector?
          caar
          cadr
          call-with-current-continuation
          call-with-port
          call-with-values
          call/cc
          car
          case
          cdar
          cddr
          cdr
          ceiling
          char->integer
          char-ready?
          char<=?
          char<?
          char=?
          char>=?
          char>?
          char?
          close-input-port
          close-output-port
          close-port
          complex?
          cond
          cond-expand
          cons
          current-error-port
          current-input-port
          current-output-port
          define
          define-record-type
          define-syntax
          define-values
          denominator
          do
          dynamic-wind
          else
          eof-object?
          equal?
          error
          error-object-message
          even?
          exact-integer-sqrt
          exact?
          features
          floor
          floor-remainder
          flush-output-port
          gcd
          get-output-string
          if
          include-ci
          inexact?
          input-port?
          integer?
          lcm
          let
          let*-values
          let-values
          letrec*
          list
          list->vector
          list-ref
          list-tail
          make-bytevector
          make-parameter
          make-vector
          max
          memq
          min
          negative?
          not
          number->string
          numerator
          open-input-bytevector
          open-output-bytevector
          or
          output-port?
          parameterize
          peek-u8
          positive?
          quasiquote
          quotient
          raise-continuable
          rationalize
          read-bytevector!
          read-error?
          read-string
          real?
          reverse
          set!
          set-cdr!
          string
          string->number
          string->utf8
          string-append
          eof-object
          eq?
          eqv?
          error-object-irritants
          error-object?
          exact
          exact-integer?
          expt
          file-error?
          floor-quotient
          floor/
          for-each
          get-output-bytevector
          guard
          include
          inexact
          input-port-open?
          integer->char
          lambda
          length
          let*
          let-syntax
          letrec
          letrec-syntax
          list->string
          list-copy
          list-set!
          list?
          make-list
          make-string
          map
          member
          memv
          modulo
          newline
          null?
          number?
          odd?
          open-input-string
          open-output-string
          output-port-open?
          pair?
          peek-char
          port?
          procedure?
          quote
          raise
          rational?
          read-bytevector
          read-char
          read-line
          read-u8
          remainder
          round
          set-car!
          square
          string->list
          string->symbol
          string->vector
          string-copy
          string-copy!
          string-for-each
          string-map
          string-set!
          string<?
          string>=?
          string?
          symbol->string
          symbol?
          syntax-rules
          truncate
          truncate-remainder
          u8-ready?
          unquote
          utf8->string
          vector
          vector->string
          vector-copy
          vector-fill!
          vector-length
          vector-ref
          vector?
          with-exception-handler
          write-char
          write-u8
          string-fill!
          string-length
          string-ref
          string<=?
          string=?
          string>?
          substring
          symbol=?
          syntax-error
          textual-port?
          truncate-quotient
          truncate/
          unless
          unquote-splicing
          values
          vector->list
          vector-append
          vector-copy!
          vector-for-each
          vector-map
          vector-set!
          when
          write-bytevector
          write-string
          zero?)
  (import (hoot syntax)
          (hoot features)
          (hoot cond-expand)
          (hoot apply)
          (hoot bytevectors)
          (hoot char)
          (hoot control)
          (hoot dynamic-wind)
          (hoot eq)
          (hoot error-handling)
          (hoot not)
          (hoot parameters)
          (hoot pairs)
          (hoot procedures)
          (hoot ports)
          (hoot errors)
          (only (hoot read) string->number)
          (hoot exceptions)
          (hoot equal)
          (hoot lists)
          (hoot assoc)
          (hoot numbers)
          (hoot match)
          (hoot strings)
          (hoot symbols)
          (hoot write)
          (hoot values)
          (hoot vectors)
          (srfi srfi-9))

  ;; Here we should have definitions of procedures that aren't generally
  ;; useful: they only exist for conformity with R7RS.
  (define (boolean? x) (match x ((or #f #t) #t) (_ #f)))
  (define boolean=?
    (case-lambda
     ((x y)
      (check-type x boolean? 'boolean=?)
      (check-type y boolean? 'boolean=?)
      (eq? x y))
     ((x y . z)
      (let lp ((z z) (res (boolean=? x y)))
        (match z
          (() res)
          ((y . z)
           (lp z (boolean=? x y))))))))

  (define (symbol=? x y . z)
    (check-type x symbol? 'symbol=?)
    (check-type y symbol? 'symbol=?)
    (for-each (lambda (z) (check-type z symbol? 'symbol=?)) z)
    (apply eq? x y z))

  (define* (string->vector str #:optional (start 0)
                           (end (string-length string)))
    (list->vector (string->list str start end)))
  (define* (vector->string v #:optional (start 0) (end (vector-length v)))
    (list->string (vector->list v start end)))

  (define (error-object? x)
    (and (exception-with-message? x)
         (exception-with-irritants? x)))
  (define error-object-message exception-message)
  (define error-object-irritants exception-irritants)
  (define read-error? lexical-violation?)
  (define file-error? i/o-error?))
