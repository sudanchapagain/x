;;; (hoot chars) library
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
;;; Basic parts of (scheme chars).
;;;
;;; Code:

(library (hoot char)
  (export char->integer
          integer->char
          char?
          char<?
          char<=?
          char=?
          char>=?
          char>?

          char-upcase
          char-downcase
          char-alphabetic?
          char-lower-case?
          char-numeric?
          char-upper-case?
          char-whitespace?

          string-upcase
          string-downcase)
  (import (rename (only (hoot primitives)
                        %+ %- %string? %vector-ref %< %<= %= %>= %>
                        %char->integer %integer->char %char?)
                  (%+ +)
                  (%- -)
                  (%string? string?)
                  (%vector-ref vector-ref)
                  (%< <) (%<= <=) (%= =) (%>= >=) (%> >))
          (hoot bitvectors)
          (hoot bitwise)
          (hoot errors)
          (hoot inline-wasm)
          (hoot match)
          (hoot syntax))

  (define (char->integer x) (%char->integer x))
  (define (integer->char x) (%integer->char x))
  (define (char? x) (%char? x))

  (define-syntax-rule (define-comparison-expansion name cmp)
    (define name
      (case-lambda
       ((a b) (cmp a b))
       ((a b . c)
        (let lp ((res (cmp a b)) (a b) (c c))
          (match c
            (() res)
            ((b . c)
             (lp (and (cmp a b) res) b c))))))))
  (define-syntax-rule (define-char-comparison-expansion name cmp)
    (define-comparison-expansion name
      (lambda (a b) (cmp (char->integer a) (char->integer b)))))

  (define-char-comparison-expansion char<? <)
  (define-char-comparison-expansion char<=? <=)
  (define-char-comparison-expansion char=? =)
  (define-char-comparison-expansion char>=? >=)
  (define-char-comparison-expansion char>? >)

  ;; generated (scheme char) procedures:
  ;;   char-upcase
  ;;   char-downcase
  ;;   char-upper-case?
  ;;   char-lower-case?
  ;;   char-alphabetic?
  ;;   char-numeric?
  ;;   char-whitespace?
  (include-from-path "hoot/char-prelude")

  (define (string-upcase str)
    (check-type str string? 'string-upcase)
    (%inline-wasm
     '(func (param $str (ref string))
            (result (ref eq))
            (struct.new $string
                        (i32.const 0)
                        (call $string-upcase (local.get $str))))
     str))
  (define (string-downcase str)
    (check-type str string? 'string-downcase)
    (%inline-wasm
     '(func (param $str (ref string))
            (result (ref eq))
            (struct.new $string
                        (i32.const 0)
                        (call $string-downcase (local.get $str))))
     str)))
