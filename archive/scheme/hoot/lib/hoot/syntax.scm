;;; Syntax primitives
;;; Copyright (C) 2024, 2025 Igalia, S.L.
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
;;; Primitive syntax.
;;;
;;; Code:

(library (hoot syntax)
  (export _ ... => else
          lambda case-lambda define
          lambda* case-lambda* define*
          let let* letrec letrec*
          begin if set! or and cond case when unless do
          quote quasiquote unquote unquote-splicing
          define-syntax let-syntax letrec-syntax
          syntax-rules syntax-error define-syntax-rule
          syntax-case syntax quasisyntax unsyntax unsyntax-splicing
          quote-syntax with-syntax identifier-syntax
          include include-ci include-from-path

          make-variable-transformer
          identifier? generate-temporaries free-identifier=? bound-identifier=?
          syntax-local-binding syntax-violation)
  (import (hoot core-syntax)
          (hoot core-syntax-helpers)))
