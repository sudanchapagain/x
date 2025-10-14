;;; Symbols
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
;;; Symbols.
;;;
;;; Code:

(library (hoot symbols)
  (export symbol?
          symbol->string
          string->symbol)
  (import (only (hoot primitives)
                %symbol? %string? %string->symbol %symbol->string)
          (hoot errors)
          (hoot syntax))

  (define (symbol? x) (%symbol? x))

  (define (string->symbol str)
    (check-type str %string? 'string->symbol)
    (%string->symbol str))

  (define (symbol->string sym)
    (check-type sym symbol? 'symbol->string)
    (%symbol->string sym)))
