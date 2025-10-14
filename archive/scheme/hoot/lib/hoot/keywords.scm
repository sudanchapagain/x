;;; Keywords
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
;;; Keywords.
;;;
;;; Code:

(library (hoot keywords)
  (export keyword?
          keyword->symbol
          symbol->keyword)
  (import (only (hoot primitives) %keyword? %symbol->keyword %keyword->symbol)
          (hoot syntax))

  (define (keyword? x) (%keyword? x))
  (define (symbol->keyword str) (%symbol->keyword str))
  (define (keyword->symbol sym) (%keyword->symbol sym)))
