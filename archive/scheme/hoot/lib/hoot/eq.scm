;;; Eq, eqv
;;; Copyright (C) 2023, 2024 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
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
;;; eq? and eqv?
;;;
;;; Code:

(library (hoot eq)
  (export eq? eqv?)
  (import (only (hoot primitives) %eq? %eqv?)
    (hoot syntax))

  (define (eq? x y) (%eq? x y))
  (define (eqv? x y) (%eqv? x y)))
