;;; Atomic boxes
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
;;; Atomic boxes.
;;;
;;; Code:

(library (hoot atomics)
  (export make-atomic-box
          atomic-box-ref
          atomic-box-set!
          atomic-box-swap!
          atomic-box-compare-and-swap!)
  (import (only (hoot primitives)
                %make-atomic-box %atomic-box-ref %atomic-box-set!
                %atomic-box-swap! %atomic-box-compare-and-swap!)
          (hoot syntax))

  (define (make-atomic-box x) (%make-atomic-box x))
  (define (atomic-box-ref x) (%atomic-box-ref x))
  (define (atomic-box-set! x y) (%atomic-box-set! x y))
  (define (atomic-box-swap! x y) (%atomic-box-swap! x y))
  (define (atomic-box-compare-and-swap! x y z) (%atomic-box-compare-and-swap! x y z)))
