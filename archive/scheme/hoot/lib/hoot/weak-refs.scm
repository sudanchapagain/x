;;; Hoot weak references
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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
;;; Host weak ref bindings.
;;;
;;; Code:

(library (hoot weak-refs)
  (export make-weak-ref weak-ref? weak-ref-deref weak-ref-null?)
  (import (hoot eq)
          (hoot ffi)
          (hoot pairs)
          (hoot syntax))

  (define-foreign %make-weak-ref
    "rt" "make_weak_ref"
    (ref eq) -> (ref extern))
  (define-foreign %weak-ref-deref
    "rt" "weak_ref_deref"
    (ref extern) (ref eq) -> (ref eq))

  (define %weak-ref-null (list 'weak-ref-null))
  (define (weak-ref-null? obj)
    (eq? obj %weak-ref-null))

  (define-external-type <weak-ref>
    weak-ref?
    wrap-weak-ref
    unwrap-weak-ref)

  (define (make-weak-ref val)
    (wrap-weak-ref (%make-weak-ref val)))

  (define (weak-ref-deref ref)
    (%weak-ref-deref (unwrap-weak-ref ref) %weak-ref-null)))
