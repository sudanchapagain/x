;;; Hoot weak vectors module
;;; Copyright (C) 2024 Vivianne Langdon <puttabutta@gmail.com>
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
;;; Hoot weak vectors module.
;;;
;;; Code:

(library (hoot weak-vectors)
  (export make-weak-vector
          weak-vector
          list->weak-vector
          weak-vector?
          weak-vector-ref
          weak-vector-set!
          weak-vector-length)
  (import (hoot inline-wasm)
          (hoot lists)
          (hoot match)
          (hoot numbers)
          (hoot not)
          (hoot pairs)
          (hoot ports)
          (hoot records)
          (hoot syntax)
          (hoot vectors)
          (hoot weak-refs)
          (hoot write)
          (only (guile)
                *unspecified*))

  (define (immediate? x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (if (ref eq)
                (ref.test i31 (local.get $x))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     x))

  (define-record-type <weak-vector>
    #:printer (lambda (wvec port)
                (let ((len (weak-vector-length wvec)))
                  (write-string "#<weak-vector" port)
                  (do ((i 0 (1+ i)))
                      ((= i len))
                    (write-char #\space port)
                    (write (weak-vector-ref wvec i) port)))
                (write-string ">" port))
    (%make-weak-vector vec)
    weak-vector?
    (vec weak-vector-vec))

  (define (maybe-make-weak-ref x)
    ;; immediates are simply stored as-is and not wrapped in a weak ref
    (if (immediate? x) x (make-weak-ref x)))

  (define* (make-weak-vector size #:optional (fill *unspecified*))
    (%make-weak-vector (make-vector size (maybe-make-weak-ref fill))))

  (define (weak-vector . elems) (list->weak-vector elems))

  (define (list->weak-vector l)
    (define vec (make-weak-vector (length l)))
    (let lp ((i 0) (l l))
      (match l
        (() vec)
        ((x . rest)
         (weak-vector-set! vec i x)
         (lp (1+ i) rest)))))

  (define (weak-vector-ref wvect k)
    (let ((ref (vector-ref (weak-vector-vec wvect) k)))
      (if (weak-ref? ref)
          (let ((item (weak-ref-deref ref)))
            (and (not (weak-ref-null? item)) item))
          ref)))

  (define (weak-vector-set! wvect k elt)
    (vector-set! (weak-vector-vec wvect) k (maybe-make-weak-ref elt)))

  (define (weak-vector-length wvect)
    (vector-length (weak-vector-vec wvect))))
