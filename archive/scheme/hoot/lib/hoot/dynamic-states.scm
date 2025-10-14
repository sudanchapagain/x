;;; Dynamic states
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
;;; Dynamic states.
;;;
;;; Code:

(library (hoot dynamic-states)
  (export current-dynamic-state
          dynamic-state?
          with-dynamic-state)
  (import (only (hoot primitives) %with-dynamic-state)
          (hoot debug)
          (hoot errors)
          (hoot inline-wasm)
          (hoot lists)
          (hoot match)
          (hoot numbers)
          (hoot syntax)
          (hoot values)
          (hoot vectors))

  (define (copy-alist alist)
    (match alist
      (() (values '() 0))
      (((k . v) . alist)
       (call-with-values (lambda () (copy-alist alist))
         (lambda (alist len)
           (values (acons k v alist) (1+ len)))))))
  (define (copy-hash-table table)
    (define buckets
      (%inline-wasm
       '(func (param $table (ref $hash-table)) (result (ref eq))
              (struct.new $vector (i32.const 0)
                          (struct.get $hash-table $buckets
                                      (local.get $table))))
       table))
    (define nbuckets (vector-length buckets))
    (define buckets* (make-vector nbuckets '()))
    (let lp ((i 0) (size 0))
      (cond
       ((< i nbuckets)
        (call-with-values (lambda () (copy-alist (vector-ref buckets i)))
          (lambda (bucket len)
            (vector-set! buckets* i bucket)
            (lp (1+ i) (+ size len)))))
       (else
        (%inline-wasm
         '(func (param $buckets (ref $vector))
                (param $size i32)
                (result (ref eq))
                (struct.new
                 $hash-table
                 (i32.const 0)
                 (local.get $size)
                 (struct.get $vector $vals (local.get $buckets))))
         buckets* size)))))

  (define (current-dynamic-state)
    (define current-fluids
      (%inline-wasm
       '(func (result (ref eq)) (global.get $current-fluids))))
    (%inline-wasm
     '(func (param $fluids (ref $hash-table))
            (result (ref eq))
            (struct.new $dynamic-state (i32.const 0) (local.get $fluids)))
     (copy-hash-table current-fluids)))

  (define (dynamic-state? x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (if (ref eq)
                (ref.test $dynamic-state (local.get $x))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     x))

  (define (with-dynamic-state state thunk)
    (check-type state dynamic-state? 'with-dynamic-state)
    (%with-dynamic-state state thunk)))
