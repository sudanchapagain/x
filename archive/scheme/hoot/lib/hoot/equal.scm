;;; Equal?
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
;;; Implementation of 'equal?' based on the interleaved union-find and
;;; tree equality with precheck algorithm from "Efficient
;;; Nondestructive Equality Checking for Trees and Graphs"
;;;
;;; See: https://cs.indiana.edu/~dyb/pubs/equal.pdf
;;;
;;; Code:

(library (hoot equal)
  (export equal?)
  (import (hoot bitvectors)
          (hoot bytevectors)
          (hoot boxes)
          (hoot eq)
          (hoot inline-wasm)
          (hoot match)
          (hoot numbers)
          (hoot not)
          (hoot pairs)
          (only (hoot primitives) %struct-ref %struct-vtable)
          (hoot records)
          (hoot strings)
          (hoot syntax)
          (hoot values)
          (hoot vectors))

  (define (equal? x y)
    ;; TODO: Add pseudorandom number generator
    (define (random x) x)
    ;; Use low-level wasm hashq tables to avoid a cycle with (hoot
    ;; hashtables).
    (define (make-eq-hashtable)
      (%inline-wasm
       '(func (result (ref eq))
              (call $make-hash-table))))
    (define (hashtable-ref table key)
      (%inline-wasm
       '(func (param $table (ref eq))
              (param $key (ref eq))
              (result (ref eq))
              (call $hashq-ref
                    (ref.cast $hash-table (local.get $table))
                    (local.get $key)
                    (ref.i31 (i32.const 1))))
       table key))
    (define (hashtable-set! table key value)
      (%inline-wasm
       '(func (param $table (ref eq))
              (param $key (ref eq))
              (param $value (ref eq))
              (call $hashq-set!
                    (ref.cast $hash-table (local.get $table))
                    (local.get $key)
                    (local.get $value)))
       table key value))
    (define (record-type-compare vtable)
      (%struct-ref vtable 7))
    (define (bytevector=? x y)
      (let ((n (bytevector-length x)))
        (and (= n (bytevector-length y))
             (let lp ((i 0))
               (or (= i n)
                   (and (eqv? (bytevector-u8-ref x i)
                              (bytevector-u8-ref y i))
                        (lp (+ i 1))))))))
    (define (bitvector=? x y)
      (let ((n (bitvector-length x)))
        (and (= n (bitvector-length y))
             (let lp ((i 0))
               (or (= i n)
                   (and (eqv? (bitvector-ref x i)
                              (bitvector-ref y i))
                        (lp (+ i 1))))))))
    ;; Bounds for precheck and fast/slow interleave paths.  These
    ;; magic numbers are taken straight out of the aforementioned
    ;; paper.
    (define k0 400)
    (define kb -40)
    ;; The precheck does a simple tree equality test with a bound on
    ;; the number of checks, recurring up to k times.  This means that
    ;; the precheck will terminate even when given cyclic inputs.
    (define (pre? x y k)
      (cond
       ((eq? x y) k)
       ((pair? x)
        (and (pair? y)
             (if (<= k 0)
                 k
                 (let ((k (pre? (car x) (car y) (- k 1))))
                   (and k (pre? (cdr x) (cdr y) k))))))
       ((vector? x)
        (and (vector? y)
             (let ((n (vector-length x)))
               (and (= n (vector-length y))
                    (let lp ((i 0) (k k))
                      (if (or (= i n) (<= k 0))
                          k
                          (let ((k (pre? (vector-ref x i) (vector-ref y i) (- k 1))))
                            (and k (lp (+ i 1) k)))))))))
       ((record? x)
        (and (record? y)
             (let ((vtable (%struct-vtable x)))
               (and (eq? vtable (%struct-vtable y))
                    (match (record-type-compare vtable)
                      (#f #f)
                      (compare
                       ;; Since the record type comparison procedure
                       ;; is external to 'equal?', we need to create a
                       ;; wrapper that updates the counter after each
                       ;; call.  Opaque records will never call
                       ;; 'equal?*', so 'k*' is lazily initialized to
                       ;; detect this case.
                       (let ((k* #f))
                         (define (equal?* x y)
                           (unless k* (set! k* k))
                           (and (> k* 0)
                                (match (pre? x y k*)
                                  (#f
                                   (set! k* #f)
                                   #f)
                                  (k
                                   (set! k* (- k 1))
                                   ;; The values were equal, but if
                                   ;; the precheck has reached its
                                   ;; bound we will lie and say the
                                   ;; values were not equal so
                                   ;; 'compare' will stop.
                                   (> k 0)))))
                         (compare x y equal?*)
                         k*)))))
             k))
       ((string? x)
        (and (string? y) (string=? x y) k))
       ((bytevector? x)
        (and (bytevector? y) (bytevector=? x y) k))
       ((bitvector? x)
        (and (bitvector? y) (bitvector=? x y) k))
       (else (and (eqv? x y) k))))
    (define (interleave? ht x y k)
      ;; Union-find algorithm with splitting path compression.
      (define (union-find x y)
        (define (find b)
          (let ((n (box-ref b)))
            (if (number? n)
                b
                ;; Equivalence classes form chains of boxes.  To
                ;; reduce pointer chasing as the set grows, the path
                ;; is compressed during lookup via the "splitting"
                ;; technique.  Each box in the chain becomes linked to
                ;; the one two beyond it.
                (let loop ((b b) (n n))
                  (let ((nn (box-ref n)))
                    (if (number? nn)
                        n
                        (begin
                          (box-set! b nn)
                          (loop n nn))))))))
        (let ((bx (hashtable-ref ht x))
              (by (hashtable-ref ht y)))
          (if (not bx)
              (if (not by)
                  ;; Neither value has been visited before.  Create a
                  ;; new equivalence class for them to share.
                  (let ((b (make-box 1)))
                    (hashtable-set! ht x b)
                    (hashtable-set! ht y b)
                    #f)
                  ;; x hasn't been visited before, but y has.  Use y's
                  ;; equivalence class.
                  (let ((ry (find by)))
                    (hashtable-set! ht x ry)
                    #f))
              (if (not by)
                  ;; y hasn't been visited before, but x has.  Use x's
                  ;; equivalence class.
                  (let ((rx (find bx)))
                    (hashtable-set! ht y rx)
                    #f)
                  ;; Both x and y have been visited before.
                  (let ((rx (find bx))
                        (ry (find by)))
                    ;; If x and y share an equivalance class then they
                    ;; are equal and we're done.  Otherwise, the
                    ;; representative of the smaller class is linked
                    ;; to the representative of the larger class and
                    ;; the size is updated to reflect the size of the
                    ;; new class.
                    (or (eq? rx ry)
                        (let ((nx (box-ref rx))
                              (ny (box-ref ry)))
                          (if (> nx ny)
                              (begin
                                (box-set! ry rx)
                                (box-set! rx (+ nx ny))
                                #f)
                              (begin
                                (box-set! rx ry)
                                (box-set! ry (+ ny nx))
                                #f)))))))))
      (define (e? x y k)
        (if (<= k 0)
            (if (= k kb)
                ;; The fast path is taken when k hits the lower bound,
                ;; resetting k in the process.  The random k value
                ;; "reduces the likelihood of repeatedly tripping on
                ;; worst-case behavior in cases where sizes of the
                ;; input graphs happen to be related to the chosen
                ;; bounds in a bad way."
                (fast? x y (random (* 2 k0)))
                (slow? x y k))
            (fast? x y k)))
      (define (slow? x y k)
        (cond
         ((eq? x y) k)
         ((pair? x)
          (and (pair? y)
               (if (union-find x y)
                   ;; Reset k back to zero to re-enter slow? on the
                   ;; basis that if one equivalence is found then it
                   ;; is likely that more will be found.
                   0
                   (let ((k (e? (car x) (car y) (- k 1))))
                     (and k (e? (cdr x) (cdr y) k))))))
         ((vector? x)
          (and (vector? y)
               (let ((length (vector-length x)))
                 (and (= length (vector-length y))
                      (if (union-find x y)
                          0
                          (let lp ((i 0) (k (- k 1)))
                            (if (= i length)
                                k
                                (let ((k (e? (vector-ref x i) (vector-ref y i) k)))
                                  (and k (lp (+ i 1) k))))))))))
         ((record? x)
          (and (record? y)
               (let ((vtable (%struct-vtable x)))
                 (and (eq? vtable (%struct-vtable y))
                      (match (record-type-compare vtable)
                        (#f #f)
                        (compare
                         (let ((k* #f))
                           (define (equal?* x y)
                             (unless k* (set! k* k))
                             (if (union-find x y)
                                 (begin
                                   (set! k* 0)
                                   #t)
                                 (match (e? x y k*)
                                   (#f
                                    (set! k* #f)
                                    #f)
                                   (k
                                    (set! k* (- k 1))
                                    (> k 0)))))
                           k*)))))))
         ((string? x)
          (and (string? y) (string=? x y) k))
         ((bytevector? x)
          (and (bytevector? y) (bytevector=? x y) k))
         ((bitvector? x)
          (and (bitvector? y) (bitvector=? x y) k))
         (else (and (eqv? x y) k))))
      (define (fast? x y k)
        (let ((k (- k 1)))
          (cond
           ((eq? x y) k)
           ((pair? x)
            (and (pair? y)
                 (let ((k (e? (car x) (car y) k)))
                   (and k (e? (cdr x) (cdr y) k)))))
           ((vector? x)
            (and (vector? y)
                 (let ((length (vector-length x)))
                   (and (= length (vector-length y))
                        (let lp ((i 0) (k k))
                          (if (= i length)
                              k
                              (let ((k (e? (vector-ref x i) (vector-ref y i) k)))
                                (and k (lp (+ i 1) k)))))))))
           ((record? x)
            (and (record? y)
                 (let ((vtable (%struct-vtable x)))
                   (and (eq? vtable (%struct-vtable y))
                        (match (record-type-compare vtable)
                          (#f #f)
                          (compare
                           (let ((k* #f))
                             (define (equal?* x y)
                               (unless k* (set! k* k))
                               (match (e? x y k*)
                                 (#f
                                  (set! k* #f)
                                  #f)
                                 (k
                                  (set! k* (- k 1))
                                  (> k 0))))
                             (and (compare x y equal?*) k))))))))
           ((string? x)
            (and (string? y) (string=? x y) k))
           ((bytevector? x)
            (and (bytevector? y) (bytevector=? x y) k))
           ((bitvector? x)
            (and (bitvector? y) (bitvector=? x y) k))
           (else (and (eqv? x y) k)))))
      (and (e? x y k) #t))
    ;; Perform the precheck before falling back to the slower
    ;; interleave method.  For atoms and small trees, the precheck
    ;; will be sufficient to determine equality.
    (let ((k (pre? x y k0)))
      ;; The precheck returns #f if not equal, a number greater than
      ;; zero if equal, or 0 if it couldn't determine equality within
      ;; k0 checks.  For the first two cases, we can return
      ;; immediately.  For the last case, we proceed to the
      ;; interleaved algorithm.
      (and k (or (> k 0) (interleave? (make-eq-hashtable) x y 0))))))
