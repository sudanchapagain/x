;;; Vectors
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
;;; Vectors.
;;;
;;; Code:

(library (hoot vectors)
  (export vector
          make-vector
          vector?
          vector-length
          vector-ref
          vector-set!
          vector-copy
          vector-copy!
          vector-fill!
          vector->list
          list->vector
          vector-concatenate
          vector-append
          vector-for-each
          vector-map
          vector-sort!
          vector-binary-search
          vector-move-left!)
  (import (only (hoot primitives)
                %vector? %make-vector %vector
                %vector-length %vector-ref %vector-set!)
          (hoot apply)
          (hoot errors)
          (hoot inline-wasm)
          (hoot lists)
          (hoot match)
          (hoot numbers)
          (hoot pairs)
          (hoot syntax)
          (hoot values))

  (define (%generic-vector . args) (list->vector args))

  (define-syntax vector
    (lambda (stx)
      (syntax-case stx ()
        ((_ . x) #'(%vector . x))
        (f (identifier? #'f) #'%generic-vector))))

  (define* (make-vector n #:optional init) (%make-vector n init))

  (define (vector? x) (%vector? x))

  (define (vector-length x) (%vector-length x))

  (define (vector-ref x i) (%vector-ref x i))

  (define (vector-set! x i v) (%vector-set! x i v))

  (define* (vector-copy v #:optional (start 0) (end (vector-length v)))
    (check-type v vector? 'vector-copy)
    (check-range start 0 (vector-length v) 'vector-copy)
    (check-range end start (vector-length v) 'vector-copy)
    (%inline-wasm
     '(func (param $src (ref $vector)) (param $start i32) (param $end i32)
            (result (ref eq))
            (local $i0 i32)
            (local $vec0 (ref $raw-scmvector))
            (local.set $i0 (i32.sub (local.get $end)
                                    (local.get $start)))
            (local.set $vec0 (array.new $raw-scmvector (ref.i31 (i32.const 0))
                                      (local.get $i0)))
            (array.copy $raw-scmvector $raw-scmvector
                        (local.get $vec0) (i32.const 0)
                        (struct.get $vector $vals (local.get $src))
                        (local.get $start) (local.get $i0))
            (struct.new $mutable-vector (i32.const 0) (local.get $vec0)))
     v start end))

  (define* (vector-copy! to at from #:optional (start 0) (end (vector-length from)))
    (check-type to vector? 'vector-copy!)
    (check-range at 0 (vector-length to) 'vector-copy!)
    (check-type from vector? 'vector-copy!)
    (check-range start 0 (vector-length from) 'vector-copy!)
    (check-range end start (vector-length from) 'vector-copy!)
    (%inline-wasm
     '(func (param $to (ref $mutable-vector)) (param $at i32)
            (param $from (ref $vector)) (param $start i32) (param $end i32)
            (array.copy $raw-scmvector $raw-scmvector
                        (struct.get $mutable-vector $vals (local.get $to))
                        (local.get $at)
                        (struct.get $vector $vals (local.get $from))
                        (local.get $start)
                        (i32.sub (local.get $end) (local.get $start))))
     to at from start end))

  (define* (vector-fill! v fill #:optional (start 0) (end (vector-length v)))
    ;; FIXME: check for mutability
    (check-type v vector? 'vector-fill!)
    (check-range start 0 (vector-length v) 'vector-fill!)
    (check-range end start (vector-length v) 'vector-fill!)
    (%inline-wasm
     '(func (param $dst (ref $mutable-vector)) (param $fill (ref eq))
            (param $start i32) (param $end i32)
            (array.fill $raw-scmvector
                        (struct.get $mutable-vector $vals (local.get $dst))
                        (local.get $start)
                        (local.get $fill)
                        (i32.sub (local.get $end) (local.get $start))))
     v fill start end))

  (define* (vector->list v #:optional (start 0) (end (vector-length v)))
    (let lp ((i start))
      (if (< i end)
          (cons (vector-ref v i) (lp (1+ i)))
          '())))

  (define (list->vector elts)
    (define (length l)
      (let lp ((len 0) (l l))
        (if (null? l) len (lp (1+ len) (cdr l)))))
    (let* ((len (length elts))
           (v (make-vector len #f)))
      (let lp ((i 0) (elts elts))
        (match elts
          (() v)
          ((elt . elts)
           (vector-set! v i elt)
           (lp (1+ i) elts))))))

  (define (vector-concatenate v*)
    (match v*
      (() #())
      ((v) v)
      (v*
       (let* ((len (fold (lambda (v len) (+ (vector-length v) len)) 0 v*))
              (flattened (make-vector len 0)))
         (let lp ((v* v*) (cur 0))
           (match v*
             (() flattened)
             ((v . v*)
              (vector-copy! flattened cur v)
              (lp v* (+ cur (vector-length v))))))))))

  (define (vector-append . vectors)
    (vector-concatenate vectors))

  (define (vector-for-each f v . v*)
    (apply for-each f (vector->list v) (map vector->list v*)))

  (define (vector-map f v . v*)
    (list->vector (apply map f (vector->list v) (map vector->list v*))))

  (define* (vector-sort! v less? #:optional (start 0) (end (vector-length v)))
    (define (partition start end)
      ;; TODO: Using last element as pivot for simplicity.  Choose a
      ;; different pivot to avoid worst-case scenarios for sorted or
      ;; nearly sorted vectors.
      (let* ((pivot-idx (1- end))
             (pivot (vector-ref v pivot-idx)))
        (let lp ((i start) (j start))
          (if (< j pivot-idx)
              (let ((item (vector-ref v j)))
                (if (less? pivot item)
                    (lp i (1+ j))
                    (let ((other (vector-ref v i)))
                      (vector-set! v j other)
                      (vector-set! v i item)
                      (lp (1+ i) (1+ j)))))
              (let ((other (vector-ref v i)))
                (vector-set! v i pivot)
                (vector-set! v pivot-idx other)
                i)))))
    (define (quicksort start end)
      (when (< start end)
        (let ((i (partition start end)))
          (quicksort start i)
          (quicksort (1+ i) end))))
    (check-size end (vector-length v) 'vector-sort!)
    (check-size start (1- end) 'vector-sort!)
    (quicksort start end)
    (values))

  (define* (vector-binary-search v x compare #:optional (start 0) (end (vector-length v)))
    (and (< start end)
         (let* ((i (+ start (quotient (- end start) 2)))
                (diff (compare (vector-ref v i) x)))
           (cond
            ((zero? diff) i)
            ((positive? diff) (vector-binary-search v x compare start i))
            (else (vector-binary-search v x compare (1+ i) end))))))

  (define (vector-move-left! v1 start1 end1 v2 start2)
    (check-range end1 0 (vector-length v1) 'vector-move-left!)
    (check-range start1 0 end1 'vector-move-left!)
    (let ((n (- end1 start1)))
      (check-range start2 0 (- (vector-length v2) n) 'vector-move-left!)
      (do ((i 0 (1+ i)))
          ((= i n))
        (vector-set! v2 (+ start2 i) (vector-ref v1 (+ start1 i)))))))
