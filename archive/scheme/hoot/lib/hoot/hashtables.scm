;;; Hoot hashtables
;;; Copyright (C) 2023, 2024 David Thompson <dave@spritely.institute>
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
;;; R6RS-inspired hashtables.
;;;
;;; Code:

(library (hoot hashtables)
  (export hashq
          hashv
          hash

          make-hashtable
          make-eq-hashtable
          make-eqv-hashtable
          hashtable?
          hashtable-hash
          hashtable-equiv
          hashtable-size
          hashtable-ref
          hashtable-set!
          hashtable-delete!
          hashtable-clear!
          hashtable-contains?
          hashtable-copy
          hashtable-keys
          hashtable-values
          hashtable-for-each
          hashtable-fold

          make-weak-key-hashtable
          make-eq-weak-key-hashtable
          make-eqv-weak-key-hashtable
          weak-key-hashtable?
          weak-key-hashtable-hash
          weak-key-hashtable-equiv
          weak-key-hashtable-size
          weak-key-hashtable-ref
          weak-key-hashtable-set!
          weak-key-hashtable-delete!
          weak-key-hashtable-clear!
          weak-key-hashtable-contains?
          weak-key-hashtable-copy
          weak-key-hashtable-keys
          weak-key-hashtable-values
          weak-key-hashtable-for-each
          weak-key-hashtable-fold

          make-weak-value-hashtable
          make-eq-weak-value-hashtable
          make-eqv-weak-value-hashtable
          weak-value-hashtable?
          weak-value-hashtable-hash
          weak-value-hashtable-equiv
          weak-value-hashtable-size
          weak-value-hashtable-ref
          weak-value-hashtable-set!
          weak-value-hashtable-delete!
          weak-value-hashtable-clear!
          weak-value-hashtable-contains?
          weak-value-hashtable-copy
          weak-value-hashtable-keys
          weak-value-hashtable-values
          weak-value-hashtable-for-each
          weak-value-hashtable-fold

          make-doubly-weak-hashtable
          make-eq-doubly-weak-hashtable
          make-eqv-doubly-weak-hashtable
          doubly-weak-hashtable?
          doubly-weak-hashtable-hash
          doubly-weak-hashtable-equiv
          doubly-weak-hashtable-size
          doubly-weak-hashtable-ref
          doubly-weak-hashtable-set!
          doubly-weak-hashtable-delete!
          doubly-weak-hashtable-clear!
          doubly-weak-hashtable-contains?
          doubly-weak-hashtable-copy
          doubly-weak-hashtable-keys
          doubly-weak-hashtable-values
          doubly-weak-hashtable-for-each
          doubly-weak-hashtable-fold)
  (import (only (hoot primitives)
                %struct-ref %struct-vtable
                guile:hashq guile:hashv guile:hash)
          (hoot bitwise)
          (hoot bitvectors)
          (hoot bytevectors)
          (hoot cond-expand)
          (hoot eq)
          (hoot equal)
          (hoot errors)
          (hoot ffi)
          (hoot finalization)
          (hoot inline-wasm)
          (hoot lists)
          (hoot match)
          (hoot pairs)
          (hoot procedures)
          (hoot not)
          (hoot numbers)
          (hoot records)
          (hoot strings)
          (hoot symbols)
          (hoot syntax)
          (hoot values)
          (hoot vectors)
          (hoot write)
          (hoot weak-refs))

  (cond-expand
   (guile-vm
    (define (hashq key size) (guile:hashq key size))
    (define (hashv key size) (guile:hashv key size))
    (define (hash key size) (guile:hash key size)))
   (hoot
    (define (string-hash str)
      (%inline-wasm
       '(func (param $str (ref eq)) (result i64)
              (i64.extend_i32_u
               (call $string-hash
                     (struct.get $string $str
                                 (ref.cast $string (local.get $str))))))
       str))

    (define (%hashq key)
      (%inline-wasm
       '(func (param $key (ref eq)) (result i64)
              (i64.extend_i32_u
               (call $hashq (local.get $key))))
       key))

    (define (%hashv key)
      (if (number? key)
          ;; Use hashq for integers, otherwise convert to a string and
          ;; hash that.
          (if (integer? key)
              (if (exact? key)
                  (%hashq key)
                  (%hashq (exact key)))
              (string-hash (number->string key)))
          (%hashq key)))

    (define (%hash key)
      (define-syntax let-values
        (syntax-rules ()
          ((_ () body ...)
           (begin body ...))
          ((_ (((vars ...) exp) rest ...) body ...)
           (call-with-values (lambda () exp)
             (lambda (vars ...)
               (let-values (rest ...)
                 body ...))))))
      ;; Simple, non-commutative hash code combiner.
      (define (combine-hashes h1 h2)
        (logxor (ash h1 5) h2))
      (define (half k)
        (ash (1+ k) -1))
      ;; For hashing records:
      (define (assq-ref alist k)
        (and (pair? alist)
             (if (eq? (caar alist) k)
                 (cdar alist)
                 (assq-ref (cdr alist) k))))
      (define (record-nfields record)
        (%struct-ref (%struct-vtable record) 0))
      (define (record-properties record)
        (%struct-ref (%struct-vtable record) 4))
      (define (record-opaque? record)
        (assq-ref (record-properties record) 'opaque))
      ;; For hashing bit/bytevectors
      (define (hash-bytevector bv)
        (%inline-wasm
         '(func (param $bv (ref eq)) (result i64)
                (i64.extend_i32_u
                 (call $hash-bytevector
                       (ref.cast $bytevector (local.get $bv)))))
         bv))
      (define (hash-bitvector bv)
        (%inline-wasm
         '(func (param $bv (ref eq)) (result i64)
                (i64.extend_i32_u
                 (call $hash-bitvector
                       (ref.cast $bitvector (local.get $bv)))))
         bv))
      ;; This recursive hashing algorithm with effort limit is inspired
      ;; by Chez Scheme.
      (define (hash key h k)
        (let ((k (1- k)))
          (cond
           ((<= k 0)                    ; out of hash juice :(
            (values h 0))
           ((pair? key)
            (let ((k/2 (half k)))
              (let-values (((h k*) (hash (car key) h k/2)))
                (hash (cdr key) h (+ (- k k/2) k*)))))
           ((vector? key)
            (let ((seed #x594fa6d))
              (let lp ((i 0) (h (combine-hashes h seed)) (k k))
                (if (and (< i (vector-length key)) (> k 0))
                    (let ((k/2 (half k)))
                      (let-values (((h k*) (hash (vector-ref key i) h k/2)))
                        (lp (1+ i) h (+ (- k k/2) k*))))
                    (values h k)))))
           ((string? key)
            (values (combine-hashes h (string-hash key)) k))
           ((bytevector? key)
            (values (combine-hashes h (hash-bytevector key)) k))
           ((bitvector? key)
            (values (combine-hashes h (hash-bitvector key)) k))
           ((record? key)
            (if (record-opaque? key)
                (values (%hashq key) k)
                (let ((seed #x1dd32f33)
                      (nfields (record-nfields key)))
                  (let lp ((i 0) (h (combine-hashes h seed)) (k k))
                    (if (and (< i nfields) (> k 0))
                        (let ((k/2 (half k)))
                          (let-values (((h k*) (hash (%struct-ref key i) h k/2)))
                            (lp (1+ i) h (+ (- k k/2) k*))))
                        (values h k))))))
           (else
            (values (combine-hashes h (%hashv key)) k)))))
      (let-values (((h k) (hash key #x12aec5bd 64)))
        h))

    (define max-hash-size (1- (ash 1 32)))

    (define (hashq key size)
      (check-size size max-hash-size 'hashq)
      (modulo (%hashq key) size))

    (define (hashv key size)
      (check-size size max-hash-size 'hashv)
      (modulo (%hashv key) size))

    (define (hash key size)
      (check-size size max-hash-size 'hash)
      (modulo (%hash key) size))))

  ;; Numbers taken from https://planetmath.org/goodhashtableprimes
  (define %bucket-sizes
    #(53 97 193 389 769 1543 3079 6151 12289 24593 98317 196613 393241 786433
         1572869 3145739 6291469 12582917 25165843 50331653 100663319
         201326611 402653189 805306457 1610612741))

  (define %min-buckets 53)

  (define (lower-bound k)
    (quotient k 8))

  (define (upper-bound k)
    (quotient (* k 9) 10))

  (define (optimal-buckets k)
    (let ((last (- (vector-length %bucket-sizes) 1)))
      (let lp ((idx 0))
        (if (= idx last)
            (vector-ref %bucket-sizes last)
            (let ((size (vector-ref %bucket-sizes idx)))
              (if (> k (upper-bound size))
                  (lp (1+ idx))
                  size))))))

  (define-syntax define-hashtable-impl
    (syntax-rules ()
      ((_ <hashtable>
          %make-hashtable make-hashtable
          make-eq-hashtable make-eqv-hashtable
          hashtable?
          hashtable-hash
          hashtable-equiv
          hashtable-size set-hashtable-size!
          hashtable-buckets set-hashtable-buckets!
          hashtable-lower set-hashtable-lower!
          hashtable-upper set-hashtable-upper!
          hashtable-ref hashtable-contains?
          hashtable-set! hashtable-delete! hashtable-clear!
          hashtable-resize-maybe!
          hashtable-copy hashtable-keys hashtable-values
          hashtable-for-each hashtable-fold
          ((extra-field . extra-field-accessor) ...)
          constructor
          key-box key-unbox key-empty?
          val-box val-unbox val-empty?
          on-add on-delete on-replace)
       (begin
         (define-record-type <hashtable>
           ;; Strip the <> characters from the name when printing.
           #:printer (let* ((name (symbol->string '<hashtable>))
                            (name* (substring name 1 (1- (string-length name)))))
                       (lambda (table port)
                         (display "#<" port)
                         (display name* port)
                         (display " size: " port)
                         (display (hashtable-size table) port)
                         (display ">" port)))
           (%make-hashtable hash equiv size buckets lower upper extra-field ...)
           hashtable?
           (hash hashtable-hash)
           (equiv hashtable-equiv)
           (size hashtable-size set-hashtable-size!)
           (buckets hashtable-buckets set-hashtable-buckets!)
           (lower hashtable-lower set-hashtable-lower!)
           (upper hashtable-upper set-hashtable-upper!)
           (extra-field . extra-field-accessor) ...)
         (define* (make-hashtable #:optional (hash hash) (equiv equal?))
           "Return a new, empty hashtable that uses the hash procedure @var{hash}
and equivalence procedure @var{equiv}."
           (constructor hash equiv 0 (make-vector %min-buckets '())
                        0 (upper-bound %min-buckets)))
         (define (make-eq-hashtable)
           "Return a new, empty hashtable that uses @code{eq?} as the equivalence
function and hashes keys accordingly."
           (make-hashtable hashq eq?))
         (define (make-eqv-hashtable)
           "Return a new, empty hashtable that uses @code{eqv?} as the equivalence
function and hashes keys accordingly."
           (make-hashtable hashv eqv?))
         (define* (hashtable-ref table key #:optional default)
           "Return the value associated with @var{key} in @var{table}, or
@var{default} if there is no such association."
           (let ((hash (hashtable-hash table))
                 (equiv? (hashtable-equiv table))
                 (buckets (hashtable-buckets table)))
             (let lp ((chain (vector-ref buckets (hash key (vector-length buckets)))))
               (match chain
                 (() default)
                 (((other-key . val) . rest)
                  (cond
                   ;; Skip dead pairs in weak tables.
                   ((or (key-empty? other-key) (val-empty? val))
                    (lp rest))
                   ((equiv? key (key-unbox other-key))
                    (val-unbox val))
                   (else (lp rest))))))))
         (define (hashtable-resize! table k)
           (let ((old (hashtable-buckets table))
                 (new (make-vector k '()))
                 (hash (hashtable-hash table)))
             (set-hashtable-lower! table (if (eq? k %min-buckets) 0 (lower-bound k)))
             (set-hashtable-upper! table (upper-bound k))
             (set-hashtable-buckets! table new)
             ;; Rehash all key/value pairs.
             (do ((idx 0 (1+ idx)))
                 ((= idx (vector-length old)))
               (let lp ((chain (vector-ref old idx)))
                 (match chain
                   (() (values))
                   (((and link (key . val)) . rest)
                    (cond
                     ;; Prune dead pairs in weak tables.
                     ((or (key-empty? key) (val-empty? val))
                      (on-delete table key val)
                      (lp rest))
                     (else
                      (let ((new-idx (hash (key-unbox key) k)))
                        ;; For weak tables, we unregister the old
                        ;; bucket index from the finalization registry
                        ;; then register the new one.
                        (on-delete table key val)
                        (on-add table key val new-idx)
                        (vector-set! new new-idx (cons link (vector-ref new new-idx)))
                        (lp rest))))))))))
         (define (hashtable-resize-maybe! table)
           (let ((size (hashtable-size table))
                 (lower (hashtable-lower table))
                 (upper (hashtable-upper table)))
             (when (or (< size lower) (> size upper))
               (hashtable-resize! table (optimal-buckets size)))))
         (define (hashtable-set! table key val)
           "Associate @{val} with @var{key} in @var{table}, potentially
overwriting any previous association with @var{key}."
           (let* ((hash (hashtable-hash table))
                  (equiv? (hashtable-equiv table))
                  (buckets (hashtable-buckets table))
                  (idx (hash key (vector-length buckets))))
             (define (increment-size!)
               (set-hashtable-size! table (1+ (hashtable-size table))))
             (define (decrement-size!)
               (set-hashtable-size! table (1- (hashtable-size table))))
             (vector-set! buckets idx
                          (let lp ((chain (vector-ref buckets idx)))
                            (match chain
                              (()
                               (let ((key* (key-box key))
                                     (val* (val-box val)))
                                 (on-add table key* val* idx)
                                 (increment-size!)
                                 (list (cons key* val*))))
                              (((and link (other-key . other-val)) . rest)
                               (cond
                                ;; Prune dead pairs in weak tables.
                                ((or (key-empty? other-key) (val-empty? other-val))
                                 (decrement-size!)
                                 (on-delete table other-key other-val)
                                 (lp rest))
                                ((equiv? key (key-unbox other-key))
                                 (let ((val* (val-box val)))
                                   (on-replace table other-key val* other-val idx)
                                   (set-cdr! link val*)
                                   chain))
                                (else (cons link (lp rest))))))))
             (hashtable-resize-maybe! table)
             (values)))
         (define (hashtable-delete! table key)
           "Remove the association with @var{key} in @var{table}, if one exists."
           (let* ((hash (hashtable-hash table))
                  (equiv? (hashtable-equiv table))
                  (buckets (hashtable-buckets table))
                  (idx (hash key (vector-length buckets))))
             (define (decrement-size!)
               (set-hashtable-size! table (1- (hashtable-size table))))
             (vector-set! buckets idx
                          (let lp ((chain (vector-ref buckets idx)))
                            (match chain
                              (() '())
                              (((and link (other-key . val)) . rest)
                               (cond
                                ;; Prune dead pairs in weak tables.
                                ((or (key-empty? other-key) (val-empty? val))
                                 (on-delete table other-key val)
                                 (decrement-size!)
                                 (lp rest))
                                ((equiv? key (key-unbox other-key))
                                 (on-delete table other-key val)
                                 (decrement-size!)
                                 rest)
                                (else (cons link (lp rest))))))))
             (hashtable-resize-maybe! table)
             (values)))
         (define* (hashtable-clear! table)
           "Remove all items from @var{table}."
           (let ((buckets (hashtable-buckets table)))
             (do ((idx 0 (1+ idx)))
                 ((= idx (vector-length buckets)))
               (let lp ((chain (vector-ref buckets idx)))
                 (match chain
                   (() (values))
                   (((key . val) . rest)
                    (on-delete table key val)
                    (lp rest)))))
             (vector-fill! buckets '())
             (set-hashtable-size! table 0)
             (values)))
         (define (hashtable-contains? table key)
           "Return #t if @var{key} has an associated value in @var{table}."
           (let ((hash (hashtable-hash table))
                 (equiv? (hashtable-equiv table))
                 (buckets (hashtable-buckets table)))
             (let lp ((chain (vector-ref buckets (hash key (vector-length buckets)))))
               (match chain
                 (() #f)
                 (((other-key . val) . rest)
                  (cond
                   ;; Skip dead pairs in weak tables.
                   ((or (key-empty? other-key) (val-empty? val))
                    (lp rest))
                   ((equiv? key (key-unbox other-key)) #t)
                   (else (lp rest))))))))
         (define* (hashtable-copy table)
           "Return a copy of @var{table}."
           (let* ((buckets (hashtable-buckets table))
                  (k (vector-length buckets))
                  (buckets* (make-vector k))
                  (table* (constructor (hashtable-hash table)
                                       (hashtable-equiv table)
                                       (hashtable-size table)
                                       buckets*
                                       (hashtable-lower table)
                                       (hashtable-upper table))))
             (define (decrement-size!)
               (set-hashtable-size! table* (1- (hashtable-size table*))))
             (do ((i 0 (1+ i)))
                 ((= i k))
               (vector-set! buckets* i
                            (let lp ((chain (vector-ref buckets i)))
                              (match chain
                                (() '())
                                (((key . val) . rest)
                                 (cond
                                  ;; Skip dead pairs in weak tables.
                                  ((or (key-empty? key) (val-empty? val))
                                   (decrement-size!)
                                   (lp rest))
                                  (else
                                   (on-add table key val i)
                                   (cons (cons key val) (lp rest)))))))))
             (hashtable-resize-maybe! table*)
             table*))
         (define (hashtable-keys table)
           "Return a list of keys in @var{table}."
           (hashtable-fold (lambda (key val result)
                             (cons key result))
                           '() table))
         (define (hashtable-values table)
           "Return a list of values in @var{table}."
           (hashtable-fold (lambda (key val result)
                             (cons val result))
                           '() table))
         (define (hashtable-for-each proc table)
           "Apply @var{proc} to each key/value association in @var{table}.
Each call is of the form @code{(proc key value)}."
           (let ((buckets (hashtable-buckets table)))
             (do ((idx 0 (1+ idx)))
                 ((= idx (vector-length buckets)))
               (let lp ((chain (vector-ref buckets idx)))
                 (match chain
                   (() (values))
                   (((key . val) . rest)
                    ;; Skip dead pairs in weak tables.
                    (unless (or (key-empty? key) (val-empty? val))
                      (proc (key-unbox key) (val-unbox val)))
                    (lp rest)))))))
         (define (hashtable-fold proc init table)
           "Accumulate a result by applying @var{proc} with each key/value
association in @var{table} and the result of the previous @var{proc}
call.  Each call is of the form @code{(proc key value prev)}.  For the
first call, @code{prev} is the initial value @var{init}."
           (let ((buckets (hashtable-buckets table)))
             (let bucket-lp ((idx 0) (result init))
               (if (< idx (vector-length buckets))
                   (bucket-lp (1+ idx)
                              (let chain-lp ((chain (vector-ref buckets idx))
                                             (result result))
                                (match chain
                                  (() result)
                                  (((key . val) . rest)
                                   ;; Skip dead pairs in weak tables.
                                   (if (or (key-empty? key) (val-empty? val))
                                       (chain-lp rest result)
                                       (let ((k (key-unbox key))
                                             (v (val-unbox val)))
                                         (chain-lp rest (proc k v result))))))))
                   result))))))))

  (define-hashtable-impl <hashtable>
    %make-hashtable make-hashtable
    make-eq-hashtable make-eqv-hashtable
    hashtable?
    hashtable-hash
    hashtable-equiv
    hashtable-size set-hashtable-size!
    hashtable-buckets set-hashtable-buckets!
    hashtable-lower set-hashtable-lower!
    hashtable-upper set-hashtable-upper!
    hashtable-ref hashtable-contains?
    hashtable-set! hashtable-delete! hashtable-clear!
    hashtable-resize-maybe!
    hashtable-copy hashtable-keys hashtable-values
    hashtable-for-each hashtable-fold
    () ; no extra fields
    %make-hashtable
    ;; Keys and values are unboxed.
    (lambda (k) k) (lambda (k) k) (lambda (k) #f)
    (lambda (v) v) (lambda (v) v) (lambda (v) #f)
    ;; on-add, on-delete, on-replace are all no-ops.
    (lambda (table k v i) (values))
    (lambda (table k v) (values))
    (lambda (table k v v* i) (values)))

  ;; Weak refs cannot store immediates, so here are some wrappers that
  ;; will allow immediates to be stored in weak tables anyway.
  (define (immediate? x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (if (ref eq)
                (ref.test i31 (local.get $x))
                (then (ref.i31 (i32.const 17)))
                (else (ref.i31 (i32.const 1)))))
     x))
  (define (make-weak-ref* x)
    (if (immediate? x) x (make-weak-ref x)))
  (define (weak-ref-deref* x)
    (if (weak-ref? x) (weak-ref-deref x) x))
  (define (weak-ref-empty? x)
    (if (weak-ref? x) (weak-ref-null? (weak-ref-deref x)) #f))
  (define (maybe-register! registry ref held-value)
    (when (weak-ref? ref)
      (finalization-registry-register! registry (weak-ref-deref ref)
                                       held-value ref)))
  (define (maybe-unregister! registry ref)
    (when (weak-ref? ref)
      (finalization-registry-unregister! registry ref)))
  (define-syntax-rule (weak-hashtable-constructor %make-hashtable
                                                  hashtable-buckets
                                                  hashtable-size set-hashtable-size!
                                                  hashtable-resize-maybe!
                                                  dead?)
    (lambda (hash equiv size buckets min max)
      ;; When we are notified of a key/value being GC'd, we clean up
      ;; all empty weak refs in the associated bucket.
      (define (cleanup idx)
        (let ((buckets (hashtable-buckets table)))
          (define (decrement-size!)
            (set-hashtable-size! table (1- (hashtable-size table))))
          ;; Try as we might to unregister old values when we resize
          ;; the table, it is possible for a bucket index that is no
          ;; longer valid to sneak through.  We just have to ignore
          ;; those.
          (when (< idx (vector-length buckets))
            (vector-set! buckets idx
                         (let lp ((chain (vector-ref buckets idx)))
                           (match chain
                             (() '())
                             (((and link (key . val)) . rest)
                              (cond
                               ((dead? table key val)
                                (decrement-size!)
                                (lp rest))
                               (else
                                (cons link (lp rest)))))))))
          (hashtable-resize-maybe! table)))
      (define registry (make-finalization-registry cleanup))
      (define table
        (%make-hashtable hash equiv size buckets min max registry))
      table))

  (define-hashtable-impl <weak-key-hashtable>
    %make-weak-key-hashtable make-weak-key-hashtable
    make-eq-weak-key-hashtable make-eqv-weak-key-hashtable
    weak-key-hashtable?
    weak-key-hashtable-hash
    weak-key-hashtable-equiv
    weak-key-hashtable-size set-weak-key-hashtable-size!
    weak-key-hashtable-buckets set-weak-key-hashtable-buckets!
    weak-key-hashtable-lower set-weak-key-hashtable-lower!
    weak-key-hashtable-upper set-weak-key-hashtable-upper!
    weak-key-hashtable-ref weak-key-hashtable-contains?
    weak-key-hashtable-set! weak-key-hashtable-delete! weak-key-hashtable-clear!
    weak-key-hashtable-resize-maybe!
    weak-key-hashtable-copy weak-key-hashtable-keys weak-key-hashtable-values
    weak-key-hashtable-for-each weak-key-hashtable-fold
    ;; Extra field:
    ((registry weak-key-hashtable-registry))
    ;; Constructor:
    (weak-hashtable-constructor %make-weak-key-hashtable
                                weak-key-hashtable-buckets
                                weak-key-hashtable-size set-weak-key-hashtable-size!
                                weak-key-hashtable-resize-maybe!
                                (lambda (table key val) (weak-ref-empty? key)))
    ;; Keys are boxed in weak refs.
    make-weak-ref* weak-ref-deref* weak-ref-empty?
    ;; Values are unboxed.
    (lambda (v) v) (lambda (v) v) (lambda (v) #f)
    ;; Add:
    (lambda (table key val idx)
      (maybe-register! (weak-key-hashtable-registry table) key idx))
    ;; Remove:
    (lambda (table key val)
      (maybe-unregister! (weak-key-hashtable-registry table) key))
    ;; Replace is a no-op.
    (lambda (table key val old-val idx) (values)))

  (define-hashtable-impl <weak-value-hashtable>
    %make-weak-value-hashtable make-weak-value-hashtable
    make-eq-weak-value-hashtable make-eqv-weak-value-hashtable
    weak-value-hashtable?
    weak-value-hashtable-hash
    weak-value-hashtable-equiv
    weak-value-hashtable-size set-weak-value-hashtable-size!
    weak-value-hashtable-buckets set-weak-value-hashtable-buckets!
    weak-value-hashtable-lower set-weak-value-hashtable-lower!
    weak-value-hashtable-upper set-weak-value-hashtable-upper!
    weak-value-hashtable-ref weak-value-hashtable-contains?
    weak-value-hashtable-set! weak-value-hashtable-delete! weak-value-hashtable-clear!
    weak-value-hashtable-resize-maybe!
    weak-value-hashtable-copy weak-value-hashtable-keys weak-value-hashtable-values
    weak-value-hashtable-for-each weak-value-hashtable-fold
    ;; Extra field:
    ((registry weak-value-hashtable-registry))
    ;; Constructor:
    (weak-hashtable-constructor %make-weak-value-hashtable
                                weak-value-hashtable-buckets
                                weak-value-hashtable-size set-weak-value-hashtable-size!
                                weak-value-hashtable-resize-maybe!
                                (lambda (table key val) (weak-ref-empty? val)))
    ;; Keys are unboxed.
    (lambda (v) v) (lambda (v) v) (lambda (v) #f)
    ;; Values are boxed in weak refs.
    make-weak-ref* weak-ref-deref* weak-ref-empty?
    ;; Add:
    (lambda (table key val idx)
      (maybe-register! (weak-value-hashtable-registry table) val idx))
    ;; Remove:
    (lambda (table key val)
      (maybe-unregister! (weak-value-hashtable-registry table) val))
    ;; Replace:
    (lambda (table key val old-val idx)
      (maybe-unregister! (weak-value-hashtable-registry table) old-val)
      (maybe-register! (weak-value-hashtable-registry table) val idx)))

  (define-hashtable-impl <doubly-weak-hashtable>
    %make-doubly-weak-hashtable make-doubly-weak-hashtable
    make-eq-doubly-weak-hashtable make-eqv-doubly-weak-hashtable
    doubly-weak-hashtable?
    doubly-weak-hashtable-hash
    doubly-weak-hashtable-equiv
    doubly-weak-hashtable-size set-doubly-weak-hashtable-size!
    doubly-weak-hashtable-buckets set-doubly-weak-hashtable-buckets!
    doubly-weak-hashtable-lower set-doubly-weak-hashtable-lower!
    doubly-weak-hashtable-upper set-doubly-weak-hashtable-upper!
    doubly-weak-hashtable-ref doubly-weak-hashtable-contains?
    doubly-weak-hashtable-set! doubly-weak-hashtable-delete! doubly-weak-hashtable-clear!
    doubly-weak-hashtable-resize-maybe!
    doubly-weak-hashtable-copy doubly-weak-hashtable-keys doubly-weak-hashtable-values
    doubly-weak-hashtable-for-each doubly-weak-hashtable-fold
    ;; Extra field:
    ((registry doubly-weak-hashtable-registry))
    ;; Constructor:
    (weak-hashtable-constructor %make-doubly-weak-hashtable
                                doubly-weak-hashtable-buckets
                                doubly-weak-hashtable-size set-doubly-weak-hashtable-size!
                                doubly-weak-hashtable-resize-maybe!
                                (lambda (table key val)
                                  (let ((registry (doubly-weak-hashtable-registry table)))
                                    (cond
                                     ((weak-ref-empty? key)
                                      (maybe-unregister! registry val)
                                      #t)
                                     ((weak-ref-empty? val)
                                      (maybe-unregister! registry key)
                                      #t)
                                     (else #f)))))
    ;; Both keys and values are boxed in weak refs.
    make-weak-ref* weak-ref-deref* weak-ref-empty?
    make-weak-ref* weak-ref-deref* weak-ref-empty?
    ;; Add:
    (lambda (table key val idx)
      (maybe-register! (doubly-weak-hashtable-registry table) key idx)
      (maybe-register! (doubly-weak-hashtable-registry table) val idx))
    ;; Remove:
    (lambda (table key val)
      (maybe-unregister! (doubly-weak-hashtable-registry table) key)
      (maybe-unregister! (doubly-weak-hashtable-registry table) val))
    ;; Replace:
    (lambda (table key val old-val idx)
      (maybe-unregister! (doubly-weak-hashtable-registry table) old-val)
      (maybe-register! (doubly-weak-hashtable-registry table) val idx))))
