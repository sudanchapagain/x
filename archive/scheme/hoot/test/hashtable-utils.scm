;;; Copyright (C) 2024, 2025 David Thompson <dave@spritely.institute>
;;; Copyright (C) 2023, 2024 Robin Templeton
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
;;; Hashtable tests.
;;;
;;; Code:

(define-module (test hashtable-utils)
  #:use-module (srfi srfi-64)
  #:use-module (test utils)
  #:export (test-hashtable-impl))

(define-syntax-rule (test-hashtable-impl make-hashtable
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
                                         hashtable-fold)
  (begin
    ;; Ref hit
    (test-call "b"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-ref ht 'a))))
    ;; Ref miss
    (test-call "#f"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'x 'y)
                   (hashtable-ref ht 'a))))
    ;; Ref miss with default
    (test-call "b"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'x 'y)
                   (hashtable-ref ht 'a 'b))))
    ;; Key insertion increases size
    (test-call "1"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-size ht))))
    ;; Key deletion
    (test-call "#f"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-delete! ht 'a)
                   (hashtable-contains? ht 'a))))
    ;; Key deletion decrements size
    (test-call "0"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-delete! ht 'a)
                   (hashtable-size ht))))
    ;; Key deletion miss does not decrement size
    (test-call "1"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-delete! ht 'c)
                   (hashtable-size ht))))
    ;; Check for existing key
    (test-call "#t"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-contains? ht 'a))))
    ;; Overwrite value for key
    (test-call "c"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-set! ht 'a 'c)
                   (hashtable-ref ht 'a))))
    ;; Copy
    (test-call "(2 b d)"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-set! ht 'c 'd)
                   (let ((ht* (hashtable-copy ht)))
                     (list (hashtable-size ht*)
                           (hashtable-ref ht* 'a)
                           (hashtable-ref ht* 'c))))))
    ;; Clear sets size to 0
    (test-call "0"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-clear! ht)
                   (hashtable-size ht))))
    ;; Clear removes all associations
    (test-call "#f"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-clear! ht)
                   (hashtable-contains? ht 'a))))
    ;; Keys of an empty table
    (test-call "()"
               (lambda ()
                 (hashtable-keys (make-eq-hashtable))))
    ;; Keys of a populated table
    (test-call "(a)"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-keys ht))))
    ;; Values of an empty table
    (test-call "()"
               (lambda ()
                 (hashtable-values (make-eq-hashtable))))
    ;; Values of a populated table
    (test-call "(b)"
               (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-values ht))))
    ;; For each iteration
    (test-call "(a b)"
               (lambda ()
                 (let ((ht (make-eq-hashtable))
                       (result #f))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-for-each (lambda (k v)
                                         (set! result (list k v)))
                                       ht)
                   result)))
    ;; Fold (result order is technically unspecified but we know what it
    ;; will be)
    (test-call "((a . b) (c . d))"
               (lambda ()
                 (let ((ht (make-eq-hashtable))
                       (result #f))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-set! ht 'c 'd)
                   (hashtable-fold (lambda (k v prev)
                                     (cons (cons k v) prev))
                                   '()
                                   ht))))
    ;; Grow/shrink
    (with-additional-imports ((only (hoot numbers) 1+))
      (test-call "100"
                 (lambda ()
                   (let ((ht (make-eq-hashtable)))
                     (do ((i 0 (1+ i)))
                         ((= i 100))
                       (hashtable-set! ht i i))
                     (do ((i 0 (1+ i)))
                         ((= i 100))
                       (hashtable-delete! ht i))
                     (do ((i 0 (1+ i)))
                         ((= i 100))
                       (hashtable-set! ht i i))
                     (hashtable-size ht)))))))
