;;; Lists
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
;;; Lists.
;;;
;;; Code:

(library (hoot lists)
  (export length
          list-ref list-set! list-tail
          list?
          make-list
          reverse append list-copy
          map for-each fold
          acons
          sort)
  (import (only (hoot primitives) %append)
          (hoot apply)
          (hoot cond-expand)
          (hoot inline-wasm)
          (hoot match)
          (hoot numbers)
          (hoot pairs)
          (hoot syntax)
          (hoot values))

  (define (not x) (if x #f #t))

  (define (length l)
    (let lp ((len 0) (l l))
      (if (null? l) len (lp (1+ len) (cdr l)))))

  (define (list-ref l n)
    (let lp ((l l) (n n))
      (if (zero? n)
          (car l)
          (lp (cdr l) (1- n)))))

  (define (list-set! l n x)
    (let lp ((l l) (n n))
      (if (zero? n)
          (set-car! l x)
          (lp (cdr l) (1- n)))))

  (define (list-tail l n)
    (let lp ((l l) (n n))
      (if (zero? n)
          l
          (lp (cdr l) (1- n)))))

  (define (list? l)
    (let lp ((l l))
      (match l
        (() #t)
        ((_ . l) (lp l))
        (_ #f))))

  (define (make-list n init)
    (let lp ((n n) (out '()))
      (if (zero? n)
          out
          (lp (1- n) (cons init out)))))

  (define (reverse l)
    (let lp ((out '()) (l l))
      (match l
        (() out)
        ((head . tail) (lp (cons head out) tail)))))

  (define append
    (case-lambda
     (() '())
     ((x) x)
     ((x y) (%append x y))
     ((x y . z) (%append x (apply append y z)))))

  (define (list-copy l)
    (append l '()))

  (define (fold f seed l)
    (let lp ((seed seed) (l l))
      (match l
        (() seed)
        ((x . l) (lp (f x seed) l)))))

  ;; Temp definitions!
  (define map
    (case-lambda
     ((f l)
      (let lp ((l l))
        (match l
          (() '())
          ((x . l) (cons (f x) (lp l))))))
     ((f l1 l2)
      (let lp ((l1 l1) (l2 l2))
        (match l1
          (() '())
          ((x . l1)
           (match l2
             (() '())
             ((y . l2)
              (cons (f x y) (lp l1 l2))))))))
     ((f l1 . rest)
      (let lp ((l1 l1) (rest rest))
        (match l1
          (()
           ;; Assert the other lists are empty.
           (let lp ((rest rest))
             (match rest
               (() '())
               ((() . rest) (lp rest)))))
          ((x . l1)
           (cons (apply f x (map car rest))
                 (lp l1 (map cdr rest)))))))))

  (define for-each
    (case-lambda
     ((f l)
      (let lp ((l l))
        (unless (null? l)
          (f (car l))
          (lp (cdr l)))))
     ((f l1 l2)
      (let lp ((l1 l1) (l2 l2))
        (match l1
          (() (values))
          ((x . l1)
           (match l2
             (() (values))
             ((y . l2)
              (f x y)
              (lp l1 l2)))))))
     ((f l1 . rest)
      (let lp ((l1 l1) (rest rest))
        (match l1
          (()
           ;; Assert the other lists are empty.
           (let lp ((rest rest))
             (match rest
               (() (values))
               ((() . rest) (lp rest)))))
          ((x . l1)
           (apply f x (map car rest))
           (lp l1 (map cdr rest))))))))

  (define (acons x y z) (cons (cons x y) z))

  (define (sort items <)
    (define (split k items)
      (if (zero? k)
          (values '() items)
          (match items
            ((x . rest)
             (call-with-values (lambda () (split (1- k) rest))
               (lambda (left right)
                 (values (cons x left) right)))))))
    (define (merge left right)
      (match left
        (() right)
        ((a . rest-left)
         (match right
           (() left)
           ((b . rest-right)
            (if (< b a)
                (cons b (merge left rest-right))
                (cons a (merge rest-left right))))))))
    (define (mergesort items k)
      (match items
        ((_) items)
        (_
         (let ((k/2 (quotient k 2)))
           (call-with-values (lambda () (split k/2 items))
             (lambda (left right)
               (let ((left (mergesort left k/2))
                     (right (mergesort right (- k k/2))))
                 (merge left right))))))))
    (match items
      (() '())
      (_ (mergesort items (length items)))))

  (cond-expand
   (guile-vm)
   (hoot
    (%inline-wasm
     '(func (param $append (ref $proc))
            (global.set $append-primitive (local.get $append)))
     (lambda (x z)
       (let lp ((x x))
         (if (null? x)
             z
             (cons (car x) (lp (cdr x))))))))))
