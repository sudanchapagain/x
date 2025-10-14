;;; Simple pattern-matcher
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
;;; Simple pattern matcher, based on Oleg Kiselyov's pmatch.
;;;
;;; Code:

(library (hoot match)
  (export match)
  (import (only (hoot primitives)
                %eq? %+ %vector? %vector-length %vector-ref
                %null? %pair? %car %cdr)
          (hoot syntax)
          (hoot errors))

  (define (vector-ref v n) (%vector-ref v n))
  (define (null? x) (%null? x))
  (define (eq? x y) (%eq? x y))
  (define (1+ x) (%+ x 1))
  (define (vector? x) (%vector? x))
  (define (pair? x) (%pair? x))
  (define (car x) (%car x))
  (define (cdr x) (%cdr x))
  (define (vector-length x) (%vector-length x))

  (define (length x)
    (if (null? x)
        0
        (1+ (length (cdr x)))))

  (define-syntax-rule (simple-match e cs ...)
    (let ((v e)) (simple-match-1 v cs ...)))

  (define-syntax simple-match-1
    (syntax-rules ()
      ((_ v) (raise (make-match-error v)))
      ((_ v (pat e0 e ...) cs ...)
       (let ((fk (lambda () (simple-match-1 v cs ...))))
         (simple-match-pat v pat (let () e0 e ...) (fk))))))

  (define-syntax simple-match-patv
    (syntax-rules ()
      ((_ v idx () kt kf) kt)
      ((_ v idx (x . y) kt kf)
       (simple-match-pat (vector-ref v idx) x
                         (simple-match-patv v (1+ idx) y kt kf)
                         kf))))

  (define-syntax simple-match-pat
    (syntax-rules (_ quote unquote ? and or not)
      ((_ v _ kt kf) kt)
      ((_ v () kt kf) (if (null? v) kt kf))
      ((_ v #t kt kf) (if (eq? v #t) kt kf))
      ((_ v #f kt kf) (if (eq? v #f) kt kf))
      ((_ v (and) kt kf) kt)
      ((_ v (and x . y) kt kf)
       (simple-match-pat v x (simple-match-pat v (and . y) kt kf) kf))
      ((_ v (or) kt kf) kf)
      ((_ v (or x . y) kt kf)
       (let ((tk (lambda () kt)))
         (simple-match-pat v x (tk) (simple-match-pat v (or . y) (tk) kf))))
      ((_ v (not pat) kt kf) (simple-match-pat v pat kf kt))
      ((_ v (quote lit) kt kf)
       (if (eq? v (quote lit)) kt kf))
      ((_ v (? proc) kt kf) (simple-match-pat v (? proc _) kt kf))
      ((_ v (? proc pat) kt kf)
       (if (proc v) (simple-match-pat v pat kt kf) kf))
      ((_ v (x . y) kt kf)
       (if (pair? v)
           (let ((vx (car v)) (vy (cdr v)))
             (simple-match-pat vx x (simple-match-pat vy y kt kf) kf))
           kf))
      ((_ v #(x ...) kt kf)
       (if (and (vector? v)
                (eq? (vector-length v) (length '(x ...))))
           (simple-match-patv v 0 (x ...) kt kf)
           kf))
      ((_ v var kt kf) (let ((var v)) kt))))

  (define-syntax-rule (match e cs ...) (simple-match e cs ...)))
