;;; Bytevectors
;;; Copyright (C) 2024 Igalia, S.L.
;;; Copyright (C) David Thompson <dave@spritely.institute>
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
;;; Bytevectors.
;;;
;;; Code:

(library (hoot numbers)
  (export + * - /
          < <= = >= >

          1+ 1-

          abs
          floor
          ceiling
          round
          truncate
          number?
          complex?
          real?
          rational?
          integer?
          exact-integer?
          exact?
          inexact?

          finite?
          infinite?
          nan?
          inexact
          exact
          quotient
          remainder
          modulo
          even?
          odd?
          numerator
          denominator
          exact-integer-sqrt
          floor/
          floor-quotient
          floor-remainder
          ceiling/
          ceiling-quotient
          ceiling-remainder
          truncate/
          truncate-quotient
          truncate-remainder
          euclidean-quotient
          euclidean-remainder
          euclidean/
          gcd lcm

          max min

          negative?
          positive?
          zero?
          sin
          cos
          tan
          asin
          acos

          atan
          sqrt
          log
          exp

          rationalize
          square
          expt

          make-rectangular
          make-polar
          magnitude
          angle
          real-part
          imag-part

          most-positive-fixnum
          most-negative-fixnum)
  (import (only (hoot primitives)
                %+ %- %* %/
                %< %<= %= %>= %>
                %exact-integer? %complex? %real? %exact? %inexact? %integer?
                %rational? %number?
                %inexact
                %abs %floor %ceiling %sqrt
                %quotient %remainder %modulo
                %sin %cos %tan %asin %acos %atan
                %inline-wasm)
          (hoot apply)
          (hoot bitwise)
          (hoot eq)
          (hoot errors)
          (hoot match)
          (hoot not)
          (hoot syntax)
          (hoot values))

  (define (1+ x) (%+ x 1))
  (define (1- x) (%- x 1))

  (define-syntax-rule (define-associative-eta-expansion f %f)
    (define f
      (case-lambda
       (() (%f))
       ((x) (%f x))
       ((x y) (%f x y))
       ((x y . z) (apply f (%f x y) z)))))

  (define-associative-eta-expansion * %*)
  (define-associative-eta-expansion + %+)

  (define-syntax-rule (define-sub/div-eta-expansion f %f zero)
    (begin
      (define %generic
        (case-lambda
         ((y) (%f zero y))
         ((x y) (%f x y))
         ((x y . z) (apply %generic (%f x y) z))))
      (define-syntax f
        (lambda (stx)
          (syntax-case stx ()
            ((_ . x) #'(%f . x))
            (f (identifier? #'f) #'%generic))))))

  (define-sub/div-eta-expansion - %- 0)
  (define-sub/div-eta-expansion / %/ 1)

  (define-syntax-rule (define-comparison-expansion f %f)
    (begin
      (define %generic
        (case-lambda
         ((a b) (%f a b))
         ((a b . c)
          (let lp ((res (%f a b)) (a b) (c c))
            (match c
              (() res)
              ((b . c)
               (lp (and (%f a b) res) b c)))))))
      (define-syntax f
        (lambda (stx)
          (syntax-case stx ()
            ((_ x y . z) #'(%f x y . z))
            (f (identifier? #'f) #'%generic))))))

  (define-comparison-expansion < %<)
  (define-comparison-expansion <= %<=)
  (define-comparison-expansion = %=)
  (define-comparison-expansion >= %>=)
  (define-comparison-expansion > %>)

  (define (number? x) (%number? x))
  (define (complex? x) (%complex? x))
  (define (real? x) (%real? x))
  (define (rational? x) (%rational? x))
  (define (integer? x) (%integer? x))
  (define (exact-integer? x) (%exact-integer? x))
  (define (exact? x) (%exact? x))
  (define (inexact? x) (%inexact? x))

  (define (abs x) (%abs x))
  (define (floor x) (%floor x))
  (define (ceiling x) (%ceiling x))
  (define (round x) (%floor (+ x 0.5)))
  (define (truncate x)
    (check-type x real? 'truncate)
    (if (exact? x)
        (if (integer? x)
            x
            (truncate-quotient (numerator x) (denominator x)))
        (%inline-wasm
         '(func (param $x f64) (result f64)
                (f64.trunc (local.get $x)))
         x)))

  ;; Unlike R7RS, these only operate on real numbers.
  (define (infinite? x)
    (or (= x +inf.0) (= x -inf.0)))
  (define (nan? x)
    (not (= x x)))
  (define (finite? x)
    (and (not (infinite? x)) (not (nan? x))))

  (define (inexact x) (%inexact x))
  (define (exact x)
    (cond
     ((exact? x) x)
     (else
      (check-type x finite? 'exact)
      (call-with-values (lambda ()
                          (%inline-wasm
                           '(func (param $x f64)
                                  (result (ref eq) (ref eq))
                                  (call $f64->ratio (local.get $x)))
                           x))
        (lambda (num denom) (/ num denom))))))

  (define (quotient x y) (%quotient x y))
  (define (remainder x y) (%remainder x y))
  (define (modulo x y) (%modulo x y))

  (define (even? x) (zero? (logand x 1)))
  (define (odd? x) (not (even? x)))

  (define (numerator x)
    (cond
     ((exact-integer? x) x)
     ((exact? x)
      (%inline-wasm
       '(func (param $x (ref $fraction))
              (result (ref eq))
              (struct.get $fraction $num (local.get $x)))
       x))
     (else (inexact (numerator (exact x))))))
  (define (denominator x)
    (cond
     ((exact-integer? x) 1)
     ((exact? x)
      (%inline-wasm
       '(func (param $x (ref $fraction))
              (result (ref eq))
              (struct.get $fraction $denom (local.get $x)))
       x))
     (else (inexact (denominator (exact x))))))
  (define (exact-integer-sqrt n)
    ;; FIXME: There's a compiler bug that makes this procedure return
    ;; junk when this exact-integer? check is enabled.
    ;;
    (check-type n exact-integer? 'exact-integer-sqrt)
    (assert (>= n 0) 'exact-integer-sqrt)
    (let loop ((x n) (y (quotient (+ n 1) 2)))
      (if (< y x)
          (loop y (quotient (+ y (quotient n y)) 2))
          (values x (- n (* x x))))))

  ;; Division operations adapted from SRFI-141 reference implementation.
  ;;
  ;; Copyright (c) 2010--2011 Taylor R. Campbell
  ;; All rights reserved.
  ;;
  ;; Redistribution and use in source and binary forms, with or without
  ;; modification, are permitted provided that the following conditions
  ;; are met:
  ;; 1. Redistributions of source code must retain the above copyright
  ;;    notice, this list of conditions and the following disclaimer.
  ;; 2. Redistributions in binary form must reproduce the above copyright
  ;;    notice, this list of conditions and the following disclaimer in the
  ;;    documentation and/or other materials provided with the distribution.
  ;;
  ;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
  ;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  ;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
  ;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  ;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  ;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  ;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  ;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  ;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  ;; SUCH DAMAGE.
  (define (floor-/+ n d)
    (let ((n (- n)))
      (let ((q (quotient n d)) (r (remainder n d)))
        (if (zero? r)
            (values (- q) r)
            (values (1- (- q)) (- d r))))))
  (define (floor+/- n d)
    (let ((d (- d)))
      (let ((q (quotient n d)) (r (remainder n d)))
        (if (zero? r)
            (values (- q) r)
            (values (1- (- q)) (- r d))))))
  (define (floor/ n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (let ((n (- n)) (d (- d)))
            (values (quotient n d) (- (remainder n d)))))
         ((negative? n) (floor-/+ n d))
         ((negative? d) (floor+/- n d))
         (else (values (quotient n d) (remainder n d))))
        (let ((q (floor (/ n d))))
          (values q (- n (* d q))))))
  (define (floor-quotient n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (quotient (- n) (- d)))
         ((negative? n)
          (call-with-values (lambda () (floor-/+ n d))
            (lambda (q r) q)))
         ((negative? d)
          (call-with-values (lambda () (floor+/- n d))
            (lambda (q r) q)))
         (else (quotient n d)))
        (floor (/ n d))))
  (define (floor-remainder n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (- (remainder (- n) (- d))))
         ((negative? n)
          (call-with-values (lambda () (floor-/+ n d))
            (lambda (q r) r)))
         ((negative? d)
          (call-with-values (lambda () (floor+/- n d))
            (lambda (q r) r)))
         (else (remainder n d)))
        (- n (* d (floor (/ n d))))))

  (define (ceiling-/- n d)
    (let ((n (- n)) (d (- d)))
      (let ((q (quotient n d)) (r (remainder n d)))
        (if (zero? r)
            (values q r)
            (values (1+ q) (- d r))))))
  (define (ceiling+/+ n d)
    (let ((q (quotient n d)) (r (remainder n d)))
      (if (zero? r)
          (values q r)
          (values (1+ q) (- r d)))))
  (define (ceiling/ n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (ceiling-/- n d))
         ((negative? n)
          (let ((n (- n)))
            (values (- (quotient n d)) (- (remainder n d)))))
         ((negative? d)
          (let ((d (- d)))
            (values (- (quotient n d)) (remainder n d))))
         (else (ceiling+/+ n d)))
        (let ((q (ceiling (/ n d))))
          (values q (- n (* d q))))))
  (define (ceiling-quotient n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (call-with-values (lambda () (ceiling-/- n d))
            (lambda (q r) q)))
         ((negative? n) (- (quotient (- n) d)))
         ((negative? d) (- (quotient n (- d))))
         (else
          (call-with-values (lambda () (ceiling+/+ n d))
            (lambda (q r) q))))
        (ceiling (/ n d))))
  (define (ceiling-remainder n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (call-with-values (lambda () (ceiling-/- n d))
            (lambda (q r) r)))
         ((negative? n) (- (remainder (- n) d)))
         ((negative? d) (remainder n (- d)))
         (else
          (call-with-values (lambda () (ceiling+/+ n d))
            (lambda (q r) r))))
        (- n (* d (ceiling (/ n d))))))

  (define (truncate/ n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (let ((n (- n)) (d (- d)))
            (values (quotient n d) (- (remainder n d)))))
         ((negative? n)
          (let ((n (- n)))
            (values (- (quotient n d)) (- (remainder n d)))))
         ((negative? d)
          (let ((d (- d)))
            (values (- (quotient n d)) (remainder n d))))
         (else
          (values (quotient n d) (remainder n d))))
        (let ((q (truncate (/ n d))))
          (values q (- n (* d q))))))
  (define (truncate-quotient n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d)) (quotient (- n) (- d)))
         ((negative? n) (- (quotient (- n) d)))
         ((negative? d) (- (quotient n (- d))))
         (else (quotient n d)))
        (truncate (/ n d))))
  (define (truncate-remainder n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (- (remainder (- n) (- d))))
         ((negative? n) (- (remainder (- n) d)))
         ((negative? d) (remainder n (- d)))
         (else (remainder n d)))
        (- n (* d (truncate (/ n d))))))

  (define (euclidean/ n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d)) (ceiling-/- n d))
         ((negative? n) (floor-/+ n d))
         ((negative? d)
          (let ((d (- 0 d)))
            (values (- 0 (quotient n d)) (remainder n d))))
         (else (values (quotient n d) (remainder n d))))
        (let ((q (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))
          (values q (- n (* d q))))))
  (define (euclidean-quotient n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (call-with-values (lambda () (ceiling-/- n d))
            (lambda (q r) q)))
         ((negative? n)
          (call-with-values (lambda () (floor-/+ n d))
            (lambda (q r) q)))
         ((negative? d) (- (quotient n (- d))))
         (else (quotient n d)))
        (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))
  (define (euclidean-remainder n d)
    (if (and (exact-integer? n) (exact-integer? d))
        (cond
         ((and (negative? n) (negative? d))
          (call-with-values (lambda () (ceiling-/- n d))
            (lambda (q r) r)))
         ((negative? n)
          (call-with-values (lambda () (floor-/+ n d))
            (lambda (q r) r)))
         ((negative? d) (remainder n (- d)))
         (else (remainder n d)))
        (- n (* d (if (negative? d) (ceiling (/ n d)) (floor (/ n d)))))))

  (define gcd
    (case-lambda
      (() 0)
      ((x)
       (check-type x integer? 'gcd)
       x)
      ((x y)
       (cond
        ((or (eq? x 0) (eq? y 0)) 0)
        ((and (exact-integer? x) (exact-integer? y))
         (/ (abs y) (denominator (/ x y))))
        (else
         (check-type x integer? 'gcd)
         (check-type y integer? 'gcd)
         (inexact (gcd (exact x) (exact y))))))
      ((x y . z)
       (apply gcd (gcd x y) z))))

  (define lcm
    (case-lambda
     (() 1)
     ((x)
      (check-type x integer? 'lcm)
      x)
     ((x y)
      (cond
       ((and (eq? x 0) (eq? y 0)) 0)
       ((and (exact-integer? x) (exact-integer? y))
        (quotient (abs (* x y)) (gcd x y)))
       (else
        (check-type x integer? 'lcm)
        (check-type y integer? 'lcm)
        (inexact (lcm (exact x) (exact y))))))
     ((x y . z)
      (apply lcm (lcm x y) z))))

  (define-syntax-rule (define-minmax-expansion f %f compare)
    (begin
      (define %f
        (case-lambda
          ((x) x)
          ((x y) (if (compare x y) x y))
          ((x . rest)
           (let lp ((m x)
                    (rest rest))
             (match rest
               (() m)
               ((x . rest)
                (if (compare x m)
                    (lp x rest)
                    (lp m rest))))))))
      (define-syntax f
        (lambda (stx)
          (syntax-case stx ()
            (f (identifier? #'f) #'%f)
            ((_ x) #'x)
            ((_ x y)
             #'(let ((x* x)
                     (y* y))
                 (if (compare x* y*) x* y*)))
            ((_ x . y)
             #'(f x (f . y))))))))

  (define-minmax-expansion min %min <)
  (define-minmax-expansion max %max >)

  (define (negative? x) (< x 0))
  (define (positive? x) (> x 0))
  (define (zero? x) (= x 0))

  (define (sin x) (%sin x))
  (define (cos x) (%cos x))
  (define (tan x) (%tan x))
  (define (asin x) (%asin x))
  (define (acos x) (%acos x))
  (define atan
    (case-lambda
     ((x) (%atan x))
     ((x y) (%atan x y))))
  (define (sqrt x) (%sqrt x))

  (define* (log x #:optional y)
    (define (%log x)
      (check-type x real? 'log)
      (%inline-wasm
       '(func (param $x f64) (result (ref eq))
              (call $log (local.get $x)))
       (inexact x)))
    (if y
        (/ (%log x)
           (%log y))
        (%log x)))

  (define (exp x)
    (define (%exp x)
      (check-type x real? 'log)
      (%inline-wasm
       '(func (param $x f64) (result (ref eq))
              (call $exp (local.get $x)))
       (inexact x)))
    (%exp x))

  ;; Adapted from the comments for scm_rationalize in libguile's numbers.c
  (define (rationalize x y)
    (check-type x rational? 'rationalize)
    (check-type y rational? 'rationalize)
    (define (exact-rationalize x eps)
      (let ((n1  (if (negative? x) -1 1))
            (x   (abs x))
            (eps (abs eps)))
        (let ((lo (- x eps))
              (hi (+ x eps)))
          (if (<= lo 0)
              0
              (let loop ((nlo (numerator lo)) (dlo (denominator lo))
                         (nhi (numerator hi)) (dhi (denominator hi))
                         (n1 n1) (d1 0) (n2 0) (d2 1))
                (let-values (((qlo rlo) (floor/ nlo dlo))
                             ((qhi rhi) (floor/ nhi dhi)))
                  (let ((n0 (+ n2 (* n1 qlo)))
                        (d0 (+ d2 (* d1 qlo))))
                    (cond ((zero? rlo) (/ n0 d0))
                          ((< qlo qhi) (/ (+ n0 n1) (+ d0 d1)))
                          (else (loop dhi rhi dlo rlo n0 d0 n1 d1))))))))))
    (if (and (exact? x) (exact? y))
        (exact-rationalize x y)
        (inexact (exact-rationalize (exact x) (exact y)))))

  (define (square x) (* x x))

  (define (expt x y)
    (check-type x number? 'expt)
    (check-type y number? 'expt)
    (cond
     ((eqv? x 0)
      (cond ((zero? y) (if (exact? y) 1 1.0))
            ((positive? y) (if (exact? y) 0 0.0))
            (else +nan.0)))
     ((eqv? x 0.0)
      (cond ((zero? y) 1.0)
            ((positive? y) 0.0)
            (else +nan.0)))
     ((exact-integer? y)
      (if (< y 0)
          (/ 1 (expt x (abs y)))
          (let lp ((y y)
                   (result 1))
            (if (= y 0)
                result
                (lp (1- y) (* x result))))))
     (else (exp (* y (log x))))))

  ;; (scheme complex)
  ;; Adapted from Guile's numbers.c
  (define (make-rectangular real imag)
    (check-type real real? 'make-rectangular)
    (check-type imag real? 'make-rectangular)
    (if (eq? imag 0)
        real
        (%inline-wasm
         '(func (param $real f64) (param $imag f64) (result (ref eq))
                (struct.new $complex
                            (i32.const 0)
                            (local.get $real)
                            (local.get $imag)))
         (inexact real) (inexact imag))))
  (define (make-polar mag ang)
    (check-type mag real? 'make-polar)
    (check-type ang real? 'make-polar)
    (cond
     ((eq? mag 0) 0)
     ((eq? ang 0) mag)
     (else
      (%inline-wasm
       '(func (param $mag f64) (param $ang f64) (result (ref eq))
              (local $f0 f64) (local $f1 f64)
              (local.set $f0 (call $fcos (local.get $ang)))
              (local.set $f1 (call $fsin (local.get $ang)))
              ;; If sin/cos are NaN and magnitude is 0, return a complex
              ;; zero.
              (if (ref eq)
                  (i32.and (call $f64-is-nan (local.get $f0))
                           (call $f64-is-nan (local.get $f1))
                           (f64.eq (local.get $mag) (f64.const 0.0)))
                  (then (struct.new $complex
                                    (i32.const 0)
                                    (f64.const 0.0)
                                    (f64.const 0.0)))
                  (else (struct.new $complex
                                    (i32.const 0)
                                    (f64.mul (local.get $mag) (local.get $f0))
                                    (f64.mul (local.get $mag) (local.get $f1))))))
       (inexact mag) (inexact ang)))))
  (define (magnitude z)
    (cond
     ((real? z) (abs z))
     (else
      (check-type z complex? 'magnitude)
      (let ((r (real-part z))
            (i (imag-part z)))
        (sqrt (+ (* r r) (* i i)))))))
  (define (angle z)
    (cond
     ((real? z)
      (if (negative? z)
          (atan 0.0 -1.0)
          0.0))
     (else
      (check-type z complex? 'angle)
      (atan (imag-part z) (real-part z)))))
  (define (real-part z)
    (cond
     ((real? z) z)
     (else
      (check-type z complex? 'real-part)
      (%inline-wasm
       '(func (param $z (ref $complex)) (result f64)
              (struct.get $complex $real (local.get $z)))
       z))))
  (define (imag-part z)
    (cond
     ((real? z) 0.0)
     (else
      (check-type z complex? 'real-part)
      (%inline-wasm
       '(func (param $z (ref $complex)) (result f64)
              (struct.get $complex $imag (local.get $z)))
       z))))

  (define most-negative-fixnum (ash -1 29))
  (define most-positive-fixnum (- (ash 1 29) 1))
  )
