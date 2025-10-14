;;; Copyright (C) 2025 Igalia, S.L.
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
;;; Complex number tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-complex")

(with-additional-imports ((scheme complex))
  (test-call "42" (lambda (x y) (make-rectangular x y)) 42 0)
  (test-call "1.0+2.0i" (lambda (x y) (make-rectangular x y)) 1 2)

  (test-call "0" (lambda (x y) (make-polar x y)) 0 3)
  (test-call "42" (lambda (x y) (make-polar x y)) 42 0)
  (test-call "+nan.0+nan.0i" (lambda (x y) (make-polar x y)) +inf.0 +inf.0)
  (test-call "4.322418446945118+6.731767878463172i" (lambda (x y) (make-polar x y)) 8.0 1.0))

;; Addition
(test-call "4.0+2.0i" (lambda (x y) (+ x y)) 1+2i 3)
(test-call "4.0+2.0i" (lambda (x y) (+ x y)) 3 1+2i)
(with-additional-imports
 ((hoot bitwise))
 (test-call "536870913.0+2.0i" (lambda (x y) (+ x y)) 1+2i (ash 1 29))
 (test-call "536870913.0+2.0i" (lambda (x y) (+ x y)) (ash 1 29) 1+2i))
(test-call "4.0+2.0i" (lambda (x y) (+ x y)) 1+2i 3.0)
(test-call "4.0+2.0i" (lambda (x y) (+ x y)) 3.0 1+2i)
(test-call "1.25+2.0i" (lambda (x y) (+ x y)) 1+2i 1/4)
(test-call "1.25+2.0i" (lambda (x y) (+ x y)) 1/4 1+2i)
(test-call "7.0+7.0i" (lambda (x y) (+ x y)) 3+4i 4+3i)

;; Subtraction
(test-call "0.0+2.0i" (lambda (x y) (- x y)) 1+2i 1)
(test-call "0.0-2.0i" (lambda (x y) (- x y)) 1 1+2i)
(with-additional-imports
 ((hoot bitwise))
 (test-call "-536870911.0+2.0i" (lambda (x y) (- x y)) 1+2i (ash 1 29))
 (test-call "536870911.0-2.0i" (lambda (x y) (- x y)) (ash 1 29) 1+2i))
(test-call "0.0+2.0i" (lambda (x y) (- x y)) 1+2i 1.0)
(test-call "0.0-2.0i" (lambda (x y) (- x y)) 1.0 1+2i)
(test-call "0.75+2.0i" (lambda (x y) (- x y)) 1+2i 1/4)
(test-call "-0.75-2.0i" (lambda (x y) (- x y)) 1/4 1+2i)
(test-call "-1.0+1.0i" (lambda (x y) (- x y)) 3+4i 4+3i)

;; Multiplication
(test-call "6.0+8.0i" (lambda (x y) (* x y)) 3+4i 2)
(test-call "6.0+8.0i" (lambda (x y) (* x y)) 2 3+4i)
(with-additional-imports
 ((hoot bitwise))
 (test-call "536870912.0+1073741824.0i" (lambda (x y) (* x y)) 1+2i (ash 1 29))
 (test-call "536870912.0+1073741824.0i" (lambda (x y) (* x y)) (ash 1 29) 1+2i))
(test-call "6.0+8.0i" (lambda (x y) (* x y)) 3+4i 2.0)
(test-call "6.0+8.0i" (lambda (x y) (* x y)) 2.0 3+4i)
(test-call "0.25+0.5i" (lambda (x y) (* x y)) 1+2i 1/4)
(test-call "0.25+0.5i" (lambda (x y) (* x y)) 1/4 1+2i)
(test-call "-5.0+10.0i" (lambda (x y) (* x y)) 1+2i 3+4i)

;; Division
(test-call "1.5+2.0i" (lambda (x y) (/ x y)) 3+4i 2)
(test-call "0.24-0.32i" (lambda (x y) (/ x y)) 2 3+4i)
(with-additional-imports
 ((hoot bitwise)
  (scheme complex))
 (test-call "1.0+1.0i"
            (lambda (x y) (/ x y))
            (make-rectangular (ash 1 29) (ash 1 29))
            (ash 1 29))
 (test-call "0.5-0.5i"
            (lambda (x y) (/ x y))
            (ash 1 29)
            (make-rectangular (ash 1 29) (ash 1 29))))
(test-call "1.5+2.0i" (lambda (x y) (/ x y)) 3+4i 2.0)
(test-call "0.24-0.32i" (lambda (x y) (/ x y)) 2.0 3+4i)
(test-call "4.0+8.0i" (lambda (x y) (/ x y)) 1+2i 1/4)
(test-call "0.05-0.1i" (lambda (x y) (/ x y)) 1/4 1+2i)
(test-call "1.75+0.25i" (lambda (x y) (/ x y)) 3+4i 2+2i)

;; =
(test-call "#t" (lambda (x y) (= x y)) 1 1+0.0i)
(test-call "#t" (lambda (x y) (= x y)) 18446744073709551616 18446744073709551616+0.0i)
(test-call "#t" (lambda (x y) (= x y)) 0.5 0.5+0.0i)
(test-call "#t" (lambda (x y) (= x y)) 1/2 0.5+0.0i)
(test-call "#t" (lambda (x y) (= x y)) 0.5+0.0i 0.5+0.0i)
(test-call "#f" (lambda (x y) (= x y)) 1 1+0.1i)
(test-call "#f" (lambda (x y) (= x y)) 18446744073709551616 18446744073709551616+0.1i)
(test-call "#f" (lambda (x y) (= x y)) 0.5 0.5+0.1i)
(test-call "#f" (lambda (x y) (= x y)) 1/2 0.5+0.1i)
(test-call "#f" (lambda (x y) (= x y)) 0.5+0.0i 0.5+0.1i)
(test-call "#f" (lambda (x y) (= x y)) 0.5+0.0i 0.4+0.0i)

;; eqv?
(test-call "#f" (lambda (x y) (eqv? x y)) 1 1+0.0i)
(test-call "#f" (lambda (x y) (eqv? x y)) 18446744073709551616 18446744073709551616+0.0i)
(test-call "#f" (lambda (x y) (eqv? x y)) 0.5 0.5+0.0i)
(test-call "#f" (lambda (x y) (eqv? x y)) 1/2 0.5+0.0i)
(test-call "#t" (lambda (x y) (eqv? x y)) 0.5+0.0i 0.5+0.0i)
(test-call "#f" (lambda (x y) (eqv? x y)) 0.5+0.0i 0.5+0.1i)
(test-call "#f" (lambda (x y) (eqv? x y)) 0.5+0.0i 0.4+0.0i)
(test-call "#t" (lambda (x y) (eqv? x y)) +nan.0+nan.0i +nan.0+nan.0i)
(test-call "#t" (lambda (x y) (eqv? x y)) +nan.0+1i +nan.0+1i)
(test-call "#t" (lambda (x y) (eqv? x y)) 1+nan.0i 1+nan.0i)

(test-end* "test-complex")
