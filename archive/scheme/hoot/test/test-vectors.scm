;;; Copyright (C) 2023 Igalia, S.L.
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
;;; Vector tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-vectors")

(test-call "#(1 2 3)" (lambda (a b c) (vector a b c)) 1 2 3)
(test-call "3" (lambda (v) (vector-ref v 2)) #(1 2 3))
(test-call "2" (lambda (v idx) (vector-ref v idx)) #(1 2 3) 1)
(test-call "#(42 42 42)" (lambda (n) (make-vector n 42)) 3)

(test-call "#t" (lambda (a b) (equal? a b)) #() #())
(test-call "#t" (lambda (a b) (equal? a b)) #(1 2) #(1 2))
(test-call "#f" (lambda (a b) (equal? a b)) #() #(1))
(test-call "#f" (lambda (a b) (equal? a b)) #(1 2) #(2 1))
;; Vectors with cycles
(test-call "#t" (lambda ()
                  (let ((x (vector 'a 'b 'c #f))
                        (y (vector 'a 'b 'c #f)))
                    (vector-set! x 3 x)
                    (vector-set! y 3 y)
                    (equal? x y))))
(test-call "#f" (lambda ()
                  (let ((x (vector 'a 'b 'c #f))
                        (y (vector 'a 'b #f)))
                    (vector-set! x 3 x)
                    (vector-set! y 2 y)
                    (equal? x y))))

(with-additional-imports ((only (hoot vectors) vector-sort! vector-move-left!))
  (test-call "#(4 4 7 8 9 9 10 12 12 13 15 17 18 18 19 19 19 21 21 21 24 24 24 25 25 26 27 28 30 34 35 35 35 35 37 38 39 41 43 43 45 46 48 49 49 52 52 53 53 54 54 55 55 56 56 57 57 58 61 61 62 62 63 64 64 65 66 67 67 69 69 69 73 73 77 79 79 79 80 80 80 84 84 84 85 85 87 87 88 88 89 91 92 95 95 96 96 97 98 99)"
             (lambda (v)
               (vector-sort! v <)
               v)
             (vector 56 19 80 52 18 37 13 63 96 30 79 84 18 64 53 67 8 24 85
                     21 79 62 55 17 21 58 26 73 46 10 69 48 15 4 45 96 89 88
                     35 9 35 97 69 43 92 61 56 61 52 80 95 84 88 24 4 7 91 80
                     66 77 39 12 85 69 34 57 35 62 38 28 95 79 84 65 25 99 43
                     54 27 9 54 57 53 87 25 87 49 35 12 49 19 98 24 64 21 19
                     67 41 55 73))
  (test-call "#(2 2 3)"
             (lambda (v)
               (vector-move-left! v 1 2 v 0)
               v)
             (vector 1 2 3)))

(test-end* "test-vectors")
