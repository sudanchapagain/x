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
;;; Pair tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-pairs")

(test-call "(1 . 2)" (lambda (a b) (cons a b)) 1 2)
(test-call "1" (lambda (a) (car a)) '(1 . 2))
(test-call "2" (lambda (a) (cdr a)) '(1 . 2))

(test-call "#t" (lambda (a b) (equal? a b)) '() '())
(test-call "#t" (lambda (a b) (equal? a b)) '(1 . 2) '(1 . 2))
(test-call "#t" (lambda (a b) (equal? a b)) '(1 2) '(1 2))
(test-call "#f" (lambda (a b) (equal? a b)) '() '(1))
(test-call "#f" (lambda (a b) (equal? a b)) '(1 2) '(2 1))

;; quasiquote
(test-call "(1 2 3)" (lambda (x) `(1 2 ,x)) 3)
;; unquote-splicing at the end
(test-call "(1 2 3 4)" (lambda (x) `(1 ,@x)) '(2 3 4))
;; unquote-splicing in the middle
(test-call "(1 2 3 4 5)" (lambda (x) `(1 ,@x 5)) '(2 3 4))

;; Circular lists
(test-call "#t" (lambda ()
                  (let ((x (list 'a 'b 'c))
                        (y (list 'a 'b 'c)))
                    (set-cdr! (cdr (cdr x)) x)
                    (set-cdr! (cdr (cdr y)) y)
                    (equal? x y))))
(test-call "#f" (lambda ()
                  (let ((x (list 'a 'b 'c))
                        (y (list 'a 'b)))
                    (set-cdr! (cdr (cdr x)) x)
                    (set-cdr! (cdr y) y)
                    (equal? x y))))

(test-call "(2 3)" (lambda (k l) (memv k l)) 2 '(1 2 3))

(with-additional-imports ((only (hoot lists) sort))
  (test-call "(0 1 2 2 4 5 5 6 6 7 7 10 10 11 11 12 12 12 14 14 15 15 16 17 17 19 20 20 21 21 23 23 23 24 25 25 28 29 29 30 31 32 32 34 36 37 38 38 40 40 41 44 45 48 48 49 50 51 52 54 56 58 58 59 59 60 63 66 69 69 69 72 73 74 75 76 77 78 80 81 81 83 84 87 88 88 88 89 89 90 91 92 93 94 95 95 95 95 96 99)"
             (lambda (lst) (sort lst <))
             (list 29 15 96 6 5 21 45 60 80 51 7 11 10 93 89 54 91 30 69 63 40
                   14 92 78 48 37 32 14 88 34 32 44 1 12 76 99 89 50 21 41 58
                   40 90 74 94 84 88 25 75 59 24 0 95 2 81 7 72 25 77 2 29 95
                   88 16 81 59 38 36 95 58 10 73 12 49 17 48 31 66 12 69 15 6
                   5 23 69 52 28 19 23 83 23 20 56 17 38 20 95 4 11 87)))


(test-end* "test-pairs")
