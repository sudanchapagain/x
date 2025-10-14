;;; Copyright (C) 2023, 2024, 2025 Igalia, S.L.
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
;;; Procedure tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-procedures")

(test-call "()" (lambda args args))
(test-call "(1)" (lambda args args) 1)
(test-call "(1 2)" (lambda args args) 1 2)
(test-call "(1 2 3)" (lambda args args) 1 2 3)
(test-call "(1 2 3 4)" (lambda args args) 1 2 3 4)
(test-call "(1 2 3 4 5)" (lambda args args) 1 2 3 4 5)
(test-call "(1 2 3 4 5 6)" (lambda args args) 1 2 3 4 5 6)
(test-call "(1 2 3 4 5 6 7)" (lambda args args) 1 2 3 4 5 6 7)
(test-call "(1 2 3 4 5 6 7 8)" (lambda args args) 1 2 3 4 5 6 7 8)
(test-call "(1 2 3 4 5 6 7 8 9)" (lambda args args) 1 2 3 4 5 6 7 8 9)
(test-call "(1 2 3 4 5 6 7 8 9 10)" (lambda args args) 1 2 3 4 5 6 7 8 9 10)

(with-additional-imports ((only (hoot pairs) cons*))
  (test-call "(1 2)" (lambda (a . args) (cons* a args)) 1 2)
  (test-call "(1 2 3)" (lambda (a b . args) (cons* a b args)) 1 2 3)
  (test-call "(1 2 3 4)" (lambda (a b c . args) (cons* a b c args)) 1 2 3 4)
  (test-call "(1 2 3 4 5)" (lambda (a b c d . args) (cons* a b c d args)) 1 2 3 4 5)
  (test-call "(1 2 3 4 5 6 7 8 9 10 11 12)"
             (lambda (a b c d e f g h i j . args)
               (cons* a b c d e f g h i j args))
             1 2 3 4 5 6 7 8 9 10 11 12))

;; inner call that grows argv
(test-call "36"
           (lambda (f) (f 1 2 3 4 5 6 7 8))
           (lambda (a b c d e f g h) (+ a b c d e f g h)))

(with-additional-imports ((only (hoot pairs) cons*))
  ;; inner apply that grows argv
  (test-call "(1 2 3 4 5 6 7 8 9 10 11 12 13 14)"
             (lambda (args) (apply cons* args))
             (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 '())))

(with-additional-imports ((only (hoot syntax) lambda*)
                          (only (hoot pairs) cons*))
  (test-call "#f" (lambda* (#:optional a) a))
  (test-call "(42 69)" (lambda* (#:optional (a 42) (b 69)) (list a b)))
  (test-call "(10 20)" (lambda* (#:optional (a 42) (b 69)) (list a b)) 10 20)

  (test-call "(1 #f ())" (lambda* (a #:optional b . rest) (list a b rest)) 1)

  (test-call "(1 2 3 4 5 6 7 8 9 10 11 12)"
             (lambda* (a b c d #:optional e f g h i j . args)
               (cons* a b c d e f g h i j args))
             1 2 3 4 5 6 7 8 9 10 11 12)

  (test-call "(1 2 3 4 5 6 #f #f #f #f)"
             (lambda* (a b c d #:optional e f g h i j . args)
               (cons* a b c d e f g h i j args))
             1 2 3 4 5 6))

(test-call "20" (lambda (f . args) (apply f args)) (lambda (x y) (+ x y)) 12 8)
(test-call "12\n8" (lambda (f . args) (apply f args)) values 12 8)
(test-call "(1 2 3)"
           (lambda (thunk) (call-with-values thunk list))
           (lambda () (values 1 2 3)))

(test-call "52" (lambda (f) ((f 42))) (lambda (n) (lambda () (+ n 10))))

(with-additional-imports ((only (hoot numbers) 1-))
  (test-call "120" (lambda (n)
                     (let fac ((n n))
                       (if (eq? n 0)
                           1
                           (* n (fac (1- n))))))
             5))

(with-additional-imports ((scheme case-lambda))
  (test-call "42" (case-lambda ((a) a) ((a b) (+ a b))) 42)
  (test-call "52" (case-lambda ((a) a) ((a b) (+ a b))) 42 10))

(with-additional-imports ((only (hoot syntax) case-lambda*))
  (test-call "42" (case-lambda* ((a) a) ((a b) (+ a b))) 42)
  (test-call "52" (case-lambda* ((a) a) ((a b) (+ a b))) 42 10)
  (test-call "69" (case-lambda* ((#:optional (a 69)) a) ((a b) (+ a b))))
  (test-call "42" (case-lambda* ((#:optional (a 69)) a) ((a b) (+ a b))) 42)
  (test-call "52" (case-lambda* ((#:optional (a 69)) a) ((a b) (+ a b))) 42 10))

(with-additional-imports ((only (hoot syntax) lambda*))
  (test-call "69" (lambda* (#:key (a 69)) a))
  (test-call "42" (lambda* (#:key (a 69)) a) #:a 42)
  (test-call "10" (lambda* (#:key (a 69)) a) #:a 42 #:a 10)

  (test-call "(69 69)" (lambda* (#:key (a 69) (b a)) (list a b)))
  (test-call "(42 42)" (lambda* (#:key (a 69) (b a)) (list a b)) #:a 42)
  (test-call "(10 10)" (lambda* (#:key (a 69) (b a)) (list a b)) #:a 42 #:a 10)

  (test-call "(69 42)" (lambda* (#:key (a 69) (b a)) (list a b)) #:b 42)
  (test-call "(42 69)" (lambda* (#:key (a 69) (b a)) (list a b)) #:a 42 #:b 69)
  (test-call "(10 42)" (lambda* (#:key (a 69) (b a)) (list a b)) #:b 42 #:a 10)

  (test-call "(1 2 3)" (lambda* (a #:optional (b 2) #:key (c 3)) (list a b c)) 1)
  (test-call "(1 42 3)" (lambda* (a #:optional (b 2) #:key (c 3)) (list a b c)) 1 42)
  (test-call "(1 2 42)" (lambda* (a #:optional (b 2) #:key (c 3)) (list a b c)) 1 #:c 42)
  (test-call "(1 42 69)" (lambda* (a #:optional (b 2) #:key (c 3)) (list a b c)) 1 42 #:c 69)

  (test-call "(1 2 3 ())"
             (lambda* (a #:optional (b 2) #:key (c 3) #:rest d) (list a b c d)) 1)
  (test-call "(1 42 3 ())"
             (lambda* (a #:optional (b 2) #:key (c 3) #:rest d) (list a b c d)) 1 42)
  (test-call "(1 2 42 (#:c 42))"
             (lambda* (a #:optional (b 2) #:key (c 3) #:rest d) (list a b c d)) 1 #:c 42)
  (test-call "(1 42 69 (#:c 69))"
             (lambda* (a #:optional (b 2) #:key (c 3) #:rest d) (list a b c d)) 1 42 #:c 69)
  (test-call "(1 42 100 (#:c 69 #:c 100))"
             (lambda* (a #:optional (b 2) #:key (c 3) #:rest d) (list a b c d)) 1 42 #:c 69 #:c 100))

(with-additional-imports ((only (hoot syntax) case-lambda*))
  (test-call "(second 3)"
             (case-lambda* ((a #:key (b 2)) (list 'first a b))
                           ((#:key (c 3)) (list 'second c))))
  ;; FIXME: this test passes with v8 but fails on the hoot VM.
  ;;(test-call "(second 42)"
  ;;           (case-lambda* ((a #:key (b 2)) (list 'first a b))
  ;;                         ((#:key (c 3)) (list 'second c)))
  ;;           #:c 42)
  (test-call "(first 10 2)"
             (case-lambda* ((a #:key (b 2)) (list 'first a b))
                           ((#:key (c 3)) (list 'second c)))
             10)
  (test-call "(first 10 20)"
             (case-lambda* ((a #:key (b 2)) (list 'first a b))
                           ((#:key (c 3)) (list 'second c)))
             10 #:b 20))

(with-additional-imports ((hoot exceptions))
  (test-call "not-a-procedure"
             (lambda (proc)
               (with-exception-handler (lambda (exn)
                                         (and (failed-type-check? exn)
                                              'not-a-procedure))
                 (lambda ()
                   (proc 'arg))
                 #:unwind? #t))
             'nope)
  (test-call "not-a-procedure"
             (lambda (proc . args)
               (with-exception-handler (lambda (exn)
                                         (and (failed-type-check? exn)
                                              'not-a-procedure))
                 (lambda ()
                   (apply proc args))
                 #:unwind? #t))
             'nope 1 2 3))

;; (test-call "9227465" (lambda (n)
;;                        (let fib ((n n))
;;                          (if (<= n 1)
;;                              1
;;                              (+ (fib (- n 1)) (fib (- n 2))))))
;;            34)

;; (test-call "1000000" (lambda ()
;;                        (let lp ((n 0))
;;                          (if (< n #e1e6)
;;                              (lp (1+ n))
;;                              n))))

(with-additional-imports ((only (hoot procedures) procedure-name))
  (test-call "list" procedure-name list))

(test-end* "test-procedures")
