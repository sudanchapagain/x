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

(use-modules (srfi srfi-64)
             ((hoot compile) #:select (default-debug-level))
             (test utils))

(test-begin "test-eval")

(with-imports
 ((hoot tree-il)
  (hoot primitive-eval)
  (hoot modules)
  ;(hoot interaction-environment)
  ;; FIXME: error-handling needed to
  ;; initialize $with-exception-handler, $raise-exception et al.
  (hoot error-handling)
  (hoot environments)
  (hoot syntax))

 ;; FIXME: Can't create Tree-IL in one module and eval it in
 ;; another, because of record generativity.
 (test-call "42"
            (lambda ()
              (primitive-eval (make-const #f 42) (make-empty-module)))))

(with-imports
 ((hoot tree-il)
  (hoot eval)
  (hoot environments)
  (hoot modules)
  (hoot interaction-environment)
  ;; FIXME: error-handling and exceptions needed to
  ;; initialize $make-match-error, $raise-exception et al.
  (hoot error-handling)
  (hoot exceptions)
  (hoot syntax))

 (define-syntax test-eval
   (lambda (stx)
     (syntax-case stx ()
       ((_ form)
        (let ((repr (object->string (primitive-eval (syntax->datum #'form)))))
          #`(parameterize ((default-debug-level 1))
              (test-call #,repr
                         (lambda (exp)
                           (eval exp (environment '(hoot core-syntax))))
                         'form)))))))

 (test-eval 42)
 (test-eval '42)
 (test-eval (let ((x 42) (y 100))
              (set! x 69)
              x))
 (test-eval (let* ((x 42) (y 100))
              (set! x 69)
              x))
 (test-eval ((lambda (x y) x) 42 69))
 (test-eval ((lambda (x y) y) 42 69))
 (with-additional-imports ((scheme case-lambda))
   (test-eval ((case-lambda ((x) x) ((x y) y)) 42))
   (test-eval ((case-lambda ((x) x) ((x y) y)) 42 69)))
 (test-eval (if #t 42 69))
 (test-eval (if #f 42 69))
 (test-eval (let ((x 42))
              (when #t (set! x 69))
              x))
 (test-eval (let ((x 42))
              (unless #t (set! x 69))
              x))
 (test-eval (let lp ((x 42))
              (if x
                  (lp #f)
                  69)))
 (test-eval (letrec ((a (lambda () (b 42)))
                     (b (lambda (x) (c x 69)))
                     (c (lambda (x y) x)))
              (a)))
 (test-eval (cond
             (#f 42)
             (else 69)))
 (test-eval (cond
             (#t 42)
             (else 69)))
 (test-eval (cond
             (42)
             (else 69)))
 (test-eval (case 42
              ((42) #t)
              (else #f)))
 (test-eval (case 42
              ((69) #t)
              (else #f)))
 (test-eval (let ((x 42))
              'what
              (define (y) (z x))
              (define (z q) q)
              (y)))

 #;
 (with-additional-imports ((only (hoot numbers) 1+))
   (test-call "(2 3 4)"
              (lambda (exp)
                (eval exp (interaction-environment)))
              '(map 1+ '(1 2 3)))))

(test-end* "test-eval")
