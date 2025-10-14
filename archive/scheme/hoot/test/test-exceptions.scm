;;; Copyright (C) 2023, 2024 Igalia, S.L.
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
;;; Exception tests.
;;;
;;; Code:

(use-modules (hoot compile)
             (hoot reflect)
             (srfi srfi-64)
             (test utils))

(test-begin "test-exceptions")

(test-call "79" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 69)))))
(test-call "52" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise-continuable 69))))))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise-continuable 69)))
                   #:unwind? #t)))
(test-call "69" (lambda ()
                  (with-exception-handler
                   (lambda (exn) exn)
                   (lambda () (+ 10 (raise-continuable 69)))
                   #:unwind? #t)))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise 69)))
                   #:unwind? #t)))
(test-call "69" (lambda ()
                  (with-exception-handler
                   (lambda (exn) exn)
                   (lambda () (+ 10 (raise 69)))
                   #:unwind? #t)))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (error "what"))
                   #:unwind? #t)))

(with-additional-imports
 ((hoot exceptions))
 (test-call "#(#t \"hey\" (ho))"
            (lambda (message irritants)
              (let ((exn (make-compound-exception
                          (list
                           (make-exception-with-message message)
                           (make-exception-with-irritants irritants)))))
                (vector (error-object? exn)
                        (error-object-message exn)
                        (error-object-irritants exn))))
            "hey"
            '(ho)))

(test-call "42"
           (lambda ()
             (guard (condition
                     ((assq 'a condition) => cdr)
                     ((assq 'b condition)))
               (raise (list (cons 'a 42))))))

(test-call "(b . 23)"
           (lambda ()
             (guard (condition
                     ((assq 'a condition) => cdr)
                     ((assq 'b condition)))
               (raise (list (cons 'b 23))))))

;; Exception thrown from stdlib
(test-call "42"
           (lambda (x y)
             (with-exception-handler
                 (lambda (exn) 42)
               (lambda () (+ x y))
               #:unwind? #t))
           1 "two")

;; Unwind for type tests
(with-additional-imports ((hoot exceptions))
  ;; Simple exception
  (test-call "42"
             (lambda ()
               (with-exception-handler (lambda (exn) 42)
                 (lambda ()
                   (with-exception-handler (lambda (exn) 69)
                     (lambda ()
                       (raise (make-assertion-violation)))
                     #:unwind? #t
                     #:unwind-for-type &error))
                 #:unwind? #t
                 #:unwind-for-type &assertion)))
  ;; Parent type of simple exception
  (test-call "42"
             (lambda ()
               (with-exception-handler (lambda (exn) 42)
                 (lambda ()
                   (with-exception-handler (lambda (exn) 69)
                     (lambda ()
                       (raise (make-assertion-violation)))
                     #:unwind? #t
                     #:unwind-for-type &error))
                 #:unwind? #t
                 #:unwind-for-type &violation)))
  ;; Compound exception
  (test-call "42"
             (lambda ()
               (with-exception-handler (lambda (exn) 42)
                 (lambda ()
                   (with-exception-handler (lambda (exn) 69)
                     (lambda ()
                       (raise
                        (make-exception (make-assertion-violation)
                                        (make-exception-with-message "test"))))
                     #:unwind? #t
                     #:unwind-for-type &error))
                 #:unwind? #t
                 #:unwind-for-type &assertion)))
  ;; Parent type of a component of a compound exception.
  (test-call "42"
             (lambda ()
               (with-exception-handler (lambda (exn) 42)
                 (lambda ()
                   (with-exception-handler (lambda (exn) 69)
                     (lambda ()
                       (raise
                        (make-exception (make-assertion-violation)
                                        (make-exception-with-message "test"))))
                     #:unwind? #t
                     #:unwind-for-type &error))
                 #:unwind? #t
                 #:unwind-for-type &violation))))

(test-equal "re-entrant exception handling"
  42
  (compile-value '(let ()
                    (define-foreign callback
                      "host" "callback"
                      (ref null extern) -> none)
                    (with-exception-handler (lambda (exn) 42)
                      (lambda ()
                        (callback
                         (procedure->external
                          (lambda () (error "uh oh")))))
                      #:unwind? #t))
                 #:imports `(,@%default-program-imports
                             (hoot exceptions)
                             (hoot ffi))
                 #:wasm-imports
                 `(("host" .
                    (("callback" .
                      ,(lambda (proc)
                         (proc))))))))

(test-end* "test-exceptions")
