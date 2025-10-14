;;; Copyright (C) 2023, 2025 Igalia, S.L.
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
             (test utils))

(test-begin "test-number-to-string")

(define-syntax test-number->string
  (syntax-rules ()
    ((_ n radix)
     (test-number->string (number->string n radix) n radix))
    ((_ expected-output n radix)
     (test-number->string expected-output (number->string n) n radix))
    ((_ scheme-repr reflect-repr n radix)
     (let ((output (string-append scheme-repr reflect-repr)))
       (test-call output
                  (lambda (x)
                    (write-string (number->string x radix)
                                  (current-output-port))
                    (flush-output-port (current-output-port))
                    x)
                  n)))))

(test-number->string 42 10)
(test-number->string "2f" 47 16)

(test-end* "test-number-to-string")
