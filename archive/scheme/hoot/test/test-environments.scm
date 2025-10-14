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

(test-begin "test-environments")

(with-additional-imports ((hoot modules)
                          (hoot environments)
                          (hoot syntax-transformers))

  (parameterize ((default-debug-level 1)
                 ;; FIXME: Re-enable when hoot-vm is faster.
                 (use-hoot-vm? #f))
    (test-call "#t"
               (lambda ()
                 (module? (environment))))

    (test-call "#t"
               (lambda ()
                 (syntax-transformer?
                  (module-ref (environment '(hoot core-syntax)) 'if
                              #:private? #t))))))

(test-end* "test-environments")
