;;; Cross-compilation-case
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
;;; A limited form of cond-expand, just for selecting code according to
;;; whether we are cross-compiling or not.
;;;
;;; Code:

(library (hoot cross-compilation)
  (export cross-compilation-case)
  (import (hoot core-syntax)
          (only (hoot primitives) target-runtime))

  (define-syntax cross-compilation-case
    (lambda (stx)
      (syntax-case stx ()
        ((_) #'(begin))
        ((_ (#f . body) . clauses)
         (case (target-runtime)
           ((hoot) #'(begin . body))
           (else #'(cross-compilation-case . clauses))))
        ((_ (#t . body) . clauses)
         (case (target-runtime)
           ((hoot) #'(cross-compilation-case . clauses))
           (else #'(begin . body))))))))
