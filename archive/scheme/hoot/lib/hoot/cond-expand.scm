;;; R7RS cond-expand library
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

;;; Commentary:
;;;
;;; R7RS cond-expand implementation
;;;
;;; Code:

(library (hoot cond-expand)
  (export cond-expand)
  (import (hoot features)
          (hoot syntax)
          (only (hoot primitives) %eq? %car %cdr %cons
                %syntax->datum target-runtime))
  (define-syntax cond-expand
    (lambda (x)
      (define (has-req? req)
        (syntax-case req (and or)
          ((and req ...)
           (let lp ((reqs #'(req ...)))
             (or (%eq? reqs '())
                 (and (has-req? (%car reqs))
                      (lp (%cdr reqs))))))
          ((or req ...)
           (let lp ((reqs #'(req ...)))
             (if (%eq? reqs '())
                 #f
                 (or (has-req? (%car reqs))
                     (lp (%cdr reqs))))))
          ((not req)
           (%eq? (%syntax->datum #'not) 'not)
           (if (has-req? #'req) #f #t))
          ((library lib-name)
           (%eq? (%syntax->datum #'library) 'library)
           ;; FIXME: No libraries, for the time being.
           #f)
          (id
           (identifier? #'id)
           (let ((req (%syntax->datum #'id)))
             (let lp ((features (%cons (target-runtime) (features))))
               (if (%eq? features '())
                   #f
                   (or (%eq? req (%car features))
                       (lp (%cdr features)))))))))
      (syntax-case x (else)
        ((_)
         (syntax-violation 'cond-expand "Unfulfilled cond-expand" x))
        ((_ (else body ...))
         #'(begin body ...))
        ((_ (req body ...) more-clauses ...)
         (if (has-req? #'req)
             #'(begin body ...)
             #'(cond-expand more-clauses ...)))))))
