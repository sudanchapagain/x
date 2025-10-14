;;; Records
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
;;; Records.
;;;
;;; Code:

(library (srfi srfi-9)
  (export define-record-type)
  (import (hoot syntax)
          (prefix (hoot records) hoot:))

  (define-syntax define-record-type
    (lambda (stx)
      (define (check-constructor-args cfields fields)
        (define (valid-cfield? cfield)
          (let lp ((fields fields))
            (syntax-case fields ()
              (()
               (syntax-violation 'define-record-type
                                 "unknown field in constructor spec"
                                 stx cfield))
              ((field . fields)
               (or (free-identifier=? cfield #'field)
                   (lp #'fields))))))
        (syntax-case cfields ()
          (() #t)
          ((cfield . cfields)
           (and (valid-cfield? #'cfield)
                (check-constructor-args #'cfields fields)))))
      (syntax-case stx ()
        ((_ rtd (constructor cfield ...)
            predicate
            (field . g&s) ...)
         (check-constructor-args #'(cfield ...) #'(field ...))
         #'(begin
             (hoot:define-record-type rtd (%constructor field ...)
                                      predicate
                                      (field . g&s) ...)
             (define constructor
               (let ((field #f) ...)
                 (lambda (cfield ...)
                   (%constructor field ...))))))))))
