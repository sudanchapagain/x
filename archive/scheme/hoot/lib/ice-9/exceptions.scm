;;; Guile exceptions
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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
;;; Guile exceptions module.
;;;
;;; Code:

(define-module (ice-9 exceptions)
  #:pure
  #:use-module (hoot error-handling)
  #:use-module (hoot errors)
  #:use-module (hoot exceptions)
  ;; Exported in upstream (ice-9 exceptions), but can't export in Hoot
  ;; due to duplicate export errors:
  ;;
  ;; &exception
  ;; make-exception
  ;; exception?
  ;; simple-exceptions
  ;; raise-exception
  ;; &error
  ;; &non-continuable
  ;; with-exception-handler
  ;;
  ;; Commented lines refer to things that either Hoot doesn't have or
  ;; has... but is using a different name, like
  ;; make-assertion-violation instead of make-assertion-failure.
  #:re-export (
               ;; make-exception-type
               ;; exception-type?
               ;; exception-predicate
               ;; exception-accessor
               ;; exception-kind
               ;; exception-args

               &message
               make-exception-with-message
               exception-with-message?
               exception-message

               &warning
               make-warning
               warning?

               make-error
               error?

               &external-error
	       make-external-error
	       external-error?

               ;; &quit-exception
               make-quit-exception
               quit-exception?

               ;; &programming-error
               make-programming-error
	       programming-error?

	       ;; &assertion-failure
	       ;; make-assertion-failure
	       ;; assertion-failure?

	       &irritants
	       make-exception-with-irritants
               exception-with-irritants?
	       exception-irritants

	       &origin
	       make-exception-with-origin
               exception-with-origin?
	       exception-origin

               ;; make-non-continuable-error
               ;; non-continuable-error?

               &implementation-restriction
               ;; make-implementation-restriction-error
               ;; implementation-restriction-error?

               &lexical
               ;; make-lexical-error
               ;; lexical-error?

               ;; &syntax
               ;; make-syntax-error
               ;; syntax-error?
               ;; syntax-error-form
               ;; syntax-error-subform

               ;; &undefined-variable
               ;; make-undefined-variable-error
               ;; undefined-variable-error?

               define-exception-type
               raise-continuable
               guard))
