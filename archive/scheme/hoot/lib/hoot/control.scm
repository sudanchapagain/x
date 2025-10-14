;;; Delimited control
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
;;; Delimited control.
;;;
;;; Code:

(library (hoot control)
  (export make-prompt-tag
          default-prompt-tag
          call-with-prompt
          abort-to-prompt

          %
          default-prompt-handler

          call-with-current-continuation
          call/cc)
  (import (hoot apply)
          (hoot cond-expand)
          (hoot inline-wasm)
          (hoot parameters)
          (rename (only (hoot primitives)
                        %cons %abort-to-prompt
                        %call-with-prompt)
                  (%abort-to-prompt abort-to-prompt))
          (hoot syntax)
          (hoot values))

  (define* (make-prompt-tag #:optional (stem "prompt"))
    (%cons stem '()))

  (cond-expand
   (guile-vm)
   (hoot
    (define default-prompt-tag
      (%inline-wasm
       '(func (result (ref eq))
              (global.get $default-prompt-tag))))))

  (define-syntax-rule (define-primcall f %f arg ...)
    (begin
      (define (generic arg ...)
        (%f arg ...))
      (define-syntax f
        (lambda (stx)
          (syntax-case stx ()
            ((_ . x) #'(%f . x))
            (id (identifier? #'id) #'generic))))))
  (define-primcall call-with-prompt %call-with-prompt tag body handler)

  (define-syntax %
    (syntax-rules ()
      ((_ expr)
       (call-with-prompt (default-prompt-tag)
                         (lambda () expr)
                         default-prompt-handler))
      ((_ expr handler)
       (call-with-prompt (default-prompt-tag)
                         (lambda () expr)
                         handler))
      ((_ tag expr handler)
       (call-with-prompt tag
                         (lambda () expr)
                         handler))))

  (define (default-prompt-handler k proc) (% (proc k)))

  ;; This is an implementation of call/cc in terms of delimited
  ;; continuations.  It correct except as regards dynamic-wind: capturing
  ;; the continuation unwinds all dynamic-winds, then rewinds them; and
  ;; invoking the continuation does the same, even if the invoking and
  ;; captured continuations overlap.  Oh well; call/cc is strictly less
  ;; useful than call-with-prompt anyway.
  (define (call-with-current-continuation proc)
    (define (unwind-and-call handler)
      (abort-to-prompt (default-prompt-tag) handler))

    (define (rewind-and-continue captured-continuation)
      (define-syntax-rule (reinstate expr)
        (captured-continuation (lambda () expr)))
      (define (k . args)
        (define (rewind-and-return-values discarded-continuation)
          (reinstate (apply values args)))
        (unwind-and-call rewind-and-return-values))
      (reinstate (proc k)))

    (let ((thunk (unwind-and-call rewind-and-continue)))
      (thunk)))

  (define call/cc call-with-current-continuation))
