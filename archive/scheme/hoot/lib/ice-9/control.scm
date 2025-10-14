;;; Delimited control
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
;;; Delimited control.
;;;
;;; Code:

(define-module (ice-9 control)
  ;; FIXME: Guile re-exports some things from (guile), but these would
  ;; cause duplicate definition errors in impure define-module forms
  ;; right now.
  ;; #:re-export (call-with-prompt abort-to-prompt default-prompt-tag make-prompt-tag)
  #:use-module ((hoot control) #:select (%))
  #:export (%
            abort
            call-with-escape-continuation
            call/ec
            let-escape-continuation
            let/ec))

(define (abort . args)
  (apply abort-to-prompt (default-prompt-tag) args))

(define (call-with-escape-continuation proc)
  (let ((tag (make-prompt-tag 'escape)))
    (call-with-prompt tag
      (lambda ()
        (proc (lambda vals
                (apply abort-to-prompt tag vals))))
      (lambda (k . vals)
        (apply values vals)))))

(define call/ec call-with-escape-continuation)

(define-syntax-rule (let-escape-continuation k body ...)
  (call/ec (lambda (k) body ...)))

(define-syntax-rule (let/ec k body ...)
  (let-escape-continuation k body ...))
