;;; Promises
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
;;; Simple JavaScript-style promises for testing fibers.
;;;
;;; Code:

(define-module (hoot promises)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-promise
            promise?
            on)
  #:replace (make-promise
             promise?))

(define-record-type <promise>
  (%make-promise state values resolve-queue reject-queue)
  promise?
  (state promise-state set-promise-state!)
  (values promise-values set-promise-values!)
  (resolve-queue promise-resolve-queue)
  (reject-queue promise-reject-queue))

(define (print-promise promise port)
  (format port "#<promise ~a>" (object-address promise)))

(set-record-type-printer! <promise> print-promise)

(define (make-promise proc)
  (let* ((resolve-q (make-q))
         (reject-q (make-q))
         (promise (%make-promise 'pending #f resolve-q reject-q)))
    (define (flush-q q vals)
      (let lp ()
        (unless (q-empty? q)
          (apply (deq! q) vals)
          (lp))))
    (define (resolve . vals)
      (set-promise-state! promise 'resolved)
      (set-promise-values! promise vals)
      (flush-q resolve-q vals))
    (define (reject . vals)
      (set-promise-state! promise 'rejected)
      (set-promise-values! promise vals)
      (flush-q reject-q vals))
    (proc resolve reject)
    promise))

;; Using Goblins style 'on' rather than 'then' because it reads nicer
;; as a prefix.
(define* (on promise on-resolve #:optional (on-reject values))
  (make-promise
   (lambda (resolve reject)
     (define (resolve* . vals)
       (call-with-values (lambda () (apply on-resolve vals))
         (lambda vals
           (apply resolve vals))))
     (define (reject* . vals)
       (call-with-values (lambda () (apply on-reject vals))
         (lambda vals
           (apply reject vals))))
     (match promise
       (($ <promise> 'pending _ resolve-q reject-q)
        (enq! resolve-q resolve*)
        (enq! reject-q reject*))
       (($ <promise> 'resolved vals)
        (apply resolve* vals))
       (($ <promise> 'rejected vals)
        (apply reject* vals))))))
