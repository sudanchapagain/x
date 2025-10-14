;;; Hoot implementation of Fibers
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

(define-module (fibers conditions)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (fibers waiter-queue)
  #:use-module (fibers operations)
  #:export (make-condition
            condition?
            signal-condition!
            wait-operation
            wait))

(define-record-type <condition>
  (%make-condition signalled? waiters)
  condition?
  (signalled? condition-signalled? set-condition-signalled?!)
  (waiters condition-waiters))

(define (make-condition)
  "Make a fresh condition variable."
  (%make-condition #f (make-waiter-queue)))

(define (signal-condition! cvar)
  "Mark @var{cvar} as having been signalled.  Resume any fiber or
thread waiting for @var{cvar}.  If @var{cvar} is already signalled,
calling @code{signal-condition!} does nothing and returns @code{#f};
returns @code{#t} otherwise."
  (match cvar
    (($ <condition> #f waiters)
     (set-condition-signalled?! cvar #t)
     (waiter-queue-pop-all! waiters (lambda (resume) (resume values)))
     #t)
    (($ <condition>)
     #f)))

(define (wait-operation cvar)
  "Make an operation that will complete when @var{cvar} is signalled."
  (match cvar
    (($ <condition> _ waiters)
     (define (try-fn)
       (and (condition-signalled? cvar) (lambda () (values))))
     (define (block-fn state resume)
       (waiter-queue-push! waiters state resume)
       (values))
     (make-base-operation #f try-fn block-fn))))

(define (wait cvar)
  "Wait until @var{cvar} has been signalled."
  (perform-operation (wait-operation cvar)))
