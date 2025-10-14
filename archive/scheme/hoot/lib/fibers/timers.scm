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

(define-module (fibers timers)
  #:use-module (fibers scheduler)
  #:use-module (fibers operations)
  #:use-module (scheme time)
  #:export (sleep-operation
            timer-operation
            sleep))

(define (timer-operation expiry)
  "Make an operation that will succeed when the current time is
greater than or equal to @var{expiry}, expressed in internal time
units.  The operation will succeed with no values."
  (define (try-fn)
    (and (< expiry (current-jiffy))
         (lambda () (values))))
  (define (block-fn state resume)
    (schedule-task (lambda ()
                     (when (op-state-complete! state)
                       (resume (lambda () (values)))))
                   (max 0 (- expiry (current-jiffy)))))
  (make-base-operation #f try-fn block-fn))

(define (sleep-operation seconds)
  "Make an operation that will succeed with no values when
@var{seconds} have elapsed."
  (define expiry
    (+ (current-jiffy)
       (inexact->exact (round (* seconds (jiffies-per-second))))))
  (timer-operation expiry))

(define (sleep seconds)
  "Block the calling fiber until @var{seconds} have elapsed."
  (perform-operation (sleep-operation seconds)))
