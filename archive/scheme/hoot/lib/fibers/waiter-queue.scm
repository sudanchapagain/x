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

(define-module (fibers waiter-queue)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (fibers operations)
  #:export (make-waiter-queue
            waiter-queue-push!
            waiter-queue-pop!
            waiter-queue-pop-all!))

(define-record-type <waiter-queue>
  (%make-waiter-queue head)
  waiter-queue?
  (head waiter-queue-head set-waiter-queue-head!))

(define (make-waiter-queue) (%make-waiter-queue '()))

;; Push an item on the back of the queue, removing any entries for
;; completed operations.
(define (waiter-queue-push! q op-state data)
  (match q
    (($ <waiter-queue> head)
     (let ((new-tail (acons op-state data '())))
       (let drop-head ((head head))
         (match head
           (()
            ;; New tail is the only entry on the queue.
            (set-waiter-queue-head! q new-tail)
            (values))
           ((((? op-state-completed?) . _) . head*)
            ;; Queue head is completed already; pop it off.
            (drop-head head*))
           ((_ . tail)
            ;; Found a pending waiter on the queue.  Filter out any
            ;; other completed operations and tack the new tail on the
            ;; back.
            (set-waiter-queue-head! q head)
            (let filter-tail ((prev head) (tail tail))
              (match tail
                (()
                 (set-cdr! prev new-tail)
                 (values))
                ((((? op-state-completed?) . _) . tail*)
                 (set-cdr! prev tail*)
                 (filter-tail prev tail*))
                ((_ . tail*)
                 (filter-tail tail tail*)))))))))))

(define* (waiter-queue-pop! q #:optional empty)
  (match (waiter-queue-head q)
    (() empty)
    (((op-state . data) . tail)
     (set-waiter-queue-head! q tail)
     (if (op-state-complete! op-state)
         data
         (waiter-queue-pop! q empty)))))

(define (waiter-queue-pop-all! q proc)
  (let ((elts (waiter-queue-head q)))
    (set-waiter-queue-head! q '())
    (for-each (match-lambda
                ((op-state . data)
                 (when (op-state-complete! op-state)
                   (proc data))))
              elts)
    (values)))
