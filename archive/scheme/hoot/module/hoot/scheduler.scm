;;; Scheduler
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
;;; A simple scheduler for testing fibers.
;;;
;;; Code:

(define-module (hoot scheduler)
  #:use-module (hoot binary-heap)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-scheduler
            scheduler?
            scheduler-empty?
            scheduler-run!
            scheduler-delay!
            scheduler-clear!
            scheduler-tick!))

(define-record-type <scheduler>
  (%make-scheduler current-jiffy run-queue delayed-tasks)
  scheduler?
  (current-jiffy scheduler-current-jiffy)
  (run-queue scheduler-run-queue)
  (delayed-tasks scheduler-delayed-tasks))

(define (print-scheduler scheduler port)
  (format port "#<scheduler ~a>" (object-address scheduler)))

(set-record-type-printer! <scheduler> print-scheduler)

;; Tasks are stored as (time . thunk) pairs.
(define (task<? a b)
  (< (car a) (car b)))

(define (make-scheduler current-jiffy)
  (%make-scheduler current-jiffy (make-q) (make-heap task<?)))

(define (scheduler-empty? scheduler)
  "Return #t if there are no tasks currently in @var{scheduler}."
  (and (heap-empty? (scheduler-delayed-tasks scheduler))
       (q-empty? (scheduler-run-queue scheduler))))

(define (scheduler-run! scheduler thunk)
  "Schedule @var{thunk} to be applied before the end of the current turn of
@var{scheduler}."
  (enq! (scheduler-run-queue scheduler) thunk))

(define (scheduler-delay! scheduler thunk delay)
  "Schedule @var{thunk} to be applied after @var{delay} jiffies have
passed."
  (match scheduler
    (($ <scheduler> current-jiffy _ tasks)
     (heap-insert! tasks (cons (+ (current-jiffy) delay) thunk)))))

(define (scheduler-clear! scheduler)
  "Clear all tasks from @var{scheduler}."
  (let ((q (scheduler-run-queue scheduler)))
    (let lp ()
      (unless (q-empty? q)
        (deq! q)
        (lp)))
    (heap-clear! (scheduler-delayed-tasks scheduler))))

(define (scheduler-tick! scheduler)
  "Run delayed tasks in @var{scheduler} whose timeout has passed and then
run all tasks in the run queue."
  (match scheduler
    (($ <scheduler> current-jiffy q tasks)
     (let ((time (current-jiffy)))
       ;; Run all tasks whose time has come.
       (let lp ()
         (unless (heap-empty? tasks)
           (match (heap-min tasks)
             ((task-time . thunk)
              (when (<= task-time time)
                (heap-remove! tasks)
                (thunk)
                (lp))))))
       ;; Flush the run queue.
       (let lp ()
         (unless (q-empty? q)
           ((deq! q))
           (lp)))))))
