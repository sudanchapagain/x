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

;;; A port of the Concurrent ML implementation from
;;; https://github.com/wingo/fibers and
;;; https://github.com/snabbco/snabb/blob/master/src/lib/fibers/op.lua.
;;; Unlike the CML in Guile's Fibers, this implementation is not
;;; parallel, so it can be much more simple, and it relies on a default
;;; prompt handler being in place instead of an explicit run-fibers.
;;;
;;; Unlike the CML in Snabb's fibers, this implementation handles
;;; multiple values.
;;;

(define-module (fibers operations)
  #:use-module (hoot boxes)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (fibers scheduler)
  #:export (op-state-completed?
            op-state-complete!

            wrap-operation
            choice-operation
            perform-operation

            make-base-operation))

;; Two possible values: #f (waiting), or #t (completed).
(define (make-op-state) (make-box #f))
(define (op-state-completed? state) (box-ref state))
(define (op-state-complete! state)
  (let ((prev (op-state-completed? state)))
    (box-set! state #t)
    (not prev)))

(define-record-type <base-op>
  (make-base-operation wrap-fn try-fn block-fn)
  base-op?
  ;; ((arg ...) -> (result ...)) | #f
  (wrap-fn base-op-wrap-fn)
  ;; () -> (thunk | #f)
  (try-fn base-op-try-fn)
  ;; (op-state resume-k) -> ()
  (block-fn base-op-block-fn))

(define-record-type <choice-op>
  (make-choice-operation base-ops)
  choice-op?
  (base-ops choice-op-base-ops))

(define (wrap-operation op f)
  "Given the operation @var{op}, return a new operation that, if and
when it succeeds, will apply @var{f} to the values yielded by
performing @var{op}, and yield the result as the values of the wrapped
operation."
  (match op
    (($ <base-op> wrap-fn try-fn block-fn)
     (make-base-operation (match wrap-fn
                            (#f f)
                            (_ (lambda args
                                 (call-with-values (lambda ()
                                                     (apply wrap-fn args))
                                   f))))
                          try-fn
                          block-fn))
    (($ <choice-op> base-ops)
     (let* ((count (vector-length base-ops))
            (base-ops* (make-vector count)))
       (let lp ((i 0))
         (when (< i count)
           (vector-set! base-ops* i (wrap-operation (vector-ref base-ops i) f))
           (lp (1+ i))))
       (make-choice-operation base-ops*)))))

(define (choice-operation . ops)
  "Given the operations @var{ops}, return a new operation that if it
succeeds, will succeed with one and only one of the sub-operations
@var{ops}."
  (define (flatten ops)
    (match ops
      (() '())
      ((op . ops)
       (append (match op
                 (($ <base-op>) (list op))
                 (($ <choice-op> base-ops) (vector->list base-ops)))
               (flatten ops)))))
  (match (flatten ops)
    ((base-op) base-op)
    (base-ops (make-choice-operation (list->vector base-ops)))))

(define (random n)
  ;; FIXME!!!
  0)

(define (perform-operation op)
  "Perform the operation @var{op} and return the resulting values.  If
the operation cannot complete directly, block until it can complete."
  (define (wrap-resume resume wrap-fn)
    (if wrap-fn
        (lambda (thunk)
          (resume (lambda ()
                    (call-with-values thunk wrap-fn))))
        resume))

  (define (block resume)
    (let ((state (make-op-state)))
      (match op
        (($ <base-op> wrap-fn try-fn block-fn)
         (block-fn state (wrap-resume resume wrap-fn)))
        (($ <choice-op> base-ops)
         (let lp ((i 0))
           (when (< i (vector-length base-ops))
             (match (vector-ref base-ops i)
               (($ <base-op> wrap-fn try-fn block-fn)
                (block-fn state (wrap-resume resume wrap-fn))))
             (lp (1+ i))))))))

  (define (suspend)
    ((suspend-current-task
      (lambda (k)
        (define (resume thunk)
          (schedule-task (lambda () (k thunk))))
        (block resume)))))

  ;; First, try to sync on an op.  If no op syncs, block.
  (match op
    (($ <base-op> wrap-fn try-fn)
     (match (try-fn)
       (#f (suspend))
       (thunk
        (if wrap-fn
            (call-with-values thunk wrap-fn)
            (thunk)))))
    (($ <choice-op> base-ops)
     (let* ((count (vector-length base-ops))
            (offset (random count)))
       (let lp ((i 0))
         (if (< i count)
             (match (vector-ref base-ops (modulo (+ i offset) count))
               (($ <base-op> wrap-fn try-fn)
                (match (try-fn)
                  (#f (lp (1+ i)))
                  (thunk
                   (if wrap-fn
                       (call-with-values thunk wrap-fn)
                       (thunk))))))
             (suspend)))))))
