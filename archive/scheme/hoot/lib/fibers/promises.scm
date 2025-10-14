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

(define-module (fibers promises)
  #:use-module (fibers operations)
  #:use-module (hoot ffi)
  #:use-module ((hoot exceptions)
                #:select (make-exception-with-message
                          make-exception-with-origin
                          make-exception-with-irritants
                          define-exception-type))
  #:export (await-promise-operation
            await
            call-with-async-result))

(define-foreign promise:on-completed
  "rt" "promise_on_completed" (ref extern) (ref extern) (ref extern) -> none)
(define-foreign promise:complete!
  "rt" "promise_complete" (ref extern) (ref eq) -> none)

(define-exception-type &promise-failure &error
  make-promise-failure
  promise-failure?)

(define (promise-failure val)
  (make-exception (make-promise-failure)
                  (make-exception-with-message "promise was rejected")
                  (make-exception-with-origin 'await-promise-operation)
                  (make-exception-with-irritants (list val))))

(define (await-promise-operation promise)
  "Make an operation that will complete when @var{promise} is resolved.
Performing the operation produces one value: a thunk which when called
will either return the value or throw an exception."
  (define (try-fn) #f)
  (define (block-fn state resume)
    (promise:on-completed
     promise
     (procedure->external
      (lambda (x)
        (when (op-state-complete! state)
          (resume (lambda () (lambda () x))))))
     (procedure->external
      (lambda (err)
        (when (op-state-complete! state)
          (resume (lambda ()
                    (raise-exception (promise-failure err))))))))
    (values))
  (make-base-operation #f try-fn block-fn))

(define (await promise)
  ((perform-operation (await-promise-operation promise))))

(define (call-with-async-result resolved rejected thunk)
  (with-exception-handler
   (lambda (err)
     (promise:complete! rejected err))
   (lambda ()
     (call-with-values thunk
       (lambda vals
         (promise:complete! resolved vals))))
   #:unwind? #t))
