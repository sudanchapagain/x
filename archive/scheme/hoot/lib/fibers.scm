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

(define-module (fibers)
  #:use-module (fibers scheduler)
  #:use-module (fibers timers)
  #:export (spawn-fiber)
  #:re-export (sleep))

(define (spawn-fiber thunk)
  "Spawn a new fiber which will start by invoking @var{thunk}.
The fiber will be scheduled on the next turn.  @var{thunk} will run
with a copy of the current dynamic state, isolating fluid and
parameter mutations to the fiber."
  (define (capture-dynamic-state thunk)
    (let ((dynamic-state (current-dynamic-state)))
      (lambda ()
        (with-dynamic-state dynamic-state thunk))))
  (schedule-task (capture-dynamic-state thunk)))
