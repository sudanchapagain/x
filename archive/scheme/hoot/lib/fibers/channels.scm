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

(define-module (fibers channels)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (fibers waiter-queue)
  #:use-module (fibers operations)
  #:export (make-channel
            channel?
            put-operation
            get-operation
            put-message
            get-message))

(define-record-type <channel>
  (%make-channel getq putq)
  channel?
  (getq channel-getq)
  (putq channel-putq))

(define (make-channel)
  "Make a fresh channel."
  (%make-channel (make-waiter-queue) (make-waiter-queue)))

(define (put-operation channel message)
  "Make an operation that if and when it completes will rendezvous
with a receiver fiber to send @var{message} over @var{channel}."
  (match channel
    (($ <channel> getq putq)
     (define (try-fn)
       (match (waiter-queue-pop! getq #f)
         (#f #f)
         (resume-get
          (resume-get (lambda () message))
          (lambda () (values)))))
     (define (block-fn state resume-put)
       (waiter-queue-push! putq state (cons resume-put message))
       (values))
     (make-base-operation #f try-fn block-fn))))

(define (get-operation channel)
  "Make an operation that if and when it completes will rendezvous
with a sender fiber to receive one value from @var{channel}."
  (match channel
    (($ <channel> getq putq)
     (define (try-fn)
       (match (waiter-queue-pop! putq #f)
         (#f #f)
         ((resume-put . message)
          (resume-put (lambda () (values)))
          (lambda () message))))
     (define (block-fn state resume-get)
       (waiter-queue-push! getq state resume-get)
       (values))
     (make-base-operation #f try-fn block-fn))))

(define (put-message channel message)
  "Send @var{message} on @var{channel}, and return zero values.  If
there is already another fiber waiting to receive a message on this
channel, give it our message and continue.  Otherwise, block until a
receiver becomes available."
  (perform-operation (put-operation channel message)))

(define (get-message channel)
  "Receive a message from @var{channel} and return it.  If there is
already another fiber waiting to send a message on this channel, take
its message directly.  Otherwise, block until a sender becomes
available."
  (perform-operation (get-operation channel)))
