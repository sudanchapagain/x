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
;;; Finalization tests.
;;;
;;; Code:

(use-modules (hoot finalization)
             (srfi srfi-64)
             (test utils))

(test-begin "test-finalization")

;; Test if our finalization registry emulation is good enough.
(test-equal "basic registration"
  'yay
  (let* ((result #f)
         (r (make-finalization-registry (lambda (x) (set! result x)))))
    (finalization-registry-register! r (list 'foo) 'yay)
    (gc)
    (poll-finalization-registry! r)
    result))

(test-equal "basic unregistration"
  #f
  (let* ((result #f)
         (r (make-finalization-registry (lambda (x) (set! result x)))))
    (finalization-registry-register! r (list 'foo) 'no 'token)
    (finalization-registry-unregister! r 'token)
    (gc)
    (poll-finalization-registry! r)
    result))

(test-equal "registering same value multiple times with same held value"
  '(yay)
  (let* ((result '())
         (r (make-finalization-registry (lambda (x) (set! result (cons x result))))))
    (let ((val (list 'foo)))
      (finalization-registry-register! r val 'yay)
      (finalization-registry-register! r val 'yay)
      (finalization-registry-register! r val 'yay))
    (gc)
    (poll-finalization-registry! r)
    result))

(test-equal "registering same value multiple times with different held values"
  6
  (let* ((result 0)
         (r (make-finalization-registry (lambda (x) (set! result (+ result x))))))
    (let ((val (list 'foo)))
      (finalization-registry-register! r val 1)
      (finalization-registry-register! r val 2)
      (finalization-registry-register! r val 3))
    (gc)
    (poll-finalization-registry! r)
    result))

(test-equal "registering same value multiple times with different held values and unregister tokens"
  4
  (let* ((result 0)
         (r (make-finalization-registry (lambda (x) (set! result (+ result x))))))
    (let ((val (list 'foo)))
      (finalization-registry-register! r val 1 'foo)
      (finalization-registry-register! r val 2 'bar)
      (finalization-registry-register! r val 3 'baz))
    (finalization-registry-unregister! r 'bar)
    (gc)
    (poll-finalization-registry! r)
    result))

(test-equal "registering different values with the same unregister token"
  3
  (let* ((result 0)
         (r (make-finalization-registry (lambda (x) (set! result (+ result x))))))
    (let ((val (list 'foo)))
      (finalization-registry-register! r val 1 'foo)
      (finalization-registry-register! r val 2 'foo)
      (finalization-registry-register! r val 3 'bar))
    (finalization-registry-unregister! r 'foo)
    (gc)
    (poll-finalization-registry! r)
    result))

(test-assert "unregistering with unused token returns #f"
  (not (let ((r (make-finalization-registry (lambda (x) x))))
         (finalization-registry-unregister! r 'nope))))

(test-assert "unregistering with used token returns #t"
  (let ((r (make-finalization-registry (lambda (x) x))))
    (finalization-registry-register! r (list 'foo) 'hey 'cancel)
    (finalization-registry-unregister! r 'cancel)))

;; These tests just verify that the wasm bindings work.  We cannot
;; manually invoke GC on the web to test that the finalization
;; callback is being called properly.
(with-additional-imports ((hoot finalization))
  (test-call "#t"
             (lambda ()
               (finalization-registry?
                (make-finalization-registry (lambda (x) x)))))
  (test-call "#t"
             (lambda ()
               (let ((r (make-finalization-registry (lambda (x) x))))
                 (finalization-registry-register! r (list 'foo) 'cool)
                 #t)))
  (test-call "(foo)\n#t"
             (lambda ()
               (let ((r (make-finalization-registry (lambda (x) x)))
                     (val (list 'foo)))
                 (finalization-registry-register! r val 'hey 'cancel)
                 ;; Return val to ensure it's live for the duration of
                 ;; the test.
                 (values val (finalization-registry-unregister! r 'cancel))))))

(test-end* "test-finalization")
