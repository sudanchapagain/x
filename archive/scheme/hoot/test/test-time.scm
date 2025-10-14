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

(use-modules (hoot reflect)
             (srfi srfi-64)
             (test utils))

(test-begin "test-time")

(parameterize ((use-d8? #f)
               (use-node? #f))
  (with-additional-imports ((scheme time))
    (call-with-fake-clock
     1000                                 ; jiffies-per-second
     (lambda () 42)                       ; current-jiffy
     (lambda () 69.0)                     ; current-second
     (lambda ()
       (test-call "1000" (lambda () (jiffies-per-second)))
       (test-call "42" (lambda () (current-jiffy)))
       (test-call "69.0" (lambda () (current-second)))))))

(test-end* "test-time")
