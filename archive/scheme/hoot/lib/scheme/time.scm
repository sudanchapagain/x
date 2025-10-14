;;; R7RS (scheme time) library
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

;;; Commentary:
;;;
;;; R7RS (scheme time) implementation
;;;
;;; Code:

(library (scheme time)
  (export jiffies-per-second
          current-jiffy
          current-second)
  (import (hoot syntax)
          (hoot inline-wasm))

  (define (jiffies-per-second)
    (%inline-wasm
     '(func (result i64)
            (i64.extend_i32_u (call $jiffies-per-second)))))

  (define (current-jiffy)
    (%inline-wasm
     '(func (result i64)
            (i64.trunc_f64_u (call $current-jiffy)))))

  (define (current-second)
    (%inline-wasm
     '(func (result f64) (call $current-second)))))
