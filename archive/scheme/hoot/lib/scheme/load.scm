;;; R7RS (scheme load) library
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
;;; R7RS (scheme load) implementation
;;;
;;; Code:

(library (scheme load)
  (export load)
  (import (scheme base)
    (only (hoot errors) make-unimplemented-error)
    (only (hoot syntax) define*))

  (define* (load filename #:optional env)
    (raise (make-unimplemented-error 'load))))
