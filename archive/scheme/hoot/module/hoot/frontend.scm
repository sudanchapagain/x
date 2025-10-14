;;; WebAssembly compiler
;;; Copyright (C) 2023, 2024 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Scheme to WebAssembly compiler.
;;;
;;; Code:

(define-module (hoot frontend)
  #:use-module (hoot config)
  #:export (hoot-load-path
            hoot-system-load-path
            hoot-load-extensions))

(define hoot-load-path
  (make-parameter
   (append (parse-path (getenv "HOOT_LOAD_PATH"))
           ;; Add Guile's source modules to the load path so things
           ;; like SRFIs can be imported without having to copy them
           ;; into Hoot as system modules.
           (list %guile-library-dir))))

(define hoot-system-load-path
  (lambda () %stdlib-path))

(define hoot-load-extensions
  (make-parameter %load-extensions))
