;;; Modules
;;; Copyright (C) 2024, 2025 Igalia, S.L.
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
;;; Run-time representation of module trees.
;;;
;;; Code:

(library (hoot hackable)
  (export boot-module-tree
          declare-module!)
  (import (hoot core-syntax)
          (hoot primitives-module)
          (hoot modules)
          (hoot values)
          (hoot lists)
          (hoot expander)
          (hoot eq)
          (ice-9 match))

  (define (boot-module-tree)
    (define root (make-empty-module))
    (the-root-module root)
    (initialize-core-syntax! (submodule-define! root '(hoot core-syntax)))
    (initialize-primitives! (submodule-define! root '(hoot primitives)))
    root)

  (define (declare-module! root modname imports def-names def-vars
                           exports)
    (let ((mod (submodule-define! root modname)))
      (for-each (match-lambda
                  ((import-modname import-names export-names)
                   (let ((import-mod (resolve-module root import-modname)))
                     (for-each
                      (lambda (import-name export-name)
                        (module-import! mod import-mod import-name export-name))
                      import-names export-names))))
                imports)
      (for-each (lambda (name var)
                  (module-add! mod name var))
                def-names def-vars)
      (for-each (match-lambda
                  ((local-name . exported-name)
                   (module-export! mod exported-name local-name)))
                exports))
    (values)))
