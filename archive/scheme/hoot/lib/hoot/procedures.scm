;;; Procedures on procedures
;;; Copyright (C) 2023, 2024, 2025 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
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
;;; procedure?
;;;
;;; Code:

(library (hoot procedures)
  (export procedure?
          procedure-name
          procedure-property
          set-procedure-property!)
  (import (rename (only (hoot primitives)
                        %procedure?
                        procedure-property
                        %struct?
                        %struct-vtable
                        %vector-length
                        %vector-ref)
                  (procedure-property guile:procedure-property))
          (hoot cond-expand)
          (hoot errors)
          (hoot inline-wasm)
          (hoot syntax)
          (hoot symbols))

  (define (applicable-record? x)
    (and (%struct? x)
         (%inline-wasm
          '(func (param $x (ref $struct)) (result (ref eq))
                 (struct.get $vtable $applicable?
                             (struct.get $struct $vtable (local.get $x))))
          x)))

  (define (procedure? x)
    (or (%procedure? x) (applicable-record? x)))

  (cond-expand
   (guile-vm
    (define (procedure-property proc prop)
      (check-type proc procedure? 'procedure-property)
      (guile:procedure-property proc prop)))
   (hoot
    (define (%procedure-name proc)
      (cond
       ((%procedure? proc)
        (%inline-wasm
         '(func (param $proc (ref $proc)) (result (ref eq))
                (local $maybe-string (ref null string))
                (call $code-name (struct.get $proc $func (local.get $proc)))
                (local.set $maybe-string)
                (if (ref eq)
                    (ref.is_null (local.get $maybe-string))
                    (then (ref.i31 (i32.const 1)))
                    (else
                     (call $string->symbol
                           (struct.new $string (i32.const 0)
                                       (ref.as_non_null
                                        (local.get $maybe-string)))))))
         proc))
       ((applicable-record? proc)
        (procedure-name
         (%inline-wasm
          '(func (param $struct (ref $struct/1)) (result (ref eq))
                 (struct.get $struct/1 $field0
                             (local.get $struct)))
          proc)))
       (else
        (raise (make-type-error proc 'procedure-name 'procedure?)))))
    (define (procedure-property proc prop)
      (check-type proc procedure? 'procedure-property)
      ;; FIXME: Wire up to (call $code-properties).
      (case prop
        ((name) (%procedure-name proc))
        (else #f)))))

  (define (procedure-name proc)
    (check-type proc procedure? 'procedure-name)
    (procedure-property proc 'name))

  (define (set-procedure-property! proc name val)
    (check-type proc procedure? 'set-procedure-property!)
    (check-type name symbol? 'set-procedure-property!)
    (raise (make-unimplemented-error 'set-procedure-property!))))
