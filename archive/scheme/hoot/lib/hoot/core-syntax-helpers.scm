;;; Syntax helper procedures
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
;;; Primitive syntax.
;;;
;;; Code:

(library (hoot core-syntax-helpers)
  (export make-variable-transformer
          identifier? generate-temporaries
          free-identifier=? bound-identifier=?
          syntax-local-binding syntax-violation
          %initialize-syntax-helpers!)
  (import (hoot core-syntax)
          (hoot cross-compilation)
          (prefix (only (hoot primitives)
                        make-variable-transformer
                        identifier? generate-temporaries
                        free-identifier=? bound-identifier=?
                        syntax-local-binding syntax-violation)
                  host:))

  (define-syntax-rule (define-syntax-helper name host-name)
    (define name (cross-compilation-case (#t host-name) (#f #f))))

  (define-syntax-rule (define-syntax-helpers initialize!
                        (name host-name) ...)
    (begin
      (define-syntax-helper name host-name)
      ...
      (define (do-init host-name ...)
        (set! name host-name) ...)
      (define* (initialize! #:key name ...)
        (do-init name ...))))

  (define-syntax-helpers %initialize-syntax-helpers!
    (make-variable-transformer host:make-variable-transformer)
    (identifier? host:identifier?)
    (generate-temporaries host:generate-temporaries)
    (free-identifier=? host:free-identifier=?)
    (bound-identifier=? host:bound-identifier=?)
    (syntax-local-binding host:syntax-local-binding)
    (syntax-violation host:syntax-violation)))
