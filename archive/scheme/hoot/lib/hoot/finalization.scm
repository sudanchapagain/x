;;; Hoot finalization
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
;;; Host finalization registry bindings.
;;;
;;; Code:

(library (hoot finalization)
  (export make-finalization-registry
          finalization-registry?
          finalization-registry-register!
          finalization-registry-unregister!)
  (import (hoot cond-expand)
          (hoot eq)
          (hoot ffi)
          (hoot primitives))

  (define-foreign %make-finalization-registry
    "finalization" "make_finalization_registry"
    (ref extern) -> (ref extern))
  (define-foreign %finalization-registry-register
    "finalization" "finalization_registry_register"
    (ref extern) (ref eq) (ref eq) -> none)
  (define-foreign %finalization-registry-register/token
    "finalization" "finalization_registry_register_with_token"
    (ref extern) (ref eq) (ref eq) (ref eq) -> none)
  (define-foreign %finalization-registry-unregister
    "finalization" "finalization_registry_unregister"
    (ref extern) (ref eq) -> i32)

  (define-external-type <finalization-registry>
    finalization-registry?
    wrap-finalization-registry
    unwrap-finalization-registry)

  ;; Registries created at expansion time do absolutely nothing, but
  ;; this allows for creating them at the module top-level.
  (define (make-finalization-registry cleanup)
    (wrap-finalization-registry
     (cond-expand
      (guile-vm #f)
      (hoot
       (%make-finalization-registry
        (procedure->external cleanup))))))

  (define* (finalization-registry-register! registry target held-value
                                            #:optional unregister-token)
    (cond-expand
     (guile-vm (values))
     (hoot
      (let ((%registry (unwrap-finalization-registry registry)))
        (if unregister-token
            (%finalization-registry-register/token %registry target
                                                   held-value unregister-token)
            (%finalization-registry-register %registry target held-value))))))

  (define (finalization-registry-unregister! registry unregister-token)
    (cond-expand
     (guile #f)
     (hoot
      (let ((%registry (unwrap-finalization-registry registry)))
        (eq? (%finalization-registry-unregister %registry unregister-token)
             1))))))
