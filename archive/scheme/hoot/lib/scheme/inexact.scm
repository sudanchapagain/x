;;; R7RS (scheme inexact) library
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
;;; R7RS (scheme inexact) implementation
;;;
;;; Code:

(library (scheme inexact)
  (export sin cos tan
          asin acos atan
          log exp
          infinite? finite? nan?
          sqrt)
  (import (rename (hoot numbers)
                  (infinite? %infinite?)
                  (finite? %finite?)
                  (nan? %nan?))
          (hoot syntax))

  (define (finite? x)
    (if (real? x)
        (%finite? x)
        (and (%finite? (real-part x))
             (%finite? (imag-part x)))))

  (define (infinite? x)
    (if (real? x)
        (%infinite? x)
        (or (%infinite? (real-part x))
            (%infinite? (imag-part x)))))

  (define (nan? x)
    (if (real? x)
        (%nan? x)
        (or (%nan? (real-part x))
            (%nan? (imag-part x))))))
