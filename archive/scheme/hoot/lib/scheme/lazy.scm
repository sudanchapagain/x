;;; R7RS (scheme lazy) library
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
;;; R7RS (scheme lazy) implementation
;;;
;;; Code:

(library (scheme lazy)
  (export delay force promise? delay-force make-promise)
  (import (except (scheme base) define-record-type)
    (hoot records)
    (hoot match)
    (only (hoot syntax) define-syntax-rule))

  ;; promises
  (define-record-type <promise>
    #:opaque? #t
    (%%make-promise value)
    promise?
    (value %promise-value %set-promise-value!))

  (define (%make-promise eager? val)
    (%%make-promise (cons eager? val)))

  (define (make-promise x)
    (if (promise? x) x (%make-promise #t x)))

  (define (force promise)
    (match (%promise-value promise)
      ((#t . val) val)
      ((#f . thunk)
       (let ((promise* (thunk)))
         (match (%promise-value promise)
           ((and value (#f . _))
            (match (%promise-value promise*)
              ((eager? . data)
               (set-car! value eager?)
               (set-cdr! value data)
               (%set-promise-value! promise* value)
               (force promise))))
           ((#t . val) val))))))

  (define-syntax-rule (delay-force expr)
    (%make-promise #f (lambda () expr)))

  (define-syntax-rule (delay expr)
    (delay-force (%make-promise #t expr))))
