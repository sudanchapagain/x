;;; Guile custom ports
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
;;; Guile custom ports module.
;;;
;;; Code:

(define-module (ice-9 custom-ports)
  #:use-module (hoot ports)
  #:export (make-custom-port))

(define (default-read-wait-fd port) #f)
(define (default-write-wait-fd port) #f)
(define (default-input-waiting? port) #t)
(define (default-get-natural-buffer-sizes port read-buf-size write-buf-size)
  (values read-buf-size write-buf-size))
(define (default-truncate port length)
  (error "custom port did not define a truncate method" port))

(define* (make-custom-port #:key
                           read
                           write
                           (read-wait-fd default-read-wait-fd)
                           (input-waiting? (and read default-input-waiting?))
                           (write-wait-fd default-write-wait-fd)
                           (seek #f)
                           (random-access? #f)
                           (close #f)
                           (get-natural-buffer-sizes default-get-natural-buffer-sizes)
                           (id "custom-port")
                           (print #f) ; TODO
                           (truncate default-truncate)
                           (encoding #f) ; TODO
                           (conversion-strategy #f) ; TODO
                           (close-on-gc? #f)) ; TODO
  ;; FIXME: We aren't calling get-natural-buffer-sizes with the port
  ;; object, because we have to know what the buffer sizes are
  ;; *before* we create the port.
  (define-values (read-buf-size write-buf-size)
    (get-natural-buffer-sizes #f 1024 1024))
  (make-port #:read read
             #:write write
             #:input-waiting? input-waiting?
             #:seek seek
             #:close close
             #:truncate truncate
             #:repr id
             #:read-buffer-size read-buf-size
             #:write-buffer-size write-buf-size
             #:r/w-random-access? random-access?))
