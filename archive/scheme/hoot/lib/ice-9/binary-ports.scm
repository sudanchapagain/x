;;; Guile binary ports
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
;;; Guile binary ports module.
;;;
;;; Code:

(define-module (ice-9 binary-ports)
  #:use-module ((hoot errors) #:select (make-unimplemented-error raise))
  #:use-module ((hoot ports) #:select (get-output-bytevector
                                       open-input-bytevector
                                       open-output-bytevector
                                       peek-u8
                                       read-bytevector
                                       read-bytevector!
                                       read-u8
                                       write-u8
                                       write-bytevector))
  #:use-module (rnrs bytevectors)
  #:re-export (eof-object)
  #:export (open-bytevector-input-port
            open-bytevector-output-port
            get-u8
            lookahead-u8
            get-bytevector-n
            get-bytevector-n!
            get-bytevector-some
            get-bytevector-some!
            get-bytevector-all
            get-string-n!
            put-u8
            put-bytevector
            unget-bytevector
            make-custom-binary-input-port
            make-custom-binary-output-port
            make-custom-binary-input/output-port
            call-with-input-bytevector
            call-with-output-bytevector))

(define* (open-bytevector-input-port bv #:optional transcoder)
  "Return an input port whose contents are drawn from bytevector
@var{bv}."
  (open-input-bytevector bv))

(define (call-with-input-bytevector bv proc)
  "Call the one-argument procedure @var{proc} with a newly created
binary input port from which the bytevector @var{bv}'s contents may be
read.  All values yielded by @var{proc} are returned."
  (proc (open-input-bytevector bv)))

(define* (open-bytevector-output-port #:optional transcoder)
  "Return two values: an output port and a procedure that returns a
bytevector containing all the output written to that port.."
  (let ((port (open-output-bytevector)))
    (values port (lambda () (get-output-bytevector port)))))

(define (call-with-output-bytevector proc)
  "Call the one-argument procedure @var{proc} with a newly created
binary output port.  When the function returns, port is closed and the
bytevector composed of the bytes written into the port is returned."
  (let ((port (open-output-bytevector)))
    (proc port)
    (let ((bv (get-output-bytevector port)))
      (close-port port)
      bv)))

(define (get-u8 port)
  "Read an octet from @var{port}, a binary input port, blocking as
necessary."
  (read-u8 port))

(define (lookahead-u8 port)
  "Like @code{get-u8} but does not update @var{port} to point past the
octet."
  (peek-u8 port))

(define (get-bytevector-n port count)
  "Read @var{count} octets from @var{port}, blocking as necessary and
return a bytevector containing the octets read.  If fewer bytes are
available, a bytevector smaller than @var{count} is returned."
  (read-bytevector count port))

(define (get-bytevector-n! port bv start count)
  "Read @var{count} bytes from @var{port} and store them in @var{bv}
starting at index @var{start}.  Return either the number of bytes
actually read or the end-of-file object."
  (read-bytevector! bv port start (+ start count)))

(define (get-bytevector-some port)
  "Read from @var{port}, blocking as necessary, until bytes are available
or an end-of-file is reached.  Return either the end-of-file object or
a new bytevector containing some of the available bytes (at least
one), and update the port position to point just past these bytes."
  (raise (make-unimplemented-error 'get-bytevector-some)))

(define (get-bytevector-some! port bv start count)
  "Read up to @var{count} bytes from @var{port}, blocking as necessary
until at least one byte is available or an end-of-file is reached.
Store them in @var{bv} starting at index @var{start}.  Return the
number of bytes actually read, or an end-of-file object."
  (raise (make-unimplemented-error 'get-bytevector-some!)))

(define (get-bytevector-all port)
  "Read from @var{port}, blocking as necessary, until
the end-of-file is reached.  Return either a new bytevector containing
the data read or the end-of-file object (if no data were available)."
  (raise (make-unimplemented-error 'get-bytevector-all)))

(define (get-string-n! port str start count)
  "Read up to @var{count} characters from @var{port} into @var{str},
starting at @var{start}.  If no characters can be read before the end
of file is encountered, the end of file object is returned.
Otherwise, the number of characters read is returned."
  (raise (make-unimplemented-error 'get-string-n!)))

(define (put-u8 port octet)
  "Write @var{octet} to binary port @var{port}."
  (write-u8 octet port))

(define* (put-bytevector port bv #:optional (start 0)
                         (count (- (bytevector-length bv) start)))
  "Write the contents of @var{bv} to @var{port}, optionally starting at
index @var{start} and limiting to @var{count} octets."
  (write-bytevector bv port start (+ start count)))

(define* (unget-bytevector port bv #:optional (start 0)
                           (count (- (bytevector-length bv) start)))
  "Unget the contents of @var{bv} to @var{port}, optionally starting at
index @var{start} and limiting to @var{count} octets."
  (raise (make-unimplemented-error 'unget-bytevector)))

(define (make-custom-binary-input-port id read get-position set-position! close)
  (raise (make-unimplemented-error 'make-custom-binary-input-port)))
(define (make-custom-binary-output-port id write get-position set-position! close)
  (raise (make-unimplemented-error 'make-custom-binary-output-port)))
(define (make-custom-binary-input/output-port id read write get-position set-position! close)
  (raise (make-unimplemented-error 'make-custom-binary-input/output-port)))
