;;; Streams
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
;;; Streams are host objects that are asynchronous sources or sinks of
;;; data.  This module wraps streams in the Hoot port interface, as
;;; input or output ports.  Although the interface with the host is
;;; somewhat abstract, it is modelled after the WhatWG Streams
;;; specification (https://streams.spec.whatwg.org/).
;;;
;;; The Streams API is somewhat vague as to what constitutes "data".
;;; For our purposes, we assume that each chunk of data is a byte array.
;;; Encoding/decoding text is the host's responsibility, if that is what
;;; is wanted.
;;;
;;; Code:

(define-module (fibers streams)
  #:use-module (hoot ports)
  #:use-module (hoot ffi)
  #:use-module (fibers promises)
  #:use-module (hoot bytevectors)
  #:export (open-input-stream
            open-output-stream

            standard-input-stream
            standard-output-stream
            standard-error-stream))

;; length -> Uint8Array
(define-foreign stream:make-chunk
  "rt" "stream_make_chunk"  i32 -> (ref extern))
;; Uint8Array -> length
(define-foreign stream:chunk-length
  "rt" "stream_chunk_length" (ref extern) -> i32)
;; Uint8Array, idx -> byte
(define-foreign stream:chunk-ref
  "rt" "stream_chunk_ref" (ref extern) i32 -> i32)
;; Uint8Array, idx, byte -> ()
(define-foreign stream:chunk-set!
  "rt" "stream_chunk_set" (ref extern) i32 i32 -> none)

;; ReadableStream -> ReadableStreamDefaultReader
(define-foreign stream:get-reader
  "rt" "stream_get_reader" (ref extern) -> (ref extern))
;; ReadableStreamDefaultReader -> Promise<Result<Uint8Array>>
(define-foreign stream:read
  "rt" "stream_read" (ref extern) -> (ref extern))
;; Result<Uint8Array> -> Uint8Array
(define-foreign stream:result-chunk
  "rt" "stream_result_chunk" (ref extern) -> (ref extern))
;; Result<Uint8Array> -> 1 if done, 0 otherwise
(define-foreign stream:result-done?
  "rt" "stream_result_done" (ref extern) -> i32)

;; WritableStream -> WritableStreamDefaultWriter
(define-foreign stream:get-writer
  "rt" "stream_get_writer" (ref extern) -> (ref extern))
;; WritableStreamDefaultWriter, Uint8Array -> Promise<undefined>
(define-foreign stream:write
  "rt" "stream_write" (ref extern) (ref extern) -> (ref extern))
;; WritableStreamDefaultWriter -> Promise<undefined>
(define-foreign stream:close-writer
  "rt" "stream_close_writer" (ref extern) -> (ref extern))

;; -> ReadableStream
(define-foreign stream:stdin
  "io" "stream_stdin" -> (ref extern))
;; -> WritableStream
(define-foreign stream:stdout
  "io" "stream_stdout" -> (ref extern))
;; -> WritableStream
(define-foreign stream:stderr
  "io" "stream_stderr" -> (ref extern))

(define (open-input-stream stream)
  (define reader (stream:get-reader stream))
  (define did-read 0)
  (define pos 0)
  (define done? #f)
  (define chunk #f)
  (define chunk-len 0)
  (define (read dst start count)
    (cond
     ((eq? pos chunk-len)
      (if done?
          0
          (let ((result (await (stream:read reader))))
            (set! done? (not (zero? (stream:result-done? result))))
            (set! did-read (+ did-read chunk-len))
            (set! chunk (if done? #f (stream:result-chunk result)))
            (set! pos 0)
            (set! chunk-len (if done? 0 (stream:chunk-length chunk)))
            (read dst start count))))
     (else
      (let ((to-copy (min count (- chunk-len pos))))
        (let lp ((i 0))
          (when (< i to-copy)
            (bytevector-u8-set! dst (+ start i)
                                (stream:chunk-ref chunk (+ pos i)))
            (lp (1+ i))))
        (set! pos (+ pos to-copy))
        to-copy))))
  (define (seek offset whence)       ; seek
    (if (and (zero? offset)
             (eq? whence 'cur))
        (+ did-read pos)
        (error "unreachable; stream ports are not seekable")))
  (make-port #:read read
             #:seek seek
             #:repr "stream"))

(define (open-output-stream stream)
  (define writer (stream:get-writer stream))
  (define pos 0)
  (define (write bv start count)
    (unless (zero? count)
      (let ((chunk (stream:make-chunk count)))
        (let lp ((i 0))
          (when (< i count)
            (stream:chunk-set! chunk i (bytevector-u8-ref bv (+ start i)))
            (lp (1+ i))))
        (await (stream:write writer chunk))))
    (set! pos (+ pos count))
    count)
  (define (seek offset whence)
    (if (and (zero? offset)
             (eq? whence 'cur))
        pos
        (error "unreachable; stream ports are not seekable")))
  (define (close)
    (stream:close-writer writer))
  (make-port #:write write
             #:seek seek
             #:close close
             #:repr "stream"))

(define (standard-input-stream)
  (open-input-stream (stream:stdin)))
(define (standard-output-stream)
  (open-output-stream (stream:stdout)))
(define (standard-error-stream)
  (open-output-stream (stream:stderr)))
