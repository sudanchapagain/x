;;; R7RS (scheme file) library
;;; Copyright (C) 2024 Igalia, S.L.
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
;;; R7RS (scheme file) implementation
;;;
;;; Code:

(library (scheme file)
  (export open-binary-input-file
          open-binary-output-file
          call-with-input-file
          call-with-output-file
          delete-file
          file-exists?
          open-input-file
          open-output-file
          with-input-from-file
          with-output-to-file)
  (import (scheme base)
          (hoot inline-wasm)
          (only (hoot ports) make-port)
          (only (hoot errors) assert make-unimplemented-error)
          (hoot match))

  (define (delete-file filename)
    (%inline-wasm
     '(func (param $filename (ref string))
            (call $delete-file (local.get $filename)))
     filename)
    (if #f #f))

  (define (file-exists? filename)
    (%inline-wasm
     '(func (param $filename (ref string)) (result (ref eq))
            (if (ref eq)
                (i32.eqz (call $file-exists? (local.get $filename)))
                (then (ref.i31 (i32.const 1)))
                (else (ref.i31 (i32.const 17)))))
     filename))

  (define (%file-random-access? handle)
    (%inline-wasm
     '(func (param $handle (ref $extern-ref)) (result (ref eq))
            (if (ref eq)
                (i32.eqz (call $file-random-access?
                               (ref.as_non_null
                                (struct.get $extern-ref $val
                                            (local.get $handle)))))
                (then (ref.i31 (i32.const 1)))
                (else (ref.i31 (i32.const 17)))))
     handle))

  (define (%file-buffer-size handle)
    (%inline-wasm
     '(func (param $handle (ref $extern-ref)) (result i64)
            (i64.extend_i32_s
             (call $file-buffer-size
                   (ref.as_non_null
                    (struct.get $extern-ref $val (local.get $handle))))))
     handle))

  (define (%close-file handle)
    (%inline-wasm
     '(func (param $handle (ref $extern-ref))
            (call $close-file
                  (ref.as_non_null
                   (struct.get $extern-ref $val (local.get $handle)))))
     handle)
    (if #f #f))

  (define (%seek-file handle offset whence)
    (let* ((whence (match whence ('start 0) ('cur 1) ('end 2)))
           (new (%inline-wasm
                 '(func (param $handle (ref $extern-ref))
                        (param $offset i64)
                        (param $whence i64)
                        (result i64)
                        (i64.extend_i32_s
                         (call $seek-file
                               (ref.as_non_null
                                (struct.get $extern-ref $val (local.get $handle)))
                               (i32.wrap_i64 (local.get $offset))
                               (i32.wrap_i64 (local.get $whence)))))
                 handle offset whence)))
      (assert (>= new 0) 'seek)
      new))

  (define (open-binary-input-file filename)
    (define handle
      (%inline-wasm
       '(func (param $filename (ref string)) (result (ref eq))
              (struct.new $extern-ref
                          (i32.const 0)
                          (call $open-input-file (local.get $filename))))
       filename))
    (define (file-buffer-ref i)
      (%inline-wasm
       '(func (param $handle (ref $extern-ref)) (param $i i64) (result i64)
              (i64.extend_i32_s
               (call $file-buffer-ref
                     (ref.as_non_null
                      (struct.get $extern-ref $val (local.get $handle)))
                     (i32.wrap_i64 (local.get $i)))))
       handle i))
    (define (file-read dst start count)
      (let ((n (%inline-wasm
                '(func (param $handle (ref $extern-ref)) (param $count i64)
                       (result i64)
                       (i64.extend_i32_s
                        (call $read-file
                              (ref.as_non_null
                               (struct.get $extern-ref $val (local.get $handle)))
                              (i32.wrap_i64 (local.get $count)))))
                handle count)))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (bytevector-u8-set! dst (+ start i) (file-buffer-ref i)))
        n))
    (define (file-close) (%close-file handle))
    (define (file-seek offset whence)
      (%seek-file handle offset whence))
    (make-port #:read file-read
               #:seek file-seek
               #:close file-close
               #:repr "file"
               #:file-name filename
               #:r/w-random-access? (%file-random-access? handle)))

  (define (open-binary-output-file filename)
    (define handle
      (%inline-wasm
       '(func (param $filename (ref string)) (result (ref eq))
              (struct.new $extern-ref
                          (i32.const 0)
                          (call $open-output-file (local.get $filename))))
       filename))
    (define handle-buffer-size (%file-buffer-size handle))
    (define (file-buffer-set! i x)
      (%inline-wasm
       '(func (param $handle (ref $extern-ref)) (param $i i64) (param $x i64)
              (call $file-buffer-set!
                    (ref.as_non_null
                     (struct.get $extern-ref $val (local.get $handle)))
                    (i32.wrap_i64 (local.get $i))
                    (i32.wrap_i64 (local.get $x))))
       handle i x))
    (define (file-write bv start count)
      (let ((count (min count handle-buffer-size)))
        (do ((i 0 (+ i 1)))
            ((= i count))
          (file-buffer-set! i (bytevector-u8-ref bv (+ start i))))
        (%inline-wasm
         '(func (param $handle (ref $extern-ref)) (param $count i64) (result i64)
                (i64.extend_i32_s
                 (call $write-file
                       (ref.as_non_null
                        (struct.get $extern-ref $val (local.get $handle)))
                       (i32.wrap_i64 (local.get $count)))))
         handle count)))
    (define (file-close) (%close-file handle))
    (define (file-seek offset whence)
      (%seek-file handle offset whence))
    (make-port #:write file-write
               #:seek file-seek
               #:close file-close
               #:repr "file"
               #:file-name filename
               #:r/w-random-access? (%file-random-access? handle)))

  (define (open-input-file filename)
    (open-binary-input-file filename))
  (define (open-output-file filename)
    (open-binary-output-file filename))

  (define (call-with-input-file filename proc)
    (let ((p (open-input-file filename)))
      (call-with-values (lambda () (proc p))
        (lambda vals
          (close-port p)
          (apply values vals)))))
  (define (call-with-output-file filename proc)
    (let ((p (open-output-file filename)))
      (call-with-values (lambda () (proc p))
        (lambda vals
          (close-port p)
          (apply values vals)))))

  (define (with-input-from-file filename thunk)
    (let ((p (open-input-file filename)))
      (parameterize ((current-input-port p))
        (call-with-values thunk
          (lambda vals
            (close-port p)
            (apply values vals))))))
  (define (with-output-to-file filename thunk)
    (let ((p (open-output-file filename)))
      (parameterize ((current-output-port p))
        (call-with-values thunk
          (lambda vals
            (close-port p)
            (apply values vals)))))))
