;;; Test script to compile a Scheme expression to wasm, then run via V8
;;; Copyright (C) 2023, 2024 Igalia, S.L.
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

(use-modules (wasm assemble)
             (hoot config)
             (hoot compile)
             (ice-9 binary-ports)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-64))

(define (unwind-protect body unwind)
  (call-with-values
      (lambda ()
        (with-exception-handler
         (lambda (exn)
           (unwind)
           (raise-exception exn))
         body))
    (lambda vals
      (unwind)
      (apply values vals))))

(define (call-with-compiled-wasm-file wasm f)
  (let* ((wasm-port (mkstemp "/tmp/tmp-wasm-XXXXXX"))
         (wasm-file-name (port-filename wasm-port)))
    (put-bytevector wasm-port (assemble-wasm wasm))
    (close-port wasm-port)
    (unwind-protect
     (lambda () (f wasm-file-name))
     (lambda () (delete-file wasm-file-name)))))

(define (run-v8 . args)
  (let* ((v8 (or %node %d8))
         (pid (spawn v8 (cons v8 args))))
    (exit (status:exit-val (cdr (waitpid pid))))))

(define (compile-expr expr)
  (call-with-compiled-wasm-file
   (compile expr)
   (lambda (wasm-file-name)
     (define runner (in-vicinity %js-runner-dir "load.js"))
     (run-v8 runner "--" %reflect-js-dir %reflect-wasm-dir wasm-file-name))))

(define (read1 str)
  (call-with-input-string
   str
   (lambda (port)
     (let ((expr (read port)))
       (when (eof-object? expr)
         (error "No expression to evaluate"))
       (let ((tail (read port)))
         (unless (eof-object? tail)
           (error "Unexpected trailing expression" tail)))
       expr))))

(when (batch-mode?)
  (match (program-arguments)
    ((arg0 str)
     (compile-expr (read1 str)))
    ((arg0 . _)
     (format (current-error-port) "usage: ~a EXPR\n" arg0)
     (exit 1))))
