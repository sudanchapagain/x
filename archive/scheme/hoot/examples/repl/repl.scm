;;; Hoot REPL example
;;; Copyright (C) 2025 David Thompson <dave@spritely.institute>
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
;;; A barebones REPL to demonstrate how to use the interpreter.  Input
;;; handling is *very* simplistic; far from a readline quality
;;; experience.
;;;
;;; Code:

(use-modules (fibers promises)
             (fibers streams)
             (hoot error-handling)
             (hoot control)
             (hoot eval)
             (hoot exceptions)
             (hoot interaction-environment)
             (ice-9 match))

(define env (interaction-environment))

(define (run-repl in out)
  (define (call-with-error-handling thunk)
    (define tag (make-prompt-tag))
    (call-with-prompt
     tag
     (lambda ()
       ;; This 5 number is determined empirically to trim the frames
       ;; within `with-exception-handler`.  Terrible!
       (define outer (+ (stack-height) 5))
       (with-exception-handler
        (lambda (exn)
          ;; Same, this 2 number is an empirical fudge.  Woooo
          (define stack (capture-stack (stack-height)))
          (define inner (max (- (vector-length stack) 2) 0))
          (define trimmed (vector-copy stack (min outer inner) inner))
          (define port (current-error-port))
          (define origin
            (and (exception-with-origin? exn) (exception-origin exn)))
          (call-with-values (lambda ()
                              (if (exception-with-source? exn)
                                  (values (exception-source-file exn)
                                          (exception-source-line exn)
                                          (exception-source-column exn))
                                  (values #f #f #f)))
            (lambda (file line column)
              (print-backtrace trimmed origin file line column port port)
              (display "\nUncaught exception:\n" port)
              (format-exception exn port)
              (newline port)
              (force-output port)
              (abort-to-prompt tag))))
        thunk
        #:unwind? #f))
     (lambda (k)
       (values))))
  (define (display-prompt)
    (newline out)
    (display "> " out)
    (force-output out))
  (define (eval* exp)
    (call-with-error-handling
     (lambda ()
       (eval exp env))))
  (define (print . vals)
    (for-each (match-lambda
                ((? unspecified?)
                 (values))
                (val
                 (newline out)
                 (display "=> " out)
                 (display val out)))
              vals))
  (display "Welcome to the Hoot REPL!\n\n" out)
  (display "Press Ctrl-D to quit.\n" out)
  (let loop ()
    (display-prompt)
    (match (peek-char in)
      ((? eof-object?)
       (values))
      (_
       (let ((exp (read in)))
         (call-with-values (lambda () (eval* exp)) print)
         (loop))))))

(lambda (resolved rejected)
  (call-with-async-result
   resolved rejected
   (lambda ()
     (parameterize ((current-input-port (standard-input-stream))
                    (current-output-port (standard-output-stream)))
       (run-repl (current-input-port) (current-output-port))))))
