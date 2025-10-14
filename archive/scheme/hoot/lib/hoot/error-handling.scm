;;; Catching errors.
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
;;; with-exception-handler, guard, and all that.
;;;
;;; Code:

(library (hoot error-handling)
  (export guard format-exception capture-stack stack-height print-backtrace)
  (import (hoot cond-expand)
          (hoot pairs)
          (hoot eq)
          (hoot errors)
          (hoot exceptions)
          (hoot fluids)
          (hoot inline-wasm)
          (only (hoot control)
                make-prompt-tag call-with-prompt abort-to-prompt)
          (hoot match)
          (hoot not)
          (hoot numbers)
          (hoot ports)
          (hoot procedures)
          (hoot records)
          (hoot strings)
          (hoot syntax)
          (hoot values)
          (hoot vectors)
          (hoot write))

  ;; Snarfed from Guile's (ice-9 exceptions).  Deviates a bit from R7RS.
  (define-syntax guard
    (lambda (stx)
      (define (dispatch tag exn clauses)
        (define (build-clause test handler clauses)
          #`(let ((t #,test))
              (if t
                  (abort-to-prompt #,tag #,handler t)
                  #,(dispatch tag exn clauses))))
        (syntax-case clauses (=> else)
          (() #`(raise-continuable #,exn))
          (((test => f) . clauses)
           (build-clause #'test #'(lambda (res) (f res)) #'clauses))
          (((else e e* ...) . clauses)
           (build-clause #'#t #'(lambda (res) e e* ...) #'clauses))
          (((test) . clauses)
           (build-clause #'test #'(lambda (res) res) #'clauses))
          (((test e* ...) . clauses)
           (build-clause #'test #'(lambda (res) e* ...) #'clauses))))
      (syntax-case stx ()
        ((guard (exn clause clause* ...) body body* ...)
         (identifier? #'exn)
         #`(let ((tag (make-prompt-tag)))
             (call-with-prompt
              tag
              (lambda ()
                (with-exception-handler
                 (lambda (exn)
                   #,(dispatch #'tag #'exn #'(clause clause* ...)))
                 (lambda () body body* ...)))
              (lambda (_ h v)
                (h v))))))))

  (define (format-exception exception port)
    (display "Scheme error:\n")
    (match (simple-exceptions exception)
      (() (display "Empty exception object" port))
      (components
       (let loop ((i 1) (components components))
         (define (format-numbered-exception exception)
           (display "  " port)
           (display i port)
           (display ". " port)
           (write exception port))
         (match components
           ((component)
            (format-numbered-exception component))
           ((component . rest)
            (format-numbered-exception component)
            (newline port)
            (loop (+ i 1) rest)))))))

  ;; A macro so as to avoid adding any stack frames.
  (define-syntax-rule (stack-height)
    (cond-expand
     (guile-vm
      (raise (make-unimplemented-error 'stack-height)))
     (hoot
      (%inline-wasm
       '(func (result (ref eq))
              (call $i32->scm (global.get $ret-sp)))))))

  (define (capture-stack height)
    (cond-expand
     (guile-vm
      (raise (make-unimplemented-error 'capture-stack)))
     (hoot
      (define (stack-ref n)
        (%inline-wasm
         '(func (param $n i32)
                (result (ref eq))
                (struct.new $code-ref (i32.const 0)
                            (ref.as_non_null
                             (table.get $ret-stack (local.get $n)))))
         n))
      (define stack (make-vector (min height (stack-height)) #f))
      (let lp ((i 0))
        (when (< i (vector-length stack))
          (vector-set! stack i (stack-ref i))
          (lp (1+ i))))
      stack)))

  (define* (print-backtrace stack origin file line column port #:optional
                            (end (vector-length stack)))
    (cond-expand
     (guile-vm
      (raise (make-unimplemented-error 'print-backtrace)))
     (hoot
      (define (code-source code)
        (%inline-wasm
         '(func (param $code (ref func))
                (result (ref eq) (ref eq) (ref eq))
                (local $maybe-string (ref null string))
                (local $i1 i32)
                (local $i2 i32)
                (call $code-source (local.get $code))
                (local.set $i2)
                (local.set $i1)
                (local.set $maybe-string)
                (if (ref eq)
                    (ref.is_null (local.get $maybe-string))
                    (then (ref.i31 (i32.const 1)))
                    (else (struct.new $string (i32.const 0)
                                      (ref.as_non_null (local.get $maybe-string)))))
                (call $i32->scm (local.get $i1))
                (call $i32->scm (local.get $i2)))
         code))
      (define (code-name code)
        (%inline-wasm
         '(func (param $code (ref func)) (result (ref eq))
                (local $maybe-string (ref null string))
                (call $code-name (local.get $code))
                (local.set $maybe-string)
                (if (ref eq)
                    (ref.is_null (local.get $maybe-string))
                    (then (ref.i31 (i32.const 1)))
                    (else (struct.new $string (i32.const 0)
                                      (ref.as_non_null (local.get $maybe-string))))))
         code))
      (define (print-file file)
        (match file
          (#f
           (write-string "In unknown file:\n" port))
          (file
           (write-string "In " port)
           (write-string file port)
           (write-string ":\n" port)))
        (flush-output-port port))
      (define (print-frame line col idx proc-name)
        (define (left-pad str size)
          (if (< (string-length str) size)
              (string-append (make-string (- size (string-length str))
                                          #\space)
                             str)
              str))
        (define (right-pad str size)
          (if (< (string-length str) size)
              (string-append str
                             (make-string (- size (string-length str))
                                          #\space))
              str))
        (cond
         ((and line col)
          (write-string (string-append
                         (left-pad (number->string line) 6)
                         ":"
                         (right-pad (number->string col) 3)
                         " ")
                        port))
         (else
          (write-string "           " port)))
        (write-string (left-pad (number->string idx) 3) port)
        (write-string " (" port)
        (write (or proc-name "_") port)
        (write-string " …)" port)
        (newline port)
        (flush-output-port port))
      (write-string "Hoot backtrace:\n" port)
      (define (same-files? a b)
        (if a
            (and b (string=? a b))
            (not b)))
      (let lp ((i 0) (previous-file #f))
        (cond
         ((< i (vector-length stack))
          (call-with-values (lambda () (code-source (vector-ref stack i)))
            (lambda (file line col)
              (define name (code-name (vector-ref stack i)))
              (unless (and (not (zero? i))
                           (same-files? file previous-file))
                (print-file file))
              (if (zero? line)
                  (print-frame #f #f (- (vector-length stack) i) name)
                  (print-frame line col (- (vector-length stack) i) name))
              (lp (1+ i) file))))
         (else
          (unless (same-files? file previous-file)
            (print-file file))
          (print-frame line column 0 origin)))))))

  (cond-expand
   (guile-vm)
   (hoot-main
    (let ()
      (define %exception-handler (make-fluid #f))
      (define (fluid-ref* fluid depth)
        (%inline-wasm
         '(func (param $fluid (ref $fluid)) (param $depth i32)
                (result (ref eq))
                (call $fluid-ref* (local.get $fluid) (local.get $depth)))
         fluid depth))

      (define* (with-exception-handler handler thunk #:key (unwind? #f) (unwind-for-type #t))
        (check-type handler procedure? 'with-exception-handler)
        (cond
         (unwind?
          (let ((tag (make-prompt-tag "exception handler")))
            (call-with-prompt
                tag
              (lambda ()
                (with-fluids ((%exception-handler (cons unwind-for-type tag)))
                  (thunk)))
              (lambda (k exn)
                (handler exn)))))
         (else
          (let ((running? (make-fluid #f)))
            (with-fluids ((%exception-handler (cons running? handler)))
              (thunk))))))

      (define (raise-non-continuable-exception)
        (raise (make-exception (make-non-continuable-violation)
                               (make-exception-with-message
                                "unhandled non-continuable exception"))))

      (define (fallback-exception-handler exn captured-height)
        (define stack (capture-stack captured-height))
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
            (print-backtrace stack origin file line column port port)
            (write-string "\nUncaught exception:\n" port)
            (format-exception exn port)
            (newline port)
            (flush-output-port port)
            (%inline-wasm
             '(func (param $status i32)
                    (call $quit (local.get $status))
                    (unreachable))
             1))))

      (define* (raise-exception exn #:key continuable?)
        (define captured-stack-height (stack-height))
        (define (is-a? x type)
          (let ((vtable (%inline-wasm
                         '(func (param $x (ref $struct)) (result (ref eq))
                                (ref.as_non_null
                                 (struct.get $struct $vtable (local.get $x))))
                         x)))
            (or (eq? vtable type)
                (let ((parents (record-type-parents vtable)))
                  (let lp ((i 0))
                    (and (< i (vector-length parents))
                         (or (eq? (vector-ref parents i) type)
                             (lp (1+ i)))))))))
        (define (exception-has-type? exn type)
          (cond
           ((eq? type #t) #t)
           ((exception? exn)
            (or (is-a? exn type)
                (and (compound-exception? exn)
                     (let lp ((simple (compound-exception-components exn)))
                       (match simple
                         (() #f)
                         ((x . rest)
                          (or (is-a? x type) (lp rest))))))))
           (else #f)))
        (let lp ((depth 0))
          ;; FIXME: fluid-ref* takes time proportional to depth, which
          ;; makes this loop quadratic.
          (match (fluid-ref* %exception-handler depth)
            (#f
             (fallback-exception-handler exn captured-stack-height))
            (((? fluid? running?) . handler)
             (if (fluid-ref running?)
                 (lp (1+ depth))
                 (with-fluids ((running? #t))
                   (cond
                    (continuable?
                     (handler exn))
                    (else
                     (handler exn)
                     (raise-non-continuable-exception))))))
            ((type . prompt-tag)
             (cond
              ((exception-has-type? exn type)
               ;; Due to dynamic state saving and restoration, it is
               ;; possible that the prompt is not on the dynamic
               ;; stack. This causes abort-to-prompt to throw an
               ;; exception of its own.  That exception would be
               ;; handled by aborting to the same prompt again,
               ;; resulting in an unbounded loop.
               (with-exception-handler (lambda (exn) (lp (1+ depth)))
                 (lambda () (abort-to-prompt prompt-tag exn))
                 #:unwind? #t)
               (raise-non-continuable-exception))
              (else
               (lp (1+ depth))))))))

      (define-syntax-rule (initialize-globals (global type proc) ...)
        (%inline-wasm
         '(func (param global type) ...
                (global.set global (local.get global)) ...)
         proc ...))
      (define-syntax-rule (initialize-proc-globals (global proc) ...)
        (initialize-globals (global (ref $proc) proc) ...))
      (initialize-proc-globals
       ($with-exception-handler with-exception-handler)
       ($raise-exception raise-exception))))
   (hoot-aux)))
