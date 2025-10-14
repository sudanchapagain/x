;;; Copyright (C) 2023, 2024, 2025 Igalia, S.L.
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
;;; Test utilities.
;;;
;;; Code:

(define-module (test utils)
  #:use-module (wasm assemble)
  #:use-module (wasm parse)
  #:use-module (hoot config)
  #:use-module (hoot compile)
  #:use-module (hoot reflect)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64)
  #:use-module ((system syntax) #:select (syntax?))
  #:export (use-node?
            use-d8?
            use-hoot-vm?
            v8
            unwind-protect
            call-with-compiled-wasm-file
            compile-main
            compile-aux
            load-wasm
            call-wasm
            await-call-wasm
            test-compilation
            test-call
            test-await
            test-end*
            with-imports
            with-additional-imports))

(define test-hosts (string-split (or (getenv "WASM_HOST") "node,hoot") #\,))
(define use-d8? (make-parameter (member "d8" test-hosts)))
(define use-node? (make-parameter (member "node" test-hosts)))
(define use-hoot-vm? (make-parameter (member "hoot" test-hosts)))

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

(define (v8)
  (or (and (use-node?) %node)
      (and (use-d8?) %d8)
      (error "no V8 runtime available")))

(define (run-v8 . args)
  (let* ((port (apply open-pipe* OPEN_READ (v8) args))
         (output (get-string-all port)))
    (close-port port)
    (string-trim-both output)))

(define (load-wasm/v8 wasm)
  (define runner (in-vicinity %js-runner-dir "load.js"))
  (call-with-compiled-wasm-file
   wasm
   (lambda (wasm-file-name)
     (run-v8 runner "--" %reflect-js-dir %reflect-wasm-dir
             wasm-file-name))))

(define (apply-wasm/v8 proc args)
  (define runner (in-vicinity %js-runner-dir "call.js"))
  (let lp ((modules (cons proc args)) (files '()) (first? #t))
    (match modules
      (()
       (apply run-v8 runner "--" %reflect-js-dir %reflect-wasm-dir
              (reverse files)))
      ((module . rest)
       (call-with-compiled-wasm-file
        module
        (lambda (file)
          (lp rest (cons file files) #f)))))))

(define (await-apply-wasm/v8 proc args)
  (define runner (in-vicinity %js-runner-dir "await-call.js"))
  (let lp ((modules (cons proc args)) (files '()) (first? #t))
    (match modules
      (()
       (apply run-v8 runner "--" %reflect-js-dir %reflect-wasm-dir
              (reverse files)))
      ((module . rest)
       (call-with-compiled-wasm-file
        module
        (lambda (file)
          (lp rest (cons file files) #f)))))))

(define (call-with-printed-values thunk)
  (string-trim-both
   (with-output-to-string
     (lambda ()
       (call-with-values thunk
         (lambda vals
           (for-each (lambda (x)
                       (hoot-print x (current-output-port))
                       (newline))
                     vals)))))))

(define (load-wasm/hoot wasm)
  (call-with-printed-values
   (lambda ()
     (hoot-load (hoot-instantiate wasm)))))

(define (apply-wasm*/hoot proc proc-wasm args-wasm)
  (call-with-printed-values
   (lambda ()
     (let* ((proc-module (hoot-instantiate proc-wasm))
            (proc* (hoot-load proc-module))
            (reflector (hoot-module-reflector proc-module))
            (args (map (lambda (arg)
                         (hoot-load
                          (hoot-instantiate arg '() reflector)))
                       args-wasm)))
       (apply proc proc* args)))))
(define (apply-wasm/hoot proc args)
  (apply-wasm*/hoot hoot-apply proc args))
(define (await-apply-wasm/hoot proc args)
  (apply-wasm*/hoot hoot-apply-async proc args))

(define (compare-results hoot-result v8-result)
  (cond
   ((and (use-hoot-vm?) (or (use-node?) (use-d8?)))
    (unless (equal? hoot-result v8-result)
      (error "our result differs from v8" hoot-result v8-result))
    hoot-result)
   ((or (use-node?) (use-d8?)) v8-result)
   (else hoot-result)))

(define-syntax-rule (hoot&v8 hoot-expr v8-expr)
  (compare-results (and (use-hoot-vm?) hoot-expr)
                   (and (or (use-node?) (use-d8?)) v8-expr)))

(define %imports (make-parameter '((scheme base))))

(define (imports-for-expr expr)
  ;; In the test suite we sometimes compile datums; don't load any
  ;; imports in that case.
  (match (if (syntax? expr)
             (syntax->datum expr)
             expr)
    (('quote _) '((hoot syntax)))
    ((? self-evaluating?) '())
    (else (%imports))))

(define cache (make-hash-table))
(define (compile/cache expr . args)
  (cond
   ((hash-ref cache (cons expr args)))
   (else
    (let ((result (apply compile expr #:imports (imports-for-expr expr) args)))
      (hash-set! cache (cons expr args) result)
      result))))

(define (compile-main expr)
  (compile/cache expr))
(define (compile-aux expr)
  (compile/cache expr #:import-abi? #t #:export-abi? #f))

(define (load-wasm wasm)
  (hoot&v8 (load-wasm/hoot wasm)
           (load-wasm/v8 wasm)))
(define (apply-wasm proc args)
  (hoot&v8 (apply-wasm/hoot proc args)
           (apply-wasm/v8 proc args)))
(define (call-wasm proc . args)
  (apply-wasm proc args))
(define (await-apply-wasm proc args)
  (hoot&v8 (await-apply-wasm/hoot proc args)
           (await-apply-wasm/v8 proc args)))
(define (await-call-wasm proc . args)
  (await-apply-wasm proc args))

(define-syntax-rule (test-compilation expr repr)
  (test-equal repr repr
              (load-wasm (compile-main `expr))))

(define-syntax-rule (test-call repr proc arg ...)
  (test-equal repr repr
              (call-wasm (compile-main `proc) (compile-aux `arg) ...)))

(define-syntax-rule (test-await repr . body)
  (with-additional-imports ((fibers promises))
    (test-equal repr repr
                (await-call-wasm
                 (compile-main
                  `(lambda (resolved rejected)
                     (call-with-async-result
                      resolved rejected (lambda () . body))))))))

(define-syntax-rule (test-end* name)
  (begin
    (when (and (batch-mode?)
               (or (not (zero? (test-runner-fail-count (test-runner-get))))
                   (not (zero? (test-runner-xpass-count (test-runner-get))))))
      (force-output)
      (exit 1))
    (test-end name)))

(define-syntax-rule (with-imports (ispec ...) . body)
  (parameterize ((%imports '(ispec ...)))
    . body))

(define-syntax-rule (with-additional-imports (ispec ...) . body)
  (parameterize ((%imports (cons* 'ispec ... (%imports))))
    . body))
