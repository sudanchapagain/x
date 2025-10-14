;;; Compile --- Command-line Guile Scheme compiler  -*- coding: iso-8859-1 -*-

;;; Copyright (C) 2023, 2024, 2025 Igalia, S.L.
;;; Copyright 2005,2008-2011,2013-2015,2017-2020 Free Software Foundation, Inc.
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
;;; Usage: compile-wasm [ARGS]
;;;
;;; A command-line interface to the Guile-to-WebAssembly compiler.
;;;
;;; Code:

(define-module (scripts compile-wasm)
  #:use-module ((system base compile) #:select (default-warning-level
                                                default-optimization-level))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (system base message)
  #:use-module (system base optimize)
  #:use-module (hoot config)
  #:use-module (hoot compile)
  #:use-module (hoot frontend)
  #:use-module (hoot reflect)
  #:use-module (wasm assemble)
  #:export (compile-wasm))

(define %summary "Compile a file to WebAssembly.")


(define (fail message . args)
  (format (current-error-port) "error: ~?~%" message args)
  (exit 1))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda (opt name arg result)
		  (alist-cons 'help? #t result)))
        (option '("version") #f #f
                (lambda (opt name arg result)
                  (show-version)
                  (exit 0)))

	(option '(#\L "load-path") #t #f
		(lambda (opt name arg result)
                  (hoot-load-path (cons arg (hoot-load-path)))
                  result))
	(option '(#\o "output") #t #f
		(lambda (opt name arg result)
		  (if (assoc-ref result 'output-file)
		      (fail "`-o' option cannot be specified more than once")
		      (alist-cons 'output-file arg result))))
        (option '(#\b "bundle") #f #t
                (lambda (opt name arg result)
                  (if (assoc-ref result 'bundle-dir)
		      (fail "`-b' option cannot be specified more than once")
		      (alist-cons 'bundle-dir (or arg #t) result))))
        (option '("r6rs") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'install-r6rs? #t result)))
        (option '("dump-wasm") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'dump-wasm? #t result)))
        (option '("dump-cps") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'dump-cps? #t result)))
        (option '("dump-tree-il") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'dump-tree-il? #t result)))
        (option '("run") #f #t
		(lambda (opt name arg result)
		  (alist-cons 'vm
                              (match arg (#f 'hoot) ("hoot" 'hoot) (arg arg))
                              result)))
        (option '("user-imports") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'user-imports arg result)))
        (option '("async") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'async? #t result)))
        (option '("mode") #t #f
		(lambda (opt name arg result)
                  (acons 'mode
                         (cond
                          ((string=? arg "primary") 'primary)
                          ((string=? arg "standalone") 'standalone)
                          ((string=? arg "secondary") 'secondary)
                          (else (fail "unexpected `--mode' argument")))
                         result)))
        (option '(#\g) #f #t
                (lambda (opt name arg result)
                  (match arg
                    (#f (acons 'debug-level 1 result))
                    ((? string->number)
                     (let ((n (string->number arg)))
                       (unless (and (exact-integer? n) (<= 0 n 9))
                         (fail "bad debug level `~a'" n))
                       (acons 'debug-level n result)))
                    (_
                     (let ((arg (string->symbol arg)))
                       (cond
                        ((assq arg (available-debug-options))
                         (acons 'debug-options
                                (cons arg (assq-ref result 'debug-options))
                                result))
                        ((eq? arg 'help)
                         (show-debug-help)
                         (exit 0))
                        (else
                         (fail "unexpected `-g' argument: ~a; try `-ghelp'"
                               arg))))))))
        (option '("r7rs") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'install-r7rs? #t result)))
        (option '(#\x) #t #f
                (lambda (opt name arg result)
                  (hoot-load-extensions (cons arg (hoot-load-extensions)))
                  result))

        (option '(#\W "warn") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ("help"
                     (show-warning-help)
                     (exit 0))
                    ((? string->number)
                     (let ((n (string->number arg)))
                       (unless (and (exact-integer? n) (<= 0 n))
                         (fail "Bad warning level `~a'" n))
                       (alist-cons 'warning-level n
                                   (alist-delete 'warning-level result))))
                    (_
                     (let ((warnings (assoc-ref result 'warnings)))
                       (alist-cons 'warnings
                                   (cons (string->symbol arg) warnings)
                                   (alist-delete 'warnings result)))))))

	(option '(#\O "optimize") #t #f
		(lambda (opt name arg result)
                  (define (return val)
                    (alist-cons 'optimizations val result))
                  (define (return-option name val)
                    (let ((kw (symbol->keyword
                               (string->symbol (string-append name "?")))))
                      (unless (assq kw (available-optimizations))
                        (fail "Unknown optimization pass `~a'" name))
                      (return (list kw val))))
                  (cond
                   ((string=? arg "help")
                    (show-optimization-help)
                    (exit 0))
                   ((string->number arg)
                    => (lambda (level)
                         (unless (and (exact-integer? level) (<= 0 level 9))
                           (fail "Bad optimization level `~a'" level))
                         (alist-cons 'optimization-level level
                                     (alist-delete 'optimization-level result))))
                   ((string-prefix? "no-" arg)
                    (return-option (substring arg 3) #f))
                   (else
                    (return-option arg #t)))))))

(define (parse-args args)
  "Parse argument list @var{args} and return an alist with all the relevant
options."
  (args-fold args %options
             (lambda (opt name arg result)
               (format (current-error-port) "~A: unrecognized option~%" name)
	       (exit 1))
             (lambda (file result)
	       (let ((input-files (assoc-ref result 'input-files)))
		 (alist-cons 'input-files (cons file input-files)
			     result)))

	     ;; default option values
             `((input-files)
	       (warning-level . ,(default-warning-level))
               (optimization-level . ,(default-optimization-level))
               (debug-level . ,(default-debug-level))
               (debug-options . ())
               (warnings unsupported-warning)
               (mode . primary))))

(define (show-version)
  (format #t "compile-wasm ~A~%" (version))
  (format #t "Copyright (C) 2023  Spritely Institute, Igalia.
Part of guile-hoot:
  https://gitlab.com/spritely/guile-hoot
Licensed under the Apache License, Version 2.0:
  http://www.apache.org/licenses/LICENSE-2.0
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"))

(define (show-warning-help)
  (format #t "The available warning types are:~%~%")
  (for-each (lambda (wt)
              (format #t "  ~22A ~A~%"
                      (format #f "`~A'" (warning-type-name wt))
                      (warning-type-description wt)))
            %warning-types)
  (format #t "~%")
  (format #t "You may also specify warning levels as `-W0`, `-W1',~%")
  (format #t "`-W2', or `-W3'.  The default is `-W1'.~%"))

(define (show-optimization-help)
  (format #t "The available optimizations are:~%~%")
  (let lp ((options (available-optimizations)))
    (match options
      (() #t)
      (((kw level) . options)
       (let ((name (string-trim-right (symbol->string (keyword->symbol kw))
                                      #\?)))
         (format #t "  -O~a~%" name)
         (lp options)))))
  (format #t "~%")
  (format #t "To disable an optimization, prepend it with `no-', for example~%")
  (format #t "`-Ono-cse.'~%~%")
  (format #t "You may also specify optimization levels as `-O0', `-O1',~%")
  (format #t "`-O2', or `-O3'.  Currently `-O0' turns off all optimizations,~%")
  (format #t "`-O1' turns on partial evaluation, and `-O2' and `-O3' turn on~%")
  (format #t "everything.  The default is equivalent to `-O2'.")
  (format #t "~%"))

(define (show-debug-help)
  (format #t "The available debug options are:~%~%")
  (for-each (match-lambda
              ((opt level help)
               (format #t "  -g~22A ~A~%" opt help)
               (format #t "                           (included in -g~A)~%" level)))
            (available-debug-options))
  (format #t "~%")
  (format #t "You may also specify debug options as `-g0`, `-g1', or `-g2`.~%")
  (format #t "Passing `-g` is the same as `-g1'.~%"))

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

(define (call-with-named-output-file output-file f k)
  (call-with-values (lambda () (call-with-output-file output-file f))
    (lambda vals
      (format #t "wrote `~A'\n" output-file)
      (apply k output-file vals))))

(define (call-with-temp-output-file f k)
  (let* ((wasm-port (mkstemp "/tmp/tmp-wasm-XXXXXX"))
         (wasm-file-name (port-filename wasm-port)))
    (call-with-values (lambda () (f wasm-port))
      (lambda vals
        (close-port wasm-port)
        (unwind-protect
         (lambda () (apply k wasm-file-name vals))
         (lambda () (delete-file wasm-file-name)))))))

(define (call-with-no-output-file f k)
  (call-with-values (lambda () (f #f))
    (lambda vals
      (apply k #f vals))))

(define* (call-with-output-file-as-needed f k
                                          #:key named-output-file needs-file?)
  (cond
   (named-output-file (call-with-named-output-file named-output-file f k))
   (needs-file?       (call-with-temp-output-file f k))
   (else              (call-with-no-output-file f k))))

(define (run/hoot wasm async? user-imports-file)
  (define user-imports (if user-imports-file (load user-imports-file) '()))
  (call-with-values (lambda ()
                      (if async?
                          (hoot-apply-async
                           (hoot-load (hoot-instantiate wasm user-imports)))
                          (hoot-load (hoot-instantiate wasm user-imports))))
    (lambda vals
      (for-each (lambda (x)
                  (hoot-print x (current-output-port))
                  (newline))
                vals))))

(define (run/js wasm-file js async? user-imports-file)
  ;; system* rather than execlp, to give us a chance to remove any
  ;; temporary output file.
  (define runner (in-vicinity %js-runner-dir
                              (if async? "load-async.js" "load.js")))
  (exit (status:exit-val
         (system* js runner "--" %reflect-js-dir %reflect-wasm-dir
                  wasm-file (or user-imports-file "")))))

(define (compile-wasm . args)
  (let* ((options         (parse-args args))
         (help?           (assoc-ref options 'help?))
         (warning-level   (assoc-ref options 'warning-level))
         (optimization-level (assoc-ref options 'optimization-level))
         (import-abi?     (match (assq-ref options 'mode)
                            ((or 'standalone 'primary) #f)
                            ('secondary #t)))
         (export-abi?     (match (assq-ref options 'mode)
                            ('primary #t)
                            ((or 'standalone 'secondary) #f)))
         (dump-wasm?      (assoc-ref options 'dump-wasm?))
         (dump-cps?       (assoc-ref options 'dump-cps?))
         (dump-tree-il?   (assoc-ref options 'dump-tree-il?))
         (debug-level     (assoc-ref options 'debug-level))
         (debug-options   (assoc-ref options 'debug-options))
         (compile-opts    `(#:warnings
                            ,(assoc-ref options 'warnings)
                            ,@(append-map
                               (lambda (opt)
                                 (match opt
                                   (('optimizations . opts) opts)
                                   (_ '())))
                               options)))
         (input-files     (assoc-ref options 'input-files))
	 (output-file     (assoc-ref options 'output-file))
         (bundle-dir      (match (assoc-ref options 'bundle-dir)
                            (#f #f)
                            (#t
                             (unless output-file
                               (fail "`-b' must be used with `-o'"))
                             (dirname output-file))
                            (dir
                             (unless output-file
                               (fail "`-b' must be used with `-o'"))
                             dir)))
	 (vm              (or (assoc-ref options 'vm)
                              (and (not output-file)
                                   'hoot)))
         (user-imports    (and=> (assoc-ref options 'user-imports)
                                 canonicalize-path))
         (async?          (assoc-ref options 'async?)))
    (when (or help? (null? input-files))
      (format #t "Usage: compile-wasm [OPTION] FILE
Compile the Guile source file FILE into a WebAssembly module file.

  -h, --help           print this help message

  -L, --load-path=DIR  add DIR to the front of the module load path
  -o, --output=OFILE   write output to OFILE
  -b, --bundle[=DIR]   when combined with `-o', copy web runtime libraries
                       to DIR or the directory of OFILE if DIR is not
                       specified.

  -x EXTENSION         add EXTENSION to the set of source file extensions

  -W, --warn=WARNING   emit warnings of type WARNING; use `--warn=help'
                       for a list of available warnings
  -O, --optimize=OPT   specify optimization passes to run; use `-Ohelp'
                       for a list of available optimizations

  --r6rs, --r7rs       compile in an environment whose default bindings,
                       reader options, and load paths are adapted for
                       specific Scheme standards; see \"R6RS Support\"
                       and \"R7RS Support\" in the manual, for full details

  --run, --run=JS      run the compiled wasm; by default, in Hoot
                       virtual machine, otherwise using a JavaScript shell
  --async              when combined with `--run', run program in async
                       context.
  --user-imports=FILE  when combined with `--run`, load the Scheme/JS source
                       FILE and pass the result as additional Wasm imports.

  --mode=primary       compile a main module: one which defines run-time
                       facilities and which by default makes them
                       available to secondary modules.  The default mode.
  --mode=standalone    like `--mode=primary', but without the
                       possibility of sharing run-time facilities with
                       secondary modules
  --mode=secondary     compile an auxiliary module: one which imports
                       runtime facilities instead of defining and exporting
                       them.

  --dump-tree-il       print a debugging representation of the high-level
                       expanded and optimized Scheme code
  --dump-cps           print a debugging representation of the low-level
                       CPS code, before generating WebAssembly
  --dump-wasm          print a debugging representation of the generated
                       WebAssembly code
  -g                   residualize additional code and meta-data to allow for
                       online debugging, e.g. from an interactive REPL
  -gN, -gOPT           control level and kind of debugging code; use `-ghelp`
                       for a list of available options

Report bugs to <~A>.~%"
              %guile-bug-report-address)
      (exit 0))

    (when (assoc-ref options 'install-r6rs?)
      (install-r6rs!))
    (when (assoc-ref options 'install-r7rs?)
      (install-r7rs!))

    ;; Install a SIGINT handler.  As a side effect, this gives unwind
    ;; handlers an opportunity to run upon SIGINT; this includes that of
    ;; 'call-with-output-file/atomic', called by 'compile-file', which
    ;; removes the temporary output file.
    (sigaction SIGINT
               (lambda args
                 (fail "interrupted by the user")))

    (match input-files
      (() (fail "missing input file"))
      ((input-file)
       (let ((wasm (with-fluids ((*current-warning-prefix* ""))
                     (compile-file input-file
                                   #:extend-load-library
                                   (library-load-path-extension (hoot-load-path))
                                   #:warning-level warning-level
                                   #:optimization-level optimization-level
                                   #:import-abi? import-abi?
                                   #:export-abi? export-abi?
                                   #:dump-tree-il? dump-tree-il?
                                   #:dump-cps? dump-cps?
                                   #:dump-wasm? dump-wasm?
                                   #:debug-level debug-level
                                   #:debug-options debug-options
                                   #:opts compile-opts))))
         (call-with-output-file-as-needed
          (lambda (out)
            (when out
              (put-bytevector out (assemble-wasm wasm)))
            wasm)
          (lambda (output-file wasm)
            (define (bundle-copy root file-name)
              (let ((src (string-append root "/" file-name))
                    (dst (string-append bundle-dir "/" file-name)))
                (copy-file src dst)
                (chmod dst #o644)))
            (when bundle-dir
              (bundle-copy %reflect-js-dir "reflect.js")
              (bundle-copy %reflect-wasm-dir "reflect.wasm")
              (bundle-copy %reflect-wasm-dir "wtf8.wasm"))
            (match vm
              (#f (values))
              ('hoot (run/hoot wasm async? user-imports))
              (js (run/js output-file js async? user-imports))))
          #:named-output-file output-file
          #:needs-file? (not (eq? vm 'hoot))))
       #t)
      (_ (fail "multiple input files not supported")))))

(define main compile-wasm)
