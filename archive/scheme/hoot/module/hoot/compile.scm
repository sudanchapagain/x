;;; WebAssembly compiler
;;; Copyright (C) 2023, 2024, 2025, 2025 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Scheme to WebAssembly compiler.
;;;
;;; Code:

(define-module (hoot compile)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module ((system base compile)
                #:select ((compile . %compile)
                          default-warning-level
                          default-optimization-level))
  #:use-module (system base target)
  #:use-module (hoot library-group)
  #:use-module (hoot inline-wasm)
  #:use-module (hoot backend)
  #:use-module (hoot frontend)
  #:use-module (hoot config)
  #:use-module (wasm assemble)
  #:use-module (language tree-il)
  #:export (%default-program-imports
            scheme->sealed-tree-il
            read-and-compile
            compile-file
            compile
            library-load-path-extension
            available-debug-options
            default-debug-level))

(define (available-debug-options)
  '((names 1 "emit WebAssembly name section")
    (runtime-modules 1 "register all definitions with run-time module system")
    (mutable-defs 2 "allow all definitions to be updated at run-time")))
(define (lookup-debug-option opt debug-options debug-level)
  (or (memq opt debug-options)
      (match (assq opt (available-debug-options))
        ((_ level help) (<= level debug-level)))))

(define (level-validator x)
  (unless (and (exact-integer? x) (<= 0 x 9))
    (error
     "bad warning or optimization level: expected integer between 0 and 9"
     x))
  x)

(define default-debug-level (make-parameter 0 level-validator))

(define-syntax-rule (with-hoot-target . body)
  (with-target "wasm32-unknown-hoot"
    (lambda ()
      (parameterize ((target-runtime 'hoot))
        . body))))

(define (%include-from-path filename)
  (let ((filename
         (or (search-path (append (hoot-system-load-path) (hoot-load-path))
                          filename
                          (hoot-load-extensions))
             (error 'include "file not found in path" filename))))
    (call-with-include-port
     (datum->syntax #f (canonicalize-path filename))
     (lambda (p)
       (let lp ()
         (match (read-syntax p)
           ((? eof-object?) #'())
           (x (cons x (lp)))))))))

(define (include-relative-to-file base)
  (lambda (filename)
    (let ((filename (if (absolute-file-name? filename)
                        filename
                        (in-vicinity (dirname (canonicalize-path base))
                                     filename))))
      (unless (file-exists? filename)
        (error "file not found" filename))
      (call-with-include-port
       (datum->syntax #f filename)
       (lambda (p)
         (let lp ()
           (match (read-syntax p)
             ((? eof-object?) #'())
             (x (cons x (lp))))))))))

(define (include-relative-to-port port)
  (cond
   ((port-filename port) => include-relative-to-file)
   (else (lambda (filename) (error "port has no file name" port)))))

(define base-hoot-features
  '(r7rs exact-closed ieee-float full-unicode ratios wasm hoot hoot-1.0))

(define* (add-compilation-features features #:key (import-abi? #f)
                                   (debug-level (default-debug-level))
                                   (debug-options '()))
  (define runtime-modules?
    (and (not import-abi?)
         (lookup-debug-option 'runtime-modules debug-options debug-level)))
  (define mutable-module-bindings?
    (and runtime-modules?
         (lookup-debug-option 'mutable-defs debug-options debug-level)))
  (let* ((features (cons (if import-abi? 'hoot-aux 'hoot-main) features))
         (features (if runtime-modules?
                       (cons 'runtime-modules features)
                       features))
         (features (if mutable-module-bindings?
                       (cons 'mutable-module-bindings features)
                       features)))
    features))

(define %default-program-imports '((scheme base)))

(define (features-module-loader import-abi?)
  (lambda* (name #:key (features '()))
    (and (equal? name '(hoot features))
         (let ((trusted? #t))
           (parse-library
            `((library (hoot features)
                (export features)
                (import (hoot syntax))
                (define (features) ',features)))
            trusted?)))))

(define* (%library-load-path-extension load-path #:key (trusted? #f))
  (define (relativize-filename filename)
    (or (or-map (lambda (dir)
                  (let ((dir (false-if-exception (canonicalize-path dir))))
                    (and dir
                         (not (string-null? dir))
                         (string-prefix? dir filename)
                         (cond
                          ((file-name-separator?
                            (string-ref dir (1- (string-length dir))))
                           (substring filename (string-length dir)))
                          ((file-name-separator?
                            (string-ref filename (string-length dir)))
                           (substring filename (1+ (string-length dir))))
                          (else #f)))))
                load-path)
        filename))

  (define (read-forms-from-file filename)
    (call-with-include-port
     (datum->syntax #f (canonicalize-path filename))
     (lambda (p)
       (set-port-filename! p (relativize-filename (port-filename p)))
       (let lp ()
         (match (read-syntax p)
           ((? eof-object?) #'())
           (x (cons x (lp))))))))

  (define (name-component->string x)
    (cond
     ((symbol? x)
      (let ((str (symbol->string x)))
        (when (or (equal? str "")
                  (equal? str ".")
                  (equal? str "..")
                  (string-any file-name-separator? str)
                  (absolute-file-name? str))
          (error "invalid name component" x))
        str))
     ((and (exact-integer? x) (not (negative? x)))
      (number->string x))
     (else
      (error "invalid name component" x))))

  (define (library-name->file-name name)
    (string-join (map name-component->string name) file-name-separator-string))

  (define (locate-library name)
    (search-path load-path (library-name->file-name name) %load-extensions))

  (lambda (load-library)
    (lambda* (name #:key (features '()))
      (define (load-library-from-file filename)
        (parse-library (read-forms-from-file filename) trusted?
                       #:include-file (include-relative-to-file filename)
                       #:features features))

      (cond
       ((load-library name #:features features))
       ((locate-library name) => load-library-from-file)
       (else #f)))))

(define (builtin-module-loader import-abi?)
  ((%library-load-path-extension %stdlib-path #:trusted? #t)
   (features-module-loader import-abi?)))

(define* (library-load-path-extension load-path #:key (features '()))
  (%library-load-path-extension load-path))

(define* (expand-hoot-library-group group #:key
                                    runtime-modules? mutable-module-bindings?)
  "Expand @var{group}.  Work around some of the peculiarities of the
Hoot compilation environment: rewriting primitives introduced by Guile's
expander and support the primitives needed to compile
`cross-compilation-case`.

If @var{runtime-modules?} is true, arrange to residualize syntax
transformers and emit code to register each definition with the run-time
module system.  If @var{mutable-module-bindings?} is true, allow all
module bindings (private and exported) to be mutated at run-time."
  (define (rewrite-primitive exp make-module-ref)
    (match exp
      (($ <primitive-ref> src name)
       (case name
         ((list->vector)
          (make-module-ref src '(hoot vectors) name #t))
         ((list cons)
          (make-module-ref src '(hoot pairs) name #t))
         ((map)
          (make-module-ref src '(hoot lists) name #t))
         ((syntax-violation)
          (make-module-ref src '(hoot core-syntax-helpers) name #t))
         ((make-syntax-transformer)
          (make-module-ref src '(hoot syntax-transformers) name #t))
         (($sc-dispatch)
          (make-module-ref src '(hoot expander) name #f))
         ((error)
          (make-module-ref src '(hoot errors) name #t))
         (else
          exp)))))

  (define (rewrite-host-reference exp make-module-ref)
    (match exp
      (($ <module-ref> src ('guile) name #f)
       (case name
         ((memv not list cons list->vector map vector append)
          (make-primitive-ref src name))
         ((identifier? syntax-violation)
          (make-module-ref src '(hoot core-syntax-helpers) name #t))
         (else (error "unhandled expander host reference" src name))))
      (exp
       (error "unexpected host module reference!" exp))))

  (define (rewrite-host-primitive src name make-module-ref)
    (match name
      ('target-runtime
       (make-lambda src '()
                    (make-lambda-case src '() #f #f #f '() '()
                                      (make-const src 'hoot)
                                      #f)))
      ('%syntax->datum
       (make-module-ref src '(hoot syntax-objects) 'syntax->datum #t))
      (_
       (error "unexpected host primitive reference!" src name))))

  (define root-module-lexical (gensym "root-module"))
  (define deferred-declarations '())
  (define (add-module-declaration decl bindings)
    (if deferred-declarations
        (begin
          (set! deferred-declarations (cons decl deferred-declarations))
          bindings)
        (cons decl bindings)))

  (define* (group-by key xs #:key (equal? equal?))
    (let lp ((xs xs) (out '()))
      (match xs
        (() out)
        ((x . xs)
         (let ((k (key x)))
           (lp xs
               (let lp ((out out))
                 (match out
                   (() (acons x '() '()))
                   ((xs . out)
                    (if (equal? (key (car xs)) k)
                        (acons x xs out)
                        (cons xs (lp out))))))))))))
    
  (define (hackable-handlers src modname version imports exports
                             duplicate-handler
                             make-module-ref make-module-set)
    (define (make-declare-library module-def-names module-defs-var)
      (make-call
       src
       (make-module-ref #f '(hoot hackable) 'declare-module! #t)
       (list (make-lexical-ref #f 'root root-module-lexical)
             (make-const src modname)
             (make-const src
                         (map (lambda (imports)
                                (list
                                 (import-modname (car imports))
                                 (map import-imported-name imports)
                                 (map import-exported-name imports)))
                              (group-by import-modname imports)))
             (make-const src module-def-names)
             (make-lexical-ref src 'defs module-defs-var)
             (make-const src exports))))
    (define (make-case-lambda-wrapper name sym)
      (define assign-case
        (let ((v (gensym "new-val")))
          (make-lambda-case
           src '(new-val) #f #f #f '() (list v)
           (if mutable-module-bindings?
               (make-lexical-set #f name sym
                                 (make-lexical-ref #f 'new-val v))
               (make-call
                #f (make-module-ref src '(hoot errors) 'error #t)
                (list
                 (make-const
                  #f
                  "run-time module bindings are immutable; recompile with -g2"))))
           #f)))
      (define ref-case
        (make-lambda-case src '() #f #f #f '() '()
                          (make-lexical-ref #f name sym)
                          assign-case))
      (make-lambda #f '() ref-case))
    (define (add-root-module bindings)
      (cons (make-definition
             'root root-module-lexical
             (make-call #f
                        (make-module-ref #f '(hoot hackable) 'boot-module-tree
                                         #t)
                        '()))
            bindings))
    (define (emit-deferred-module-declarations bindings)
      (let ((decls deferred-declarations))
        (set! deferred-declarations #f)
        (append decls (add-root-module bindings))))

    (define (lift bindings)
      (cons '() bindings))
    (define (add-annotated-definition name sym val state)
      (match state
        ((defs . bindings)
         (cons (acons name (make-case-lambda-wrapper name sym) defs)
               (cons (make-definition name sym val) bindings)))))
    (define (add-annotated-statement exp state)
      (match state
        ((defs . bindings)
         (cons defs (cons (make-statement exp) bindings)))))
    (define (lower state)
      (match state
        ((((def-name . def-var) ...) . bindings)
         (let lp ((bindings bindings))
           (match bindings
             (((? statement? s) . bindings)
              (cons s (lp bindings)))
             (_
              (let* ((mod-vars-sym (gensym "defs"))
                     (mod-var-names def-name)
                     (bindings
                      (cons (make-definition 'mod-vars mod-vars-sym
                                             (make-primcall src 'list def-var))
                            bindings)))
                (add-module-declaration
                 (make-statement
                  (make-declare-library mod-var-names mod-vars-sym))
                 (if (equal? modname '(hoot hackable))
                     (emit-deferred-module-declarations bindings)
                     bindings)))))))))

    ;; Two special modules: (hoot primitives), whose definitions aren't
    ;; actually part of the compilation unit but rather provided by
    ;; (hoot primitives-module), and (hoot core-syntax), which is part
    ;; of the compilation unit but which we skip, instead having (hoot
    ;; expander) fabricate its definitions.
    (cond
     ((equal? modname '(hoot core-syntax))
      (trivial-residualize-handlers))
     (else
      (values lift add-annotated-definition add-annotated-statement lower))))

  (expand-library-group group
                        #:primitives '(hoot primitives)
                        #:rewrite-primitive rewrite-primitive
                        #:rewrite-host-reference rewrite-host-reference
                        #:rewrite-host-primitive rewrite-host-primitive
                        #:call-with-target (lambda (f)
                                             (with-hoot-target (f)))
                        #:residualize-syntax-transformers? runtime-modules?
                        #:residualize-handlers
                        (if runtime-modules?
                            hackable-handlers
                            (lambda _
                              (trivial-residualize-handlers)))))

(define* (scheme->sealed-tree-il expr #:key
                                 (imports %default-program-imports)
                                 (import-abi? #f)
                                 (include-file %include-from-path)
                                 (extend-load-library (lambda (f) f))
                                 (load-library
                                  (extend-load-library
                                   (builtin-module-loader import-abi?)))
                                 (debug-level (default-debug-level))
                                 (debug-options '())
                                 (features base-hoot-features))
  (define runtime-modules?
    (and (not import-abi?)
         (lookup-debug-option 'runtime-modules debug-options debug-level)))
  (define mutable-module-bindings?
    (and runtime-modules?
         (lookup-debug-option 'mutable-defs debug-options debug-level)))
  (define group
    (match expr
      ((? library-group?) expr)
      (_ (parse-library-group `(library-group (import . ,imports) ,expr)
                              #:include-file include-file))))
  (define (prepare-hackable-compilation-unit group)
    (concatenate-library-groups (parse-library-group
                                 '(library-group
                                   (import (only (hoot hackable)))))
                                group))
  (define linked
    (link-library-group (if runtime-modules?
                            (prepare-hackable-compilation-unit group)
                            group)
                        #:load-library load-library
                        #:features (add-compilation-features
                                    features
                                    #:import-abi? import-abi?
                                    #:debug-level debug-level
                                    #:debug-options debug-options)
                        #:allow-dangling-import?
                        (lambda (name)
                          (equal? name '(hoot primitives)))))

  (expand-hoot-library-group linked
                             #:runtime-modules? runtime-modules?
                             #:mutable-module-bindings?
                             mutable-module-bindings?))

(define* (compile expr #:key
                  (imports %default-program-imports)
                  (import-abi? #f)
                  (export-abi? #t)
                  (include-file %include-from-path)
                  (features base-hoot-features)
                  (extend-load-library
                   (library-load-path-extension (hoot-load-path)))
                  (load-library
                   (extend-load-library (builtin-module-loader import-abi?)))
                  (optimization-level (default-optimization-level))
                  (warning-level (default-warning-level))
                  (dump-tree-il? #f)
                  (dump-cps? #f)
                  (dump-wasm? #f)
                  (debug-level (default-debug-level))
                  (debug-options '())
                  (opts '()))
  (define emit-names?
    (lookup-debug-option 'names debug-options debug-level))
  (define tree-il
    (scheme->sealed-tree-il expr #:imports imports
                            #:import-abi? import-abi?
                            #:features features
                            #:include-file include-file
                            #:load-library load-library
                            #:debug-level debug-level
                            #:debug-options debug-options))
  (with-hoot-target
   (define cps
     ;; Prevent CPS conversion from capturing the current module, as we
     ;; include no top-level references.
     (parameterize (((@@ (language tree-il compile-cps) current-topbox-scope) #t))
       (%compile tree-il #:env #f #:from 'tree-il #:to 'cps
                 #:optimization-level optimization-level
                 #:warning-level warning-level
                 #:opts (cons* #:cps? #t
                               (if dump-tree-il?
                                   (cons* #:dump-optimized-tree-il? #t opts)
                                   opts)))))
   (high-level-cps->wasm cps
                         #:import-abi? import-abi?
                         #:export-abi? export-abi?
                         #:optimization-level optimization-level
                         #:warning-level warning-level
                         #:dump-cps? dump-cps?
                         #:dump-wasm? dump-wasm?
                         #:emit-names? emit-names?
                         #:opts opts)))

(define* (read-and-compile port #:key
                           (import-abi? #f)
                           (export-abi? #t)
                           (optimization-level (default-optimization-level))
                           (warning-level (default-warning-level))
                           (include-file (include-relative-to-port port))
                           (features base-hoot-features)
                           (extend-load-library (lambda (f) f))
                           (load-library
                            (extend-load-library (builtin-module-loader import-abi?)))
                           (dump-tree-il? #f)
                           (dump-cps? #f)
                           (dump-wasm? #f)
                           (debug-level (default-debug-level))
                           (debug-options '())
                           (opts '()))
  (define (name-matches? stx sym)
    (eq? (syntax->datum stx) sym))
  (define-syntax-rule (symbolic-match? name)
    (name-matches? #'name 'name))

  (define forms
    (let lp ()
      (let ((expr (read-syntax port)))
        (if (eof-object? expr)
            '()
            (cons expr (lp))))))

  (define group
    (syntax-case forms ()
      (((library-group . _))
       (symbolic-match? library-group)
       (parse-library-group (car forms) #:include-file include-file
                            #:features (add-compilation-features
                                        features
                                        #:import-abi? import-abi?
                                        #:debug-level debug-level
                                        #:debug-options debug-options)))
      (((import . imports) . body)
       (symbolic-match? import)
       (parse-library-group #'(library-group (import . imports) . body)))
      (((use-modules . imports) . body)
       (symbolic-match? use-modules)
       (parse-library-group #'(library-group (use-modules . imports) . body)))
      (_
       (parse-library-group
        `(library-group (import . ,%default-program-imports) . ,forms)))))

  (compile group
           #:import-abi? import-abi?
           #:export-abi? export-abi?
           #:optimization-level optimization-level
           #:warning-level warning-level
           #:load-library load-library
           #:features features
           #:dump-tree-il? dump-tree-il?
           #:dump-cps? dump-cps?
           #:dump-wasm? dump-wasm?
           #:debug-level debug-level
           #:debug-options debug-options
           #:opts opts))

(define* (compile-file input-file #:key
                       (output-file #f)
                       (import-abi? #f)
                       (export-abi? #t)
                       (optimization-level (default-optimization-level))
                       (warning-level (default-warning-level))
                       (include-file (include-relative-to-file input-file))
                       (features base-hoot-features)
                       (extend-load-library (lambda (f) f))
                       (load-library
                        (extend-load-library (builtin-module-loader import-abi?)))
                       (dump-tree-il? #f)
                       (dump-cps? #f)
                       (dump-wasm? #f)
                       (debug-level (default-debug-level))
                       (debug-options '())
                       (opts '()))
  (call-with-input-file input-file
    (lambda (in)
      (set-port-encoding! in (or (file-encoding in) "UTF-8"))
      (let ((wasm (read-and-compile in
                                    #:import-abi? import-abi?
                                    #:export-abi? export-abi?
                                    #:optimization-level optimization-level
                                    #:warning-level warning-level
                                    #:include-file include-file
                                    #:features features
                                    #:load-library load-library
                                    #:dump-tree-il? dump-tree-il?
                                    #:dump-cps? dump-cps?
                                    #:dump-wasm? dump-wasm?
                                    #:debug-level debug-level
                                    #:debug-options debug-options
                                    #:opts opts)))
        (when output-file
          (let ((bytes (assemble-wasm wasm)))
            (call-with-output-file output-file
              (lambda (out)
                (put-bytevector out bytes)))))
        wasm))))

(install-inline-wasm!)
