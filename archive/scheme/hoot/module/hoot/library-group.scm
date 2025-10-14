;;; Library-group expander
;;; Copyright (C) 2024, 2025 Igalia, S.L.
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
;;; Parser, linker, and expander for `library-group` form.
;;;
;;; Code:

(define-module (hoot library-group)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:use-module (language tree-il primitives)
  #:use-module ((srfi srfi-1) #:select (append-map partition fold fold-right))
  #:use-module (srfi srfi-9)
  #:use-module ((system syntax internal) #:select (syntax? syntax-sourcev))
  #:export (import?
            import-modname
            import-exported-name
            import-imported-name
            library-group?
            parse-library
            parse-library-group
            concatenate-library-groups
            link-library-group
            expand-library-group
            make-statement statement?
            make-definition
            trivial-residualize-handlers))

(define-record-type <iset-library>
  (make-iset-library name version)
  iset-library?
  (name iset-library-name)
  (version iset-library-version))

(define-record-type <iset-only>
  (make-iset-only iset ids)
  only?
  (iset iset-only-iset)
  (ids iset-only-ids))

(define-record-type <iset-except>
  (make-iset-except iset ids)
  iset-except?
  (iset iset-except-iset)
  (ids iset-except-ids))

(define-record-type <iset-rename>
  (make-iset-rename iset renamings)
  iset-rename?
  (iset iset-rename-iset)
  (renamings iset-rename-renamings))

(define-record-type <iset-prefix>
  (make-iset-prefix iset prefix)
  iset-prefix?
  (iset iset-prefix-iset)
  (prefix iset-prefix-prefix))

(define-record-type <library>
  (make-library src name version trusted? duplicate-handler exports isets body)
  library?
  (src library-src)
  (name library-name)
  (version library-version)
  (trusted? library-trusted?)
  (duplicate-handler library-duplicate-handler)
  (exports library-exports)
  (isets library-isets)
  (body library-body))

(define-record-type <program>
  (make-program src trusted? duplicate-handler isets body)
  program?
  (src program-src)
  (trusted? program-trusted?)
  (duplicate-handler program-duplicate-handler)
  (isets program-isets)
  (body program-body))

(define-record-type <library-group>
  (make-library-group src libraries programs)
  library-group?
  (src library-group-src)
  (libraries library-group-libraries)
  (programs library-group-programs))

(define imported-library-name
  (match-lambda
    (($ <iset-only> iset select) (imported-library-name iset))
    (($ <iset-except> iset hide) (imported-library-name iset))
    (($ <iset-rename> iset renamings) (imported-library-name iset))
    (($ <iset-prefix> iset prefix) (imported-library-name iset))
    (($ <iset-library> name version) name)))

(define (id? x) (symbol? x))
(define (name-component? x) (id? x))
(define (version-component? x) (and (exact-integer? x) (not (negative? x))))
(define (name-matches? stx sym)
  (eq? (syntax->datum stx) sym))
(define-syntax-rule (symbolic-match? name)
  (name-matches? #'name 'name))

(define parse-name+version
  (match-lambda
    (((? name-component? name) ... ((? version-component? version) ...))
     (values name version))
    (((? name-component? name) ...)
     (values name '()))))

(define (includes-forbidden filename)
  (error "library-group include clause forbidden" filename))

(define (parse-imports import-sets)
  (define parse-import-set
    (match-lambda
      ((head . tail)
       (match head
         ('only
          (match tail
            ((iset (? id? select) ...)
             (make-iset-only (parse-import-set iset) select))))
         ('except
          (match tail
            ((iset (? id? hide) ...)
             (make-iset-except (parse-import-set iset) hide))))
         ('prefix
          (match tail
            ((iset (? id? prefix))
             (make-iset-prefix (parse-import-set iset) prefix))))
         ('rename
          (match tail
            ((iset ((? id? from) (? id? to)) ...)
             (make-iset-rename (parse-import-set iset) (map cons from to)))))
         ('library
             (match tail
               ((name+version)
                (call-with-values (lambda ()
                                    (parse-name+version name+version))
                  (lambda (name version)
                    (make-iset-library name version))))))
         (_
          (parse-import-set `(library (,head . ,tail))))))))
  (map (match-lambda
         ;; Strip level.
         (('for iset level ...) (parse-import-set iset))
         (iset                  (parse-import-set iset)))
       import-sets))

(define (parse-r6rs-library form trusted?)
  "Given the R6RS library @var{form}, as a syntax object, parse out the
imports and exports to a @code{library}."
  (define (parse-exports exports)
    ;; -> ((local . public) ...)
    (map (match-lambda
           ((? id? id) (cons id id))
           (('rename (? id? from) (? id? to)) (cons from to)))
         exports))

  (syntax-case form ()
    ((library (name ...)
       (export export-spec ...)
       (import import-spec ...)
       body ...)
     (and (symbolic-match? library)
          (symbolic-match? export)
          (symbolic-match? import))
     (let ()
       (define src
         (and (syntax? #'library) (syntax-sourcev #'library)))
       (define-values (modname version)
         (parse-name+version (syntax->datum #'(name ...))))
       (define exports
         (parse-exports (syntax->datum #'(export-spec ...))))
       (define imports
         (parse-imports (syntax->datum #'(import-spec ...))))
       (make-library src modname version trusted? forbid-duplicate-bindings
                     exports imports #'(body ...))))))

(define (parse-r7rs-library form trusted? include-file features)
  "Given the R7RS library @var{form}, as a syntax object, parse out the
imports and exports to a @code{library}."
  (define (parse-exports exports)
    ;; -> ((local . public) ...)
    (map (match-lambda
           ((? id? id) (cons id id))
           (('rename (? id? from) (? id? to)) (cons from to)))
         exports))

  (define (include-files filenames tail)
    (fold-right (lambda (filename tail)
                  (append (include-file filename) tail))
                tail
                filenames))

  (define (resolve-cond-expand form)
    (define (has-req? req)
      (match req
        (('and req ...)
         (and-map has-req? req))
        (('or req ...)
         (or-map has-req? req))
        (('not req)
         (not (has-req? req)))
        (('library lib-name)
         ;; FIXME: No libraries, for the time being.
         #f)
        ((? symbol? req)
         (memq req features))))
    (syntax-case form ()
      ((_ ce-clause ...)
       (let lp ((clauses #'(ce-clause ...)))
         (syntax-case clauses ()
           (()
            (syntax-violation 'cond-expand "Unfulfilled cond-expand" #'form))
           (((else decl ...))
            (symbolic-match? else)
            #'(decl ...))
           (((req decl ...) . clauses)
            (if (has-req? (syntax->datum #'req))
                #'(decl ...)
                (lp #'clauses))))))))

  (syntax-case form ()
    ((define-library (name ...) clause ...)
     (symbolic-match? define-library)
     (let ((src (and (syntax? #'library) (syntax-sourcev #'library))))
       (define-values (modname version)
         (parse-name+version (syntax->datum #'(name ...))))
       (let lp ((clauses #'(clause ...)) (exports '()) (imports '()) (body '()))
         (syntax-case clauses ()
           (()
            (make-library src modname version trusted? forbid-duplicate-bindings
                          exports imports body))
           (((export spec ...) . clauses)
            (symbolic-match? export)
            (lp #'clauses
                (append (parse-exports (syntax->datum #'(spec ...))) exports)
                imports
                body))
           (((import spec ...) . clauses)
            (symbolic-match? import)
            (lp #'clauses
                exports
                (append (parse-imports (syntax->datum #'(spec ...)))
                        imports)
                body))
           (((begin form ...) . clauses)
            (symbolic-match? begin)
            (lp #'clauses
                exports
                imports
                #`(form ... . #,body)))
           (((include filename ...) . clauses)
            (and (symbolic-match? include)
                 (and-map string? #'(filename ...)))
            (lp #'clauses
                exports
                imports
                (include-files (syntax->datum #'(filename ...)) body)))
           (((include-ci filename ...) . clauses)
            (and (symbolic-match? include-ci)
                 (and-map string? #'(filename ...)))
            (lp #'clauses
                exports
                imports
                ;; FIXME: Actually be case-insensitive!
                (include-files (syntax->datum #'(filename ...)) body)))
           (((include-library-definitions filename ...) . clauses)
            (and (symbolic-match? include-library-definitions)
                 (and-map string? #'(filename ...)))
            (lp (include-files (syntax->datum #'(filename ...)) #'clauses)
                exports
                imports
                body))
           (((cond-expand . ce-clauses) . clauses)
            (symbolic-match? cond-expand)
            (lp (append (resolve-cond-expand #'(cond-expand . ce-clauses))
                        #'clauses)
                exports
                imports
                body))))))))

(define (convert-sloppy-keyword x)
  (if (symbol? x)
      (let ((str (symbol->string x)))
        (if (string-prefix? ":" str)
            (symbol->keyword (string->symbol (substring str 1)))
            x))
      x))

(define (parse-guile-import ispec)
  (define (parse-args args)
    (let lp ((args args) (select #f) (hide #f) (prefix #f) (version #f))
      (match args
        (()
         (values select (or hide '()) prefix (or version '())))
        (((= convert-sloppy-keyword kw) . args)
         (unless (keyword? kw)
           (syntax-violation 'use-module "expected a keyword argument" ispec
                             (datum->syntax ispec kw)))
         (match args
           ((val . args)
            (match kw
              (#:select
               (when select
                 (syntax-violation 'use-module "too many #:select clauses"
                                   ispec))
               (lp args
                   (match val
                     (((or (? id?) ((? id?) . (? id?))) ...) val)
                     (_
                      (syntax-violation 'use-module "bad #:select declaration"
                                        ispec val)))
                   hide prefix version))
              (#:hide
               (when hide
                 (syntax-violation 'use-module "too many #:hide clauses"
                                   ispec))
               (lp args select
                   (match val
                     (((or (? id?) ((? id?) . (? id?))) ...) val)
                     (_
                      (syntax-violation 'use-module "bad #:select declaration"
                                        ispec val)))
                   prefix version))
              (#:renamer
               (when prefix
                 (syntax-violation 'use-module "too many #:renamer/#:prefix clauses"
                                   ispec))
               (lp args select hide
                   (match val
                     (#f #f)
                     (('symbol-prefix-proc ('quote prefix)) prefix)
                     (else
                      (syntax-violation 'use-module "unsupported #:renamer clause"
                                        ispec val)))
                   version))
              (#:prefix
               (when prefix
                 (syntax-violation 'use-module "too many #:renamer/#:prefix clauses"
                                   ispec))
               (lp args select hide
                   (match val
                     (#f #f)
                     ((? id?) val)
                     (else
                      (syntax-violation 'use-module "invalid #:prefix"
                                        ispec val)))
                   version))
              (#:version
               (when version
                 (syntax-violation 'use-module "too many #:version clauses"
                                   ispec))
               (lp args select hide prefix
                   (match val
                     (((? version-component?) ...) val)
                     (else
                      (syntax-violation 'use-module "invalid #:version"
                                        ispec val)))))
              (_
               (syntax-violation 'use-module "unrecognized keyword arg"
                                 ispec kw))))
           (_
            (syntax-violation 'use-module "missing keyword argument" ispec
                              kw)))))))
  (match (syntax->datum ispec)
    (((? id? mod) ...)
     (make-iset-library mod '()))
    ((((? id? mod) ...) arg ...)
     (call-with-values (lambda () (parse-args arg))
       (lambda (select hide prefix version)
         (let* ((iset (make-iset-library mod version))
                (iset (if (null? hide)
                          iset
                          (make-iset-except iset hide)))
                (iset (if select
                          (make-iset-only
                           iset
                           (map (match-lambda
                                  ((from . to) from)
                                  (var var))
                                select))
                          iset))
                (iset (if (and select (or-map pair? select))
                          (make-iset-rename
                           iset
                           (filter pair? select))
                          iset))
                (iset (if prefix
                          (make-iset-prefix iset prefix)
                          iset)))
           iset))))
    (_
     (syntax-violation 'use-module "invalid guile module import" ispec))))

(define* (finish-guile-imports imports #:key pure?)
  (if pure?
      imports
      (cons (make-iset-library '(guile) '()) imports)))

(define (make-guile-duplicate-handler duplicates)
  (define (symbol->duplicate-handler sym)
    (match sym
      ('check
       (lambda (name old new prev)
         (forbid-duplicate-bindings name old new)))
      ('first (lambda (name old new prev) (or prev old)))
      ('last (lambda (name old new prev) new))
      ('noop (lambda (name old new prev) #f))
      ;; FIXME: Implement Guile's replace logic.
      ('replace (lambda (name old new prev) new))
      ;; FIXME: Print warnings.  As of writing, warnings will be
      ;; printed *twice* due to how library compilation works.
      ;; Also, we don't have the necessary data to know if we are
      ;; overriding a core binding.
      ('warn (lambda (name old new prev) #f))
      ('warn-override-core (lambda (name old new prev) #f))
      ;; TODO: We don't support GOOPS yet.
      ('merge-generics
       (lambda (name old new prev) (error "GOOPS is unsupported")))
      ('merge-accessors
       (lambda (name old new prev) (error "GOOPS is unsupported")))))
  (let ((handlers (map symbol->duplicate-handler duplicates)))
    (lambda (name old new)
      (let lp ((prev #f) (handlers handlers))
        (match handlers
          (() (or prev (error "unresolved duplicate definition" name)))
          ((handler . rest)
           (match (handler name old new prev)
             (#f (lp prev rest))
             (binding (lp binding rest)))))))))

(define %default-guile-duplicate-handlers '(replace warn-override-core warn last))

(define (parse-guile-library head body trusted?)
  (define (parse-exports exports)
    ;; -> ((local . public) ...)
    (map (match-lambda
           ((? id? id) (cons id id))
           (((? id? from) . (? id? to)) (cons from to)))
         exports))

  (define (parse-autoload modname bindings)
    (match (syntax->datum modname)
      (((? id? modname) ...)
       (match (syntax->datum bindings)
         (((? id? bindings) ...)
          (make-iset-only (make-iset-library modname '()) bindings))
         (_
          (syntax-violation 'define-module "bad #:autoload bindings"
                            head bindings))))
      (_
       (syntax-violation 'define-module "bad #:autoload module name"
                         head modname))))

  (define (duplicate-resolver? x)
    (memq x '(check first last noop replace warn warn-override-core
                    merge-generics merge-accessors)))

  (define (parse-define-module-args args)
    (let parse ((args args)
                (imports '()) (exports '()) (version #f) (pure? #f)
                (duplicates %default-guile-duplicate-handlers))
      (syntax-case args ()
        (()
         (values (finish-guile-imports (reverse imports) #:pure? pure?)
                 (reverse exports)
                 version
                 (make-guile-duplicate-handler duplicates)))
        ((kw . args)
         (match (convert-sloppy-keyword (syntax->datum #'kw))
           (#:pure
            (parse #'args imports exports version #t duplicates))
           (#:no-backtrace
            ;; Ignore.
            (parse #'args imports exports version pure? duplicates))
           ((? keyword? kw')
            (syntax-case #'args ()
              ((val . args)
               (match kw'
                 (#:version
                  (when version
                    (syntax-violation 'define-module "duplicate #:version"
                                      head #'val))
                  (match (syntax->datum #'val)
                    (((? version-component? version) ...)
                     (parse #'args imports exports version pure? duplicates))
                    (_
                     (syntax-violation 'define-module "invalid #:version"
                                       head #'val))))
                 (#:duplicates
                  (match (syntax->datum #'val)
                    ((and ((? duplicate-resolver?) ..1) duplicates)
                     (parse #'args imports exports version pure? duplicates))
                    (_
                     (syntax-violation 'define-module "invalid #:duplicates"
                                       head #'val))))
                 (#:filename
                  ;; Ignore.
                  (parse #'args imports exports version pure? duplicates))
                 (#:declarative?
                  (syntax-case #'val ()
                    (#t (parse #'args imports exports version pure? duplicates))
                    (_
                     (syntax-violation
                      'define-module
                      "library-group only supports modules with #:declarative? #t"
                      head #'val))))
                 (#:use-module
                  (let ((ispec (parse-guile-import #'val)))
                    (parse #'args (cons ispec imports) exports version pure? duplicates)))
                 (#:use-syntax
                  (syntax-violation 'define-module "#:use-syntax not supported"
                                    head #'val))
                 ((or #:export #:re-export
                      #:export-syntax #:re-export-syntax
                      #:replace #:replace-syntax
                      #:re-export-and-replace)
                  (syntax-case #'val ()
                    ((spec ...)
                     (parse
                      #'args imports
                      (fold
                       (lambda (spec exports)
                         (syntax-case spec ()
                           (id
                            (id? (syntax->datum #'id))
                            (acons (syntax->datum #'id) (syntax->datum #'id)
                                   exports))
                           ((from . to)
                            (and (id? (syntax->datum #'from))
                                 (id? (syntax->datum #'to)))
                            (acons (syntax->datum #'from) (syntax->datum #'to)
                                   exports))
                           (_
                            (syntax-violation 'define-module "invalid export"
                                              head spec))))
                       exports #'(spec ...))
                      version pure? duplicates))
                    (_
                     (syntax-violation 'define-module "invalid export list"
                                       head #'val))))
                 (#:autoload
                  (syntax-case #'args ()
                    ((bindings . args)
                     (let ((ispec (parse-autoload #'val #'bindings)))
                       (parse #'args (cons ispec imports) exports version pure? duplicates)))
                    (_
                     (syntax-violation 'define-module "missing #:autoload bindings"
                                       head #'(val . args)))))
                 (_
                  (syntax-violation 'define-module "unrecognized keyword arg"
                                    head #'kw))))
              (_
               (syntax-violation 'define-module "missing value for keyword arg"
                                 head #'kw))))
           (_
            (syntax-violation 'define-module "expected a keyword arg"
                              head #'kw))))
        (_ (syntax-violation 'define-module "invalid form" head)))))

  (syntax-case head ()
    ((define-module (modname ...) arg ...)
     (and-map id? (syntax->datum #'(modname ...)))
     (call-with-values (lambda () (parse-define-module-args #'(arg ...)))
       (lambda (imports exports version duplicate-handler)
         (define src
           (and (syntax? #'define-module) (syntax-sourcev #'define-module)))
         (make-library src (syntax->datum #'(modname ...)) version trusted?
                       duplicate-handler exports imports body))))
    (_
     (syntax-violation 'define-module "invalid define-module form" head))))

(define* (parse-library forms trusted? #:key (include-file includes-forbidden)
                        (features '()))
  "Parse @var{forms} to a @code{<library>} record."
  (syntax-case forms ()
    (((library . body))
     (symbolic-match? library)
     (parse-r6rs-library #'(library . body) trusted?))
    (((define-library . body))
     (symbolic-match? define-library)
     (parse-r7rs-library #'(define-library . body) trusted? include-file
                         features))
    (((define-module modname . modargs) . body)
     (symbolic-match? define-module)
     (parse-guile-library #'(define-module modname . modargs) #'body
                          trusted?))
    (_
     (error "invalid module forms" forms))))

(define* (parse-library-group form #:key (include-file includes-forbidden)
                              (features '()))
  "Parse a @code{library-group} form to a @code{<library-group>} record,
processing includes.  No other expansion or analysis is performed beyond
syntactic validity."

  (define* (parse forms libraries #:key (trusted? #f))
    "For each form in @var{forms}, which should be a list of syntax objects,
process any includes, collecting the prefix of @code{<library>} forms
and then parsing the tail @code{<program>}, or @code{#f} if there is no
program."
    (syntax-case forms ()
      (() (values (reverse libraries) '()))
      ((form . forms)
       (syntax-case #'form ()
         (#:untrusted
          (parse #'forms libraries #:trusted? #f))
         ((library . _)
          (symbolic-match? library)
          (parse #'forms (cons (parse-r6rs-library #'form trusted?) libraries)
                 #:trusted? trusted?))
         ((define-library . _)
          (symbolic-match? define-library)
          (parse #'forms
                 (cons (parse-r7rs-library #'form trusted? include-file
                                           features)
                       libraries)
                 #:trusted? trusted?))
         ((include filename)
          (symbolic-match? include)
          (let ((included (include-file (syntax->datum #'filename))))
            (syntax-case included ()
              (((define-module modname . modargs) . body)
               (symbolic-match? define-module)
               (parse #'forms
                      (cons (parse-guile-library
                             #'(define-module modname . modargs) #'body
                             trusted?)
                            libraries)
                      #:trusted? trusted?))
              (_
               (parse (append included #'forms) libraries
                      #:trusted? trusted?)))))
         ((import import-spec ...)
          (symbolic-match? import)
          (values (reverse libraries)
                  (list
                   (make-program #f trusted? forbid-duplicate-bindings
                                 (parse-imports
                                  (syntax->datum #'(import-spec ...)))
                                 #'forms))))
         ((use-modules import-spec ...)
          (symbolic-match? use-modules)
          (values (reverse libraries)
                  (list
                   (make-program #f trusted?
                                 (make-guile-duplicate-handler
                                  %default-guile-duplicate-handlers)
                                 (finish-guile-imports
                                  (map parse-guile-import #'(import-spec ...))
                                  #:pure? #f)
                                 #'forms))))))))

  (syntax-case form ()
    ((library-group form ...)
     (symbolic-match? library-group)
     (let ((src (and (syntax? #'library-group)
                     (syntax-sourcev #'library-group))))
       (call-with-values (lambda () (parse #'(form ...) '() #:trusted? #t))
         (lambda (libraries programs)
           (make-library-group src libraries programs)))))
    (_
     (error "invalid library-group" form))))

(define (concatenate-library-groups a-group b-group)
  (match a-group
    (($ <library-group> a-src a-libraries a-programs)
     (match b-group
       (($ <library-group> b-src b-libraries b-programs)
        (make-library-group
         (or a-src b-src)
         (append a-libraries b-libraries)
         (append a-programs b-programs)))))))

(define* (link-library-group group #:key
                             (load-library (lambda* (name #:key (features '()))
                                             #f))
                             (features '())
                             (allow-dangling-import? (lambda (name) #f)))
  (define linked '()) ;; List of libraries.
  (define by-name (make-hash-table))

  (define (link-library! library)
    (let ((name (library-name library)))
      (when (hash-ref by-name name)
        (error "duplicate library definition" name))
      (hash-set! by-name name 'linking)
      (for-each link-import! (library-isets library))
      (set! linked (cons library linked))
      (hash-set! by-name name 'linked)))

  (define (link-import! iset)
    (let ((name (imported-library-name iset)))
      (match (hash-ref by-name name 'unvisited)
        ('linked (values))
        ('linking (error "cycle in module graph" name))
        ('unvisited
         (cond
          ((load-library name #:features features) => link-library!)
          ((allow-dangling-import? name) (values))
          (else (error "module not found" name)))))))

  (match group
    (($ <library-group> src libraries programs)
     (for-each link-library! libraries)
     (for-each (lambda (program)
                 (for-each link-import! (program-isets program)))
               programs)
     (make-library-group src (reverse linked) programs))))

(define-record-type <import>
  (make-import modname exported-name imported-name)
  import?
  (modname import-modname)
  (exported-name import-exported-name)
  (imported-name import-imported-name))

(define-record-type <lexical>
  (make-lexical sym)
  lexical?
  (sym lexical-sym))

(define-record-type <primitive>
  (make-primitive name)
  primitive?
  (name primitive-name))

(define-record-type <host-primitive>
  (make-host-primitive name)
  host-primitive?
  (name host-primitive-name))

(define-record-type <expand-time-value>
  (make-expand-time-value)
  expand-time-value?)

;; <value> := <lexical>
;;          | <primitive>
;;          | <host-primitive>
;;          | <expand-time-value>
(define-record-type <module-definitions>
  (make-module-definitions private public)
  module-definitions?
  ;; Hash table of symbol -> <value>.
  (private module-private-definitions)
  ;; Hash table of symbol -> <value>.
  (public module-public-definitions))

(define-record-type <definition>
  (make-definition name sym val)
  definition?
  (name definition-name)
  (sym definition-sym)
  (val definition-val))

(define-record-type <statement>
  (make-statement exp)
  statement?
  (exp statement-exp))

(define (forbid-duplicate-bindings name old new)
  (match old
    ((? import?) (error "duplicate imports" old new))
    (_ (error "duplicate definition" name))))

;; FIXME: Get this exported from (language tree-il primitives).
(define (primitive-for-variable box)
  (hashq-ref (@@ (language tree-il primitives) *interesting-primitive-vars*)
             box))

(define* (expand-library call-with-target mod form
                         #:key (residualize-syntax-transformers? #f))
  "Expand the syntax object @var{form} in the module @var{mod}.

The term will be expanded twice: once to create the expand-time module,
which will then be evaluated directly, and once to residualize a Tree-IL
term for the compilation unit.

Syntax transformers (macros) will be evaluated at expansion-time.  By
default, they are not residualized into the compilation unit."
  (save-module-excursion
   (lambda ()
     (set-current-module mod)
     (primitive-eval (macroexpand form 'e '(compile eval)))
     (call-with-target
      (lambda ()
        (macroexpand form 'c (if residualize-syntax-transformers?
                                 '(load)
                                 '())))))))

(define* (expand-program call-with-target mod form
                         #:key (residualize-syntax-transformers? #f))
  "Expand the syntax object @var{form} in the module @var{mod}.

Syntax transformers (macros) will be evaluated at expansion-time.  By
default, they are not residualized into the compilation unit."
  (save-module-excursion
   (lambda ()
     (set-current-module mod)
     (call-with-target
      (lambda ()
        (macroexpand form 'c (if residualize-syntax-transformers?
                                 '(load compile)
                                 '(compile))))))))

(define (trivial-residualize-handlers)
  (define (lift state)
    state)
  (define (add-definition name sym val state)
    (cons (make-definition name sym val) state))
  (define (add-statement exp state)
    (cons (make-statement exp) state))
  (define (lower state)
    state)
  (values lift add-definition add-statement lower))

(define* (expand-library-group group #:key
                               (call-with-target (lambda (f) (f)))
                               (rewrite-primitive
                                (lambda (exp make-module-ref) exp))
                               (rewrite-host-reference
                                (lambda (exp make-module-ref)
                                  (error "unknown host reference" exp)))
                               (rewrite-host-primitive
                                (lambda (src name make-module-ref)
                                  (error
                                   "reference to host primitive in generated code"
                                   src name)))
                               (residualize-handlers
                                (lambda _ (trivial-residualize-handlers)))
                               (missing-library (lambda (modname) #f))
                               (primitives #f)
                               (residualize-syntax-transformers? #f))
  "Take a @code{<library-group>} record and expand it to a big
@code{letrec*}.
The libraries in the group are expanded one-by-one.  Expanding a library
residualises a Tree-IL AST node as part of the compilation unit, and
additionally populates a compile-time host module with definitions.  If
expanding a module needs compile-time values from another module, it
uses the bindings in the host module.

All definitions and expressions in the expanded libraries are then
rewritten to be part of a big @code{letrec*}, and top-level and module
references in those definitions and expressions are rewritten to use
lexical references.

The final program in the @code{<library-group>} is given the same
treatment, except that its final expression (if any) is evaluated in
tail position."
  ;; A mapping from module,name,public? tuple to <binding> record, for
  ;; all modules in the library group.
  (define module-definitions (make-hash-table))
  (define (add-module-definitions! modname)
    (when (hash-ref module-definitions modname)
      (error "duplicate module" modname))
    (define defs
      (make-module-definitions (make-hash-table) (make-hash-table)))
    (hash-set! module-definitions modname defs)
    defs)
  (define (lookup-module-definitions modname)
    (or (hash-ref module-definitions modname)
        (begin
          (missing-library modname)
          (error "unknown module" modname))))

  (define (add-definition! defs name public? duplicate-handler value)
    (match defs
      (($ <module-definitions> private public)
       (let* ((t (if public? public private))
              (existing (hashq-ref t name))
              (value (if existing
                         (duplicate-handler name existing value)
                         value)))
         (match (hashq-ref t name)
           (#f (hashq-set! t name value))
           (existing
            (hashq-set! t name (duplicate-handler name existing value))))))))
  (define (lookup-definition defs name public?)
    (match defs
      (($ <module-definitions> private public)
       (hashq-ref (if public? public private) name))))

  ;; Add definitions from primitive module.
  (when primitives
    (let ((defs (add-module-definitions! primitives)))
      (module-for-each
       (lambda (name box)
         (add-definition! defs name #t #f
                          (match (primitive-for-variable box)
                            (#f (make-host-primitive name))
                            (name (make-primitive name)))))
       (resolve-interface primitives))))

  (define (parse-isets isets trusted?)
    (define parse-iset
      (match-lambda
        (($ <iset-only> iset select)
         (filter (match-lambda
                   (($ <import> mod-name exported imported)
                    (memq imported select)))
                 (parse-iset iset)))
        (($ <iset-except> iset hide)
         (filter (match-lambda
                   (($ <import> mod-name exported imported)
                    (not (memq imported hide))))
                 (parse-iset iset)))
        (($ <iset-prefix> iset prefix)
         (map (match-lambda
                (($ <import> mod-name exported imported)
                 (let ((renamed (symbol-append prefix imported)))
                   (make-import mod-name exported renamed))))
              (parse-iset iset)))
        (($ <iset-rename> iset renamings)
         (map (match-lambda
                (($ <import> mod-name exported imported)
                 (define renamed
                   (or (assq-ref renamings imported) imported))
                 (make-import mod-name exported renamed)))
              (parse-iset iset)))
        (($ <iset-library> modname version)
         (unless (null? version)
           (error "version references unsupported"))
         (when (equal? modname primitives)
           (unless trusted?
             (error "untrusted module cannot import primitives")))
         (let ((exports (module-public-definitions
                         (lookup-module-definitions modname))))
           (define (id<? a b)
             (string<? (symbol->string a) (symbol->string b)))
           (define (import<? a b)
             (id<? (import-exported-name a) (import-exported-name b)))
           (sort (hash-map->list (lambda (name binding)
                                   (make-import modname name name))
                                 exports)
                 import<?)))))
    (append-map parse-iset isets))

  ;; Because each invocation of expand-library-group gets its own
  ;; namespace, we don't have to deal with lingering definitions from
  ;; any previous expansion; all modules defined by this compilation
  ;; unit are fresh.  This also allows expansion to happen in parallel.
  (define namespace (gensym "%library-group"))

  (define (host-modname? modname)
    (match modname
      (('% (? (lambda (ns) (eq? ns namespace))) . tail) #f)
      (_ #t)))
  (define (annotate-modname modname)
    (if (equal? modname primitives)
        modname
        (cons* '% namespace modname)))
  (define (strip-modname modname)
    (match modname
      (('% (? (lambda (x) (eq? x namespace))) . modname) modname)
      (_
       (unless (equal? modname primitives)
         (error "unexpected modname" modname))
       modname)))

  (define (make-namespaced-module-ref src mod name public?)
    (make-module-ref src (annotate-modname mod) name public?))
  (define (make-namespaced-module-set src mod name public? val)
    (make-module-set src (annotate-modname mod) name public? val))

  (define (make-expand-time-module modname filename version imports exports
                                   duplicate-handler)
    "Create the host module in which to store compile-time library
definitions.  The module may import other host libraries."
    (define imports-by-module (make-hash-table))
    (define (add-import! modname exported imported)
      (define tail (hash-ref imports-by-module modname '()))
      (define entry (cons exported imported))
      (hash-set! imports-by-module modname (cons entry tail)))
    (for-each (match-lambda
                (($ <import> modname exported imported)
                 (add-import! modname exported imported)))
              imports)
    (define (id<? a b)
      (string<? (symbol->string a) (symbol->string b)))
    (define (modname<? a b)
      (match a
        (() #t)
        ((a . a*) (match b
                    (() #f)
                    ((b  . b*) (and (id<? a b) (modname<? a* b*)))))))
    (define module-import-decls
      (sort (hash-map->list (lambda (modname entries)
                              (list (annotate-modname modname)
                                    #:select
                                    (sort entries
                                          (lambda (a b)
                                            (id<? (car a) (car b))))))
                            imports-by-module)
            (lambda (a b)
              (modname<? (car a) (car b)))))
    (define-values (module-export-decls module-re-export-decls)
      (let ()
        (define imports-by-name (make-hash-table))
        (for-each (match-lambda
                    ((and import ($ <import> _ _ imported))
                     (match (hashq-ref imports-by-name imported)
                       (#f (hashq-set! imports-by-name imported import))
                       (existing
                        (hashq-set! imports-by-name imported
                                    (duplicate-handler imported existing import))))))
                  imports)
        (partition (match-lambda
                     ((local . public) (not (hashq-ref imports-by-name local))))
                   exports)))
    (define-module* (annotate-modname modname)
      #:filename filename
      #:pure #t
      #:version version
      #:imports module-import-decls
      #:exports module-export-decls
      #:re-exports module-re-export-decls
      #:declarative? #t))

  (define (tree-il-for-each f exp)
    (define fold (make-tree-il-folder))
    (fold exp (lambda (exp) (values)) f))

  (define (tree-il->reversed-bindings exp modname imports exports
                                      duplicate-handler
                                      add-definition add-statement
                                      bindings)
    "Given the expanded library @var{exp}, as a Tree-IL node, transform it to
a sequence of definitions and expressions, as @code{<binding>} nodes.
Rewrite references to other top-level bindings to refer to primitive
or lexical definitions.  Append those @code{<binding>} nodes to
@var{bindings}, in reverse order.  The @var{duplicate-handler}
procedure is called to resolve issues of duplicate definitions."
    ;; Make defs for module.
    (define defs (add-module-definitions! modname))

    (define (has-expand-time-value? name)
      (module-variable (resolve-module (annotate-modname modname)) name))

    ;; Add definitions for imports.
    (for-each (match-lambda
                (($ <import> imod exported imported)
                 (match (lookup-definition (lookup-module-definitions imod)
                                           exported #t)
                   (#f (error "unknown import?" imod exported))
                   (value (add-definition! defs imported #f duplicate-handler
                                           value)))))
              imports)

    ;; Prohibit set! to imports.  Check module on expanded toplevel defs
    ;; and uses.
    (tree-il-for-each (match-lambda
                        (($ <toplevel-define> src mod name val)
                         (unless (equal? (strip-modname mod) modname)
                           (error "unexpected mod" exp mod modname))
                         (values))
                        (($ <toplevel-ref> src mod name)
                         (unless (equal? (strip-modname mod) modname)
                           (error "unexpected mod" exp mod modname))
                         (values))
                        (($ <toplevel-set> src mod name val)
                         (unless (equal? (strip-modname mod) modname)
                           (error "unexpected mod" exp mod modname))
                         (when (lookup-definition defs name #f)
                           (error "set! to imported binding" src name))
                         (values))
                        (_ (values)))
                      exp)

    ;; Record local definitions and allocate lexicals for them.
    (tree-il-for-each (match-lambda
                        (($ <toplevel-define> src mod name exp)
                         (add-definition! defs name #f duplicate-handler
                                          (make-lexical (gensym "top")))
                         (values))
                        (_ (values)))
                      exp)

    ;; Check for unbound top-levels.
    (tree-il-for-each (match-lambda
                        (($ <toplevel-ref> src mod name)
                         (unless (lookup-definition defs name #f)
                           (error "unbound top-level" src name))
                         (values))
                        (($ <toplevel-set> src mod name val)
                         (unless (lookup-definition defs name #f)
                           (error "unbound top-level" src name))
                         (values))
                        (_ (values)))
                      exp)

    ;; Find local definitions for exports.
    (for-each (match-lambda
                ((local . exported)
                 (match (lookup-definition defs local #f)
                   (#f
                    ;; An export without a binding in the compilation
                    ;; unit.  Perhaps it is an expansion-time binding.
                    (unless (has-expand-time-value? local)
                      (error "missing definition for export"
                             modname local exported))
                    (let ((val (make-expand-time-value)))
                      (add-definition! defs local #f duplicate-handler val)
                      (add-definition! defs exported #t duplicate-handler val)))
                   (val (add-definition! defs exported #t duplicate-handler val)))))
              exports)

    ;; Resolve references to local definitions to lexical-ref or
    ;; primitive-ref.
    (define (rewrite-toplevels exp)
      (post-order
       (match-lambda
         (($ <toplevel-ref> src mod name)
          (match (lookup-definition defs name #f)
            (($ <lexical> sym) (make-lexical-ref src name sym))
            (($ <primitive> name) (make-primitive-ref src name))
            (($ <host-primitive> name)
             (rewrite-host-primitive src name
                                     make-namespaced-module-ref))
            (($ <expand-time-value>)
             (error "reference to expansion-time value in generated code"
                    src modname name))))
         (($ <toplevel-set> src mod name val)
          (match (lookup-definition defs name #f)
            (($ <lexical> sym) (make-lexical-set src name sym val))
            (($ <host-primitive> name)
             (error "assignment to host primitive" name))
            (($ <expand-time-value>)
             (error "reference to expansion-time value in generated code"
                    src modname name))))
         (($ <toplevel-define> src mod name val)
          (error "nested toplevel-define" src mod name val))
         (exp exp))
       exp))

    ;; Walk the chain of <seq> and <toplevel-define> to extract
    ;; definitions and statements.
    (define (visit-top-level exp bindings)
      (match exp
        (($ <toplevel-define> src mod name val)
         (match (lookup-definition defs name #f)
           (($ <lexical> sym)
            (add-definition name sym (rewrite-toplevels val)
                            bindings))))

        (($ <seq> src head tail)
         (visit-top-level tail (visit-top-level head bindings)))

        ;; Could fold in let and letrec* bindings.  Dunno.
        (_ (add-statement (rewrite-toplevels exp) bindings))))

    (define (patch-primitives exp)
      ;; Fix up two kinds of anamolies in expanded code:
      ;;
      ;;   1. Primcalls introduced by the expander, but which might not
      ;;      correspond to primitives for the target.  For example,
      ;;      list->vector as emitted by #'#(foo).
      ;;
      ;;   2. Reference to top-level host bindings in the expander
      ;;      itself, as in #'(list->vector foo).  This happens a lot in
      ;;      Guile's expander.
      (define (patch exp)
        (match exp
          (($ <primitive-ref> src name)
           (match (rewrite-primitive exp make-namespaced-module-ref)
             (#f (error "bad rewrite-primitive return"))
             ((and exp ($ <primitive-ref>)) exp)
             (exp (patch exp))))
          (($ <primcall> src name args)
           (match (patch (make-primitive-ref src name))
             (($ <primitive-ref> src name)
              (make-primcall src name args))
             (exp (make-call src exp args))))
          (($ <module-ref> src (? host-modname? mod) name #f)
           ;; A primitive reference introduced by a primitive syntax
           ;; expander.
           (match (rewrite-host-reference exp make-namespaced-module-ref)
             (#f (error "bad rewrite-host-reference return"))
             (($ <module-ref> src (? host-modname? mod) name #f)
              (error "residual host reference after rewrite" src mod name))
             (exp (patch exp))))
          (($ <call> src ($ <primitive-ref> _ name) args)
           (make-primcall src name args))
          (_ exp)))
      (post-order patch exp))

    (visit-top-level (patch-primitives exp) bindings))

  (define (srcv-filename srcv)
    (match srcv
      (#f #f)
      (#(filename line column) filename)))

  (define (module->reversed-bindings src modname version imports exports
                                     duplicate-handler body expand)
    (define filename (srcv-filename src))
    (define ctmod
      (make-expand-time-module modname filename version imports exports
                               duplicate-handler))
    (define expanded
      (expand call-with-target ctmod #`(begin . #,body)
                      #:residualize-syntax-transformers?
                      residualize-syntax-transformers?))
    (define-values (lift add-statement add-definition lower)
      (residualize-handlers src modname version
                            imports exports duplicate-handler
                            make-namespaced-module-ref
                            make-namespaced-module-set))
    (lower
     (tree-il->reversed-bindings expanded modname imports exports
                                 duplicate-handler
                                 add-statement add-definition
                                 (lift '()))))

  (define (library->reversed-bindings library bindings)
    "Given the R6RS library @var{form}, as a syntax object, parse out the
imports and exports, create a compile-time module, and expand the body
of the library within that module.  Collect residual definitions and
statements into a reversed list."
    (match library
      (($ <library> src modname version trusted? duplicate-handler exports
          isets body)
       (define imports (parse-isets isets trusted?))
       (append (module->reversed-bindings src modname version imports exports
                                          duplicate-handler body
                                          expand-library)
               bindings))))

  (define (program->reversed-bindings program bindings)
    "Same as @code{library->reversed-bindings}, but for a program.
@var{imports} is already parsed, as a list of @code{<import>}.  A new
module with a fresh name will be defined for the purposes of expanding"
    (match program
      (($ <program> src trusted? duplicate-handler isets body)
       (define modname (list (gensym "library-group-program")))
       (define imports (parse-isets isets trusted?))
       (append (module->reversed-bindings src modname '() imports '()
                                          duplicate-handler body
                                          expand-program)
               bindings))))

  (define (resolve-references bindings)
    (define (resolve exp)
      ;; Check for unbound top-levels.
      (tree-il-for-each
       (match-lambda
         (($ <module-ref> src mod name public?)
          (unless (or (host-modname? mod)
                      (let ((defs (lookup-module-definitions
                                   (strip-modname mod))))
                        (lookup-definition defs name public?)))
            (error "unbound macro-introduced top-level for module"
                   src (strip-modname mod) name))
          (values))
         (($ <module-set> src mod name public? val)
          (unless (let ((defs (lookup-module-definitions
                               (strip-modname mod))))
                    (lookup-definition defs name public?))
            (error "unbound macro-introduced top-level for module"
                   src (strip-modname mod) name))
          (values))
         (_ (values)))
       exp)

      ;; Resolve explicit cross-module references to lexical-ref or
      ;; primitive-ref.  Do this now instead of while processing modules
      ;; one by one to allow for cyclical references introduced by the
      ;; expander.
      (define (resolve-one exp)
        (match exp
          (($ <module-ref> src mod name public?)
           (let ((defs (lookup-module-definitions (strip-modname mod))))
             (match (lookup-definition defs name public?)
               (($ <lexical> sym) (make-lexical-ref src name sym))
               (($ <primitive> name) (make-primitive-ref src name))
               (($ <host-primitive> name)
                (resolve-one
                 (rewrite-host-primitive src name
                                         make-namespaced-module-ref)))
               (($ <expand-time-value>)
                (error "reference to expansion-time value in generated code"
                       src mod name)))))
          (($ <module-set> src mod name public? val)
           (let ((defs (lookup-module-definitions (strip-modname mod))))
             (match (lookup-definition defs name public?)
               (($ <lexical> sym) (make-lexical-set src name sym val))
               (($ <expand-time-value>)
                (error "reference to expansion-time value in generated code"
                       src mod name)))))
          ((or ($ <toplevel-ref>) ($ <toplevel-set>) ($ <toplevel-define>))
           (error "unexpected toplevel" exp))
          (($ <call> src ($ <primitive-ref> _ name) args)
           (expand-primcall (make-primcall src name args)))
          (_ exp)))

      (post-order resolve-one exp))

    (map (match-lambda
           (($ <statement> exp)
            (make-statement (resolve exp)))
           (($ <definition> name sym exp)
            (make-definition name sym (resolve exp))))
         bindings))

  (define (ensure-tail-expression reversed-bindings)
    (match reversed-bindings
      ((($ <statement>) . _) reversed-bindings)
      (_
       (cons (make-statement (make-void #f)) reversed-bindings))))

  (define reversed-bindings
    (match group
      (($ <library-group> src libraries programs)
       (ensure-tail-expression
        (resolve-references
         (fold program->reversed-bindings
               (fold library->reversed-bindings '() libraries)
               programs))))))

  ;; Remove the modules we just expanded from the global module tree.
  (hashq-remove! (module-submodules (resolve-module '(%))) namespace)

  (match reversed-bindings
    ((($ <statement> tail) . bindings)
     (let ((bindings (reverse bindings)))
       (make-letrec (library-group-src group)
                    #t                  ; in-order?
                    (map (match-lambda
                           (($ <definition> name sym val) name)
                           (($ <statement> exp) '_))
                         bindings)
                    (map (match-lambda
                           (($ <definition> name sym val) sym)
                           (($ <statement> exp) (gensym "_")))
                         bindings)
                    (map (match-lambda
                           (($ <definition> name sym val) val)
                           (($ <statement> exp)
                            (if (void? exp)
                                exp
                                (make-seq #f exp (make-void #f)))))
                         bindings)
                    tail)))))
