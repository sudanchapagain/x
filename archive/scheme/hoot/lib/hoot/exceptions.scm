;;; Exception definitions
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
;;; Exception constructors for common errors.
;;;
;;; Code:

(library (hoot exceptions)
  (export &exception simple-exception?
          &compound-exception make-compound-exception compound-exception?
          compound-exception-components

          define-exception-type

          simple-exceptions make-exception exception?

          &message make-exception-with-message exception-with-message?
          exception-message

          &warning make-warning warning?

          &serious make-serious-exception serious-exception?

          &error make-error error?

          &external-error make-external-error external-error?

          &violation make-violation violation?

          &assertion make-assertion-violation assertion-violation?

          &arity-violation make-arity-violation arity-violation?

          &implementation-restriction make-implementation-restriction-violation
          implementation-restriction-violation?

          &failed-type-check make-failed-type-check failed-type-check?
          failed-type-check-predicate

          &non-continuable make-non-continuable-violation
          non-continuable-violation?

          &irritants make-exception-with-irritants exception-with-irritants?
          exception-irritants

          &origin make-exception-with-origin exception-with-origin?
          exception-origin

          &source make-exception-with-source exception-with-source?
          exception-source-file exception-source-line
          exception-source-column

          &lexical make-lexical-violation lexical-violation?

          &i/o make-i/o-error i/o-error?

          &i/o-line-and-column make-i/o-line-and-column-error
          i/o-line-and-column-error? i/o-error-line i/o-error-column

          &i/o-filename make-i/o-filename-error i/o-filename-error?
          i/o-error-filename

          &i/o-not-seekable make-i/o-not-seekable-error i/o-not-seekable-error?

          &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

          &syntax make-syntax-error syntax-error?

          &invalid-syntax make-invalid-syntax-error invalid-syntax-error?
          invalid-syntax-form invalid-syntax-subform)
  (import (hoot syntax)
          (hoot features)
          (hoot cond-expand)
          (hoot errors)
          (hoot eq)
          (hoot inline-wasm)
          (hoot pairs)
          (hoot procedures)
          (hoot lists)
          (hoot values)
          (hoot records)
          (hoot syntax-objects)
          (hoot match))

  (define-record-type &exception
    #:extensible? #t
    (make-&exception)
    simple-exception?)
  (define-record-type &compound-exception
    (make-compound-exception components)
    compound-exception?
    (components compound-exception-components))

  (define (simple-exceptions exception)
    "Return a list of the simple exceptions that compose the exception
object @var{exception}."
    (cond ((compound-exception? exception)
           (compound-exception-components exception))
          ((simple-exception? exception)
           (list exception))
          (else
           (raise (make-type-error exception 'exception? 'simple-exceptions)))))

  (define (make-exception . exceptions)
    "Return an exception object composed of @var{exceptions}."
    (define (flatten exceptions)
      (if (null? exceptions)
          '()
          (append (simple-exceptions (car exceptions))
                  (flatten (cdr exceptions)))))
    (let ((simple (flatten exceptions)))
      (if (and (pair? simple) (null? (cdr simple)))
          (car simple)
          (make-compound-exception simple))))

  (define (exception? obj)
    "Return true if @var{obj} is an exception object."
    (or (compound-exception? obj) (simple-exception? obj)))

  (define-syntax define-exception-type
    (lambda (stx)
      (define (parent-fields parent)
        (let-values (((kind value) (syntax-local-binding parent)))
          (datum->syntax
           parent
           (or (and (eq? kind 'macro)
                    (procedure-property value 'record-type?)
                    (procedure-property value 'fields))
               '()))))
      (syntax-case stx ()
        ((define-exception-type exn parent
           make-exn
           exn?
           (field exn-field)
           ...)
         (with-syntax (((pfield ...) (parent-fields #'parent))
                       ((%exn-field ...)
                        (generate-temporaries #'(exn-field ...))))
           #'(begin
               (define-record-type exn
                 #:parent parent #:extensible? #t
                 (make-exn pfield ... field ...)
                 %exn?
                 (field %exn-field)
                 ...)
               (define (exn? x)
                 (or (%exn? x)
                     (and (compound-exception? x)
                          (let lp ((simple (compound-exception-components x)))
                            (match simple
                              (() #f)
                              ((x . simple)
                               (or (%exn? x)
                                   (lp simple))))))))
               (define (exn-field x)
                 (if (%exn? x)
                     (%exn-field x)
                     (let lp ((simple (compound-exception-components x)))
                       (match simple
                         (() (raise (make-type-error x 'exn-field 'exn?)))
                         ((x . simple)
                          (if (%exn? x)
                              (%exn-field x)
                              (lp simple)))))))
               ...))))))

  (define-exception-type &message &exception
    make-exception-with-message
    exception-with-message?
    (message exception-message))
  (define-exception-type &warning &exception
    make-warning
    warning?)
  (define-exception-type &serious &exception
    make-serious-exception
    serious-exception?)
  (define-exception-type &error &serious
    make-error
    error?)
  (define-exception-type &external-error &error
    make-external-error
    external-error?)
  (define-exception-type &violation &serious
    make-violation
    violation?)
  (define-exception-type &assertion &violation
    make-assertion-violation
    assertion-violation?)
  (define-exception-type &arity-violation &violation
    make-arity-violation
    arity-violation?)
  (define-exception-type &implementation-restriction &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)
  (define-exception-type &failed-type-check &assertion
    make-failed-type-check
    failed-type-check?
    (predicate failed-type-check-predicate))
  (define-exception-type &non-continuable &violation
    make-non-continuable-violation
    non-continuable-violation?)
  (define-exception-type &irritants &exception
    make-exception-with-irritants
    exception-with-irritants?
    (irritants exception-irritants))
  (define-exception-type &origin &exception
    make-exception-with-origin
    exception-with-origin?
    (origin exception-origin))
  (define-exception-type &source &exception
    make-exception-with-source
    exception-with-source?
    (file exception-source-file)
    (line exception-source-line)
    (column exception-source-column))
  (define-exception-type &lexical &violation
    make-lexical-violation
    lexical-violation?)
  (define-exception-type &i/o &error
    make-i/o-error
    i/o-error?)
  (define-exception-type &i/o-line-and-column &i/o
    make-i/o-line-and-column-error
    i/o-line-and-column-error?
    (line i/o-error-line)
    (column i/o-error-column))
  (define-exception-type &i/o-filename &i/o
    make-i/o-filename-error
    i/o-filename-error?
    (filename i/o-error-filename))
  (define-exception-type &i/o-not-seekable &i/o
    make-i/o-not-seekable-error
    i/o-not-seekable-error?)
  (define-exception-type &i/o-port &i/o
    make-i/o-port-error
    i/o-port-error?
    (port i/o-error-port))
  (define-exception-type &syntax &violation
    make-syntax-error
    syntax-error?)
  (define-exception-type &invalid-syntax &syntax
    make-invalid-syntax-error
    invalid-syntax-error?
    (form invalid-syntax-form)
    (subform invalid-syntax-subform))

  (cond-expand
   (guile-vm)
   (hoot-main
    (let ()
      (define (make-with-irritants exn message origin irritants)
        (make-exception exn
                        (make-exception-with-message message)
                        (make-exception-with-origin origin)
                        (make-exception-with-irritants irritants)))
      (define-syntax-rule (define-exception-constructor (name arg ...) body ...)
        (cond-expand
         ((and) (define (name arg ...) body ...))
         (else (define (name arg ...) (list arg ...)))))
      (define-exception-constructor (make-size-error val max who)
        (make-with-irritants (make-error) "size out of range" who (list val)))
      (define-exception-constructor (make-index-error val size who)
        (make-with-irritants (make-error) "index out of range" who (list val)))
      (define-exception-constructor (make-range-error val min max who)
        (make-with-irritants (make-error) "value out of range" who (list val)))
      (define-exception-constructor (make-start-offset-error val size who)
        (make-with-irritants (make-error) "start offset out of range" who
                             (list val)))
      (define-exception-constructor (make-end-offset-error val size who)
        (make-with-irritants (make-error) "end offset out of range" who
                             (list val)))
      (define-exception-constructor (make-type-error val who what)
        (make-with-irritants (make-failed-type-check what)
                             "type check failed"
                             who (list val)))
      (define-exception-constructor (make-unimplemented-error who)
        (make-exception (make-implementation-restriction-violation)
                        (make-exception-with-message "unimplemented")
                        (make-exception-with-origin who)))
      (define-exception-constructor (make-assertion-error expr who)
        (make-with-irritants (make-assertion-violation) "assertion failed"
                             who (list expr)))
      (define-exception-constructor (make-not-seekable-error port who)
        (make-exception (make-i/o-not-seekable-error)
                        (make-i/o-port-error port)
                        (make-exception-with-origin who)))
      (define-exception-constructor (make-runtime-error-with-message msg)
        (make-exception (make-error) (make-exception-with-message msg)))
      (define-exception-constructor (make-runtime-error-with-message+irritants
                                     msg irritants)
        (make-exception (make-error)
                        (make-exception-with-message msg)
                        (make-exception-with-irritants irritants)))
      (define-exception-constructor (make-match-error v)
        (make-exception (make-assertion-violation)
                        (make-exception-with-message "value failed to match")
                        (make-exception-with-irritants (list v))))
      (define-exception-constructor (make-arity-error v who)
        (define (annotate-with-origin exn)
          (if who
              (make-exception (make-exception-with-origin who) exn)
              exn))
        (annotate-with-origin
         (make-exception (make-arity-violation)
                         (make-exception-with-message
                          "wrong number of arguments")
                         (make-exception-with-irritants (list v)))))
      (define-exception-constructor (make-invalid-keyword-error kw)
        (make-exception (make-arity-violation)
                        (make-exception-with-message
                         "expected a keyword")
                        (make-exception-with-irritants (list kw))))
      (define-exception-constructor (make-unrecognized-keyword-error kw)
        (make-exception (make-arity-violation)
                        (make-exception-with-message
                         "unexpected keyword")
                        (make-exception-with-irritants (list kw))))
      (define-exception-constructor (make-missing-keyword-argument-error kw)
        (make-exception (make-arity-violation)
                        (make-exception-with-message
                         "keyword missing an argument")
                        (make-exception-with-irritants (list kw))))
      (define-exception-constructor (make-syntax-violation who message form
                                                           subform)
        (make-exception (if form
                            (make-invalid-syntax-error form subform)
                            (make-syntax-error))
                        (make-exception-with-message message)
                        (make-exception-with-origin who)))

      (define (annotate-with-source exn file line column)
        (if (exception? exn)
            (make-exception exn (make-exception-with-source file line column))
            exn))

      (define-syntax-rule (initialize-globals (global type proc) ...)
        (%inline-wasm
         '(func (param global type) ...
                (global.set global (local.get global)) ...)
         proc ...))
      (define-syntax-rule (initialize-proc-globals (global proc) ...)
        (initialize-globals (global (ref $proc) proc) ...))
      (initialize-proc-globals
       ($make-size-error make-size-error)
       ($make-index-error make-index-error)
       ($make-range-error make-range-error)
       ($make-start-offset-error make-start-offset-error)
       ($make-end-offset-error make-end-offset-error)
       ($make-type-error make-type-error)
       ($make-unimplemented-error make-unimplemented-error)
       ($make-assertion-error make-assertion-error)
       ($make-not-seekable-error make-not-seekable-error)
       ($make-runtime-error-with-message make-runtime-error-with-message)
       ($make-runtime-error-with-message+irritants
        make-runtime-error-with-message+irritants)
       ($make-match-error make-match-error)
       ($make-arity-error make-arity-error)
       ($make-invalid-keyword-error make-invalid-keyword-error)
       ($make-unrecognized-keyword-error make-unrecognized-keyword-error)
       ($make-missing-keyword-argument-error
        make-missing-keyword-argument-error)
       ($make-syntax-violation make-syntax-violation)
       ($annotate-with-source annotate-with-source))))
   (else)))
