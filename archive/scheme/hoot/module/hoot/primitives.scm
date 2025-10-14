;;; Hoot primitives
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
;;; This file exists only to be a place to define primitives for use by
;;; Hoot user code.  It also exports implementation-detail primitives
;;; for use by the Hoot standard library; eventually this will change to
;;; avoid exposing these nonstandard primitives to users.
;;;
;;; Code:

(define-module (hoot primitives)
  #:pure
  #:use-module (hoot frontend)
  #:use-module ((guile)
                #:select
                (canonicalize-path
                 search-path
                 define-syntax-rule
                 syntax-case syntax quasisyntax unsyntax unsyntax-splicing
                 syntax->datum datum->syntax identifier?
                 generate-temporaries free-identifier=? bound-identifier=?
                 with-syntax identifier-syntax make-variable-transformer
                 syntax-violation procedure-property
                 lambda* case-lambda* define*

                 call-with-prompt abort-to-prompt
                 ash logand logior logxor lognot logtest logbit?
                 keyword?
                 bitvector?
                 cons*
                 fluid? fluid-ref fluid-set! with-fluid* with-dynamic-state
                 make-variable variable-ref variable-set!
                 keyword->symbol symbol->keyword
                 exact->inexact
                 error
                 raise-exception
                 eval-when
                 make-struct/simple struct? struct-vtable
                 struct-ref struct-set!
                 gensym
                 string-utf8-length
                 make-record-type record-type-parents
                 struct-vtable? vtable-index-printer
                 make-fluid
                 make-parameter parameter? parameter-fluid parameter-converter
                 hashq hashv hash
                 make-regexp regexp? regexp-exec
                 string-copy
                 pk

                 ;; Helpers for syntax-module-bindings.
                 resolve-module variable-bound? macro?
                 module-for-each module-uses))
  #:use-module ((system syntax internal)
                #:select (syntax-local-binding
                          make-syntax syntax?
                          syntax-expression syntax-wrap
                          syntax-module syntax-sourcev))
  #:use-module ((system base target) #:select (target-runtime))
  ;; A bug in Guile: the public interface of (guile) uses (ice-9 ports),
  ;; which should re-export all its bindings, but #:select doesn't work
  ;; on interfaces that use interfaces.  For now, import the-eof-object
  ;; from (ice-9 ports) instead.
  #:use-module ((ice-9 ports) #:select (the-eof-object %make-void-port))
  #:use-module ((ice-9 atomic)
                #:select
                (make-atomic-box
                 atomic-box-ref atomic-box-set!
                 atomic-box-swap! atomic-box-compare-and-swap!))
  #:use-module ((ice-9 regex)
                #:select (regexp-match?
                          match:string
                          match:start
                          match:end
                          match:count
                          match:substring))
  #:use-module ((rnrs bytevectors)
                #:select
                (make-bytevector
                 bytevector?
                 bytevector-copy!
                 bytevector-length
                 bytevector-u8-ref bytevector-u8-set!
                 bytevector-s8-ref bytevector-s8-set!
                 bytevector-u16-native-ref bytevector-u16-native-set!
                 bytevector-s16-native-ref bytevector-s16-native-set!
                 bytevector-u32-native-ref bytevector-u32-native-set!
                 bytevector-s32-native-ref bytevector-s32-native-set!
                 bytevector-u64-native-ref bytevector-u64-native-set!
                 bytevector-s64-native-ref bytevector-s64-native-set!
                 bytevector-ieee-single-native-ref
                 bytevector-ieee-single-native-set!
                 bytevector-ieee-double-native-ref
                 bytevector-ieee-double-native-set!

                 string->utf8 utf8->string))
  #:use-module ((scheme base)
                #:select
                (_
                 ... => else
                 lambda
                 define let let* letrec letrec*
                 or and
                 begin
                 if cond case when unless
                 do
                 set!
                 quote quasiquote unquote unquote-splicing
                 include include-ci
                 define-syntax let-syntax letrec-syntax
                 syntax-rules syntax-error
                 guard

                 ;; R7RS control
                 dynamic-wind

                 ;; R7RS values
                 values
                 call-with-values
                 apply

                 ;; R7RS pairs
                 pair?
                 cons
                 car
                 cdr
                 set-car!
                 set-cdr!

                 ;; R7RS lists
                 null?
                 append

                 ;; R7RS numerics
                 *
                 +
                 -
                 /
                 <
                 <=
                 =
                 >
                 >=
                 abs
                 floor
                 ceiling
                 number?
                 complex?
                 real?
                 rational?
                 integer?
                 exact-integer?
                 exact?
                 inexact?
                 quotient
                 remainder
                 modulo

                 ;; R7RS chars
                 char->integer
                 integer->char
                 char?

                 ;; R7RS ports
                 eof-object?

                 ;; R7RS equality
                 eq?
                 eqv?

                 ;; R7RS strings
                 string?
                 string-length
                 string-ref
                 string->list

                 ;; Symbols
                 symbol?
                 symbol->string
                 string->symbol

                 ;; R7RS vectors
                 vector?
                 make-vector
                 vector
                 vector-length
                 vector-ref
                 vector-set!

                 procedure?))
  #:use-module ((scheme case-lambda)
                #:select (case-lambda))
  #:use-module ((scheme inexact)
                #:select (inexact sin cos tan asin acos atan sqrt))
  #:re-export
  ( ;; R7RS syntax
   _
   ... => else
   lambda case-lambda
   define let let* letrec letrec*
   or and
   begin
   if cond case when unless
   do
   set!
   quote quasiquote unquote unquote-splicing
   include include-ci
   define-syntax let-syntax letrec-syntax
   syntax-rules syntax-error
   ;; FIXME: These two need Hoot support.
   ;; guard

   ;; Most primitives can only appear in primcalls, so we expose them as
   ;; %foo instead of foo, relying on the prelude to wrap them in
   ;; lambdas to ensure they are always called with the right number of
   ;; arguments, even when used as a value.  The three exceptions are
   ;; `apply`, `abort-to-prompt`, and `values`.

   ;; Guile syntax extensions
   define-syntax-rule
   syntax-case syntax quasisyntax unsyntax unsyntax-splicing
   (syntax->datum . %syntax->datum)
   (datum->syntax . %datum->syntax)
   identifier? generate-temporaries free-identifier=? bound-identifier=?
   with-syntax identifier-syntax make-variable-transformer
   syntax-local-binding syntax-violation procedure-property
   target-runtime
   gensym
   lambda* case-lambda* define*

   ;; Guile VM primitives
   (make-record-type . guile:make-record-type)
   (record-type-parents . guile:record-type-parents)
   (vtable-index-printer . guile:vtable-index-printer)
   (struct-vtable? . guile:struct-vtable?)
   (make-fluid . guile:make-fluid)
   (fluid? . guile:fluid?)
   (make-parameter . guile:make-parameter)
   (parameter? . guile:parameter?)
   (parameter-fluid . guile:parameter-fluid)
   (parameter-converter . guile:parameter-converter)
   (%make-void-port . guile:make-void-port)
   (hashq . guile:hashq)
   (hashv . guile:hashv)
   (hash . guile:hash)
   (make-bytevector . guile:make-bytevector)
   (bytevector-copy! . guile:bytevector-copy!)
   (string->list . guile:string->list)
   (string-copy . guile:string-copy)
   (syntax? . guile:syntax?)
   (make-syntax . guile:make-syntax)
   (syntax-expression . guile:syntax-expression)
   (syntax-wrap . guile:syntax-wrap)
   (syntax-module . guile:syntax-module)
   (syntax-sourcev . guile:syntax-sourcev)
   (make-regexp . guile:make-regexp)
   (regexp? . guile:regexp?)
   (regexp-exec . guile:regexp-exec)
   (regexp-match? . guile:regexp-match?)
   (match:string . guile:match:string)
   (match:start . guile:match:start)
   (match:end . guile:match:end)
   (match:count . guile:match:count)
   (match:substring . guile:match:substring)
   (pk . guile:pk)

   ;; R7RS control
   (dynamic-wind . %dynamic-wind)

   ;; R7RS values
   (values . %values)
   (call-with-values . %call-with-values)
   apply

   ;; R7RS pairs
   (pair? . %pair?)
   (cons . %cons)
   (car . %car)
   (cdr . %cdr)
   (set-car! . %set-car!)
   (set-cdr! . %set-cdr!)

   ;; R7RS lists
   (null? . %null?)
   (append . %append)

   ;; R7RS bytevectors
   (bytevector-length . %bytevector-length)
   (bytevector-u8-ref . %bytevector-u8-ref)
   (bytevector-u8-set! . %bytevector-u8-set!)
   (bytevector? . %bytevector?)
   (string->utf8 . %string->utf8)
   (utf8->string . %utf8->string)
   (string-utf8-length . %string-utf8-length)

   ;; R7RS numerics
   (* . %*)
   (+ . %+)
   (- . %-)
   (/ . %/)
   (< . %<)
   (<= . %<=)
   (= . %=)
   (> . %>)
   (>= . %>=)
   (abs . %abs)
   (floor . %floor)
   (ceiling . %ceiling)
   (number? . %number?)
   (complex? . %complex?)
   (real? . %real?)
   (rational? . %rational?)
   (integer? . %integer?)
   (exact-integer? . %exact-integer?)
   (exact? . %exact?)
   (inexact? . %inexact?)
   ;; FIXME: we should actually be using the R7RS variants which are
   ;; slightly different than Guile's.
   (inexact . %inexact)
   (quotient . %quotient)
   (remainder . %remainder)
   (modulo . %modulo)
   (sin . %sin)
   (cos . %cos)
   (tan . %tan)
   (asin . %asin)
   (acos . %acos)
   (atan . %atan)
   (sqrt . %sqrt)

   ;; R7RS chars
   (char->integer . %char->integer)
   (integer->char . %integer->char)
   (char? . %char?)

   ;; R7RS ports
   (eof-object? . %eof-object?)

   ;; Parameters

   ;; R7RS equality
   (eq? . %eq?)
   (eqv? . %eqv?)

   ;; R7RS strings
   (string? . %string?)
   (string-length . %string-length)
   (string-ref . %string-ref)

   ;; Symbols
   (symbol? . %symbol?)
   (symbol->string . %symbol->string)
   (string->symbol . %string->symbol)

   ;; Keywords
   (symbol->keyword . %symbol->keyword)
   (keyword->symbol . %keyword->symbol)

   ;; R7RS vectors
   (vector? . %vector?)
   (make-vector . %make-vector)
   (vector . %vector)
   (vector-length . %vector-length)
   (vector-ref . %vector-ref)
   (vector-set! . %vector-set!)

   ;; Error handling
   (error . %error)
   (raise-exception . %raise-exception)

   (procedure? . %procedure?)

   ;; guile extensions
   (call-with-prompt . %call-with-prompt)
   (abort-to-prompt . %abort-to-prompt)
   (ash . %ash)
   (logand . %logand)
   (logior . %logior)
   (logxor . %logxor)
   (lognot . %lognot)
   (logtest . %logtest)
   (logbit? . %logbit?)
   (keyword? . %keyword?)
   (bitvector? . %bitvector?)
   (cons* . %cons*)
   (fluid-ref . %fluid-ref)
   (fluid-set! . %fluid-set!)
   (with-fluid* . %with-fluid*)
   (with-dynamic-state . %with-dynamic-state)
   (make-atomic-box . %make-atomic-box)
   (atomic-box-ref . %atomic-box-ref)
   (atomic-box-set! . %atomic-box-set!)
   (atomic-box-swap! . %atomic-box-swap!)
   (atomic-box-compare-and-swap! . %atomic-box-compare-and-swap!)
   (bytevector-s8-ref . %bytevector-s8-ref)
   (bytevector-s8-set! . %bytevector-s8-set!)
   (bytevector-u16-native-ref . %bytevector-u16-native-ref)
   (bytevector-u16-native-set! . %bytevector-u16-native-set!)
   (bytevector-s16-native-ref . %bytevector-s16-native-ref)
   (bytevector-s16-native-set! . %bytevector-s16-native-set!)
   (bytevector-u32-native-ref . %bytevector-u32-native-ref)
   (bytevector-u32-native-set! . %bytevector-u32-native-set!)
   (bytevector-s32-native-ref . %bytevector-s32-native-ref)
   (bytevector-s32-native-set! . %bytevector-s32-native-set!)
   (bytevector-u64-native-ref . %bytevector-u64-native-ref)
   (bytevector-u64-native-set! . %bytevector-u64-native-set!)
   (bytevector-s64-native-ref . %bytevector-s64-native-ref)
   (bytevector-s64-native-set! . %bytevector-s64-native-set!)
   (bytevector-ieee-single-native-ref . %bytevector-ieee-single-native-ref)
   (bytevector-ieee-single-native-set! . %bytevector-ieee-single-native-set!)
   (bytevector-ieee-double-native-ref . %bytevector-ieee-double-native-ref)
   (bytevector-ieee-double-native-set! . %bytevector-ieee-double-native-set!)
   (the-eof-object . %the-eof-object)
   (make-variable . %make-box)
   (variable-ref . %box-ref)
   (variable-set! . %box-set!)
   (make-struct/simple . %make-struct)
   (struct? . %struct?)
   (struct-vtable . %struct-vtable)
   (struct-ref . %struct-ref)
   (struct-set! . %struct-set!))
  #:export (%inline-wasm
            %wasm-import
            include-from-path
            (syntax-module-bindings . guile:syntax-module-bindings))
  ;; Mark as non-declarative, as we should not have inlinable exports.
  #:declarative? #f)

(define (%inline-wasm code . args)
  "Emit inline WebAssembly code.  @var{code} is a WebAssembly module
expressed in WebAssembly's s-expression syntax.  The backend expects the
parsed module to contain a single function.  The arguments
@var{arg}... should correspond to the parameters of the function.  The
number of result values is also determined from the function signature."
  (error "target-only primitive"))

(define (%wasm-import code)
  "Emit WebAssembly import.  @var{code} is a WebAssembly module
expressed in WebAssembly's s-expression syntax.  The backend expects the
parsed module to contain a single import."
  (error "target-only primitive"))

(define (syntax-module-bindings id)
  (define local '())
  (define imported '())
  (define mod (resolve-module (cdr (syntax-module id))))
  (define (exclude-imported-binding? sym)
    ;; This is a hack.
    (case sym
      ((bound-identifier=? free-identifier=? datum->syntax
        syntax->datum generate-temporaries identifier?) #t)
      (else #f)))
  (define (exclude-binding? var)
    (if (variable-bound? var)
        (macro? (variable-ref var))
        #t))
  (define (for-each f l)
    (unless (null? l)
      (f (car l))
      (for-each f (cdr l))))
  (define (map f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l)))))
  (module-for-each (lambda (sym var)
                     (unless (exclude-binding? var)
                       (set! local (cons sym local))))
                   mod)
  (define (visit-imported-bindings m f)
    (for-each (lambda (i)
                (visit-imported-bindings i f)
                (module-for-each f i))
              (module-uses m)))
  (visit-imported-bindings mod
                           (lambda (sym var)
                             (unless (or (exclude-imported-binding? sym)
                                         (exclude-binding? var))
                               (set! imported (cons sym imported)))))
  (define (bless sym) (datum->syntax id sym))
  (values (map bless local) (map bless imported)))

(define-syntax include-from-path
  (lambda (x)
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax->datum #'filename)))
         (with-syntax ((fn (datum->syntax
                            #'filename
                            (canonicalize-path
                             (or (search-path (append (hoot-system-load-path)
                                                      (hoot-load-path))
                                              fn
                                              (hoot-load-extensions))
                                 (syntax-violation 'include-from-path
                                                   "file not found in path"
                                                   x #'filename))))))
           #'(include fn)))))))

;(add-interesting-primitive! '%inline-asm)
