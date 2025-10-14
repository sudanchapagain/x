;;; Syntax expander
;;; Copyright (C) 2024, 2025 Igalia, S.L.
;;; Copyright (C) 1997-1998,2000-2003,2005-2006,2008-2013,2015-2022,2024
;;;   Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Originally extracted from Chez Scheme Version 5.9f
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman
;;;
;;; Copyright (c) 1992-1997 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.
;;;
;;; This code is based on "Syntax Abstraction in Scheme"
;;; by R. Kent Dybvig, Robert Hieb, and Carl Bruggeman.
;;; Lisp and Symbolic Computation 5:4, 295-326, 1992.
;;; <http://www.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf>

;;; Commentary:

;;; This file defines Hoot's syntax expander and a set of associated
;;; syntactic forms and procedures.  For more documentation, see The
;;; Scheme Programming Language, Fourth Edition (R. Kent Dybvig, MIT
;;; Press, 2009), or the R6RS.

;;; Code:

(library (hoot expander)
  (export (rename macroexpand expand-syntax)
          initialize-core-syntax!)
  (import (hoot apply)
          (hoot assoc)
          (hoot core-syntax)
          (only (hoot core-syntax-helpers) %initialize-syntax-helpers!)
          (hoot cross-compilation)
          (hoot debug)
          (hoot eq)
          (hoot equal)
          (hoot errors)
          (hoot primitive-eval)
          (hoot gensym)
          (hoot hashtables)
          (hoot keywords)
          (hoot lists)
          (hoot modules)
          (hoot not)
          (only (hoot numbers) 1+ 1- = zero? most-positive-fixnum)
          (hoot pairs)
          (hoot parameters)
          (hoot procedures)
          (hoot records)
          (hoot strings)
          (hoot symbols)
          (rename (hoot syntax-objects)
                  (syntax-module %syntax-module))
          (hoot syntax-transformers)
          (hoot tree-il)
          (hoot values)
          (hoot vectors)
          (only (hoot write) number->string)
          (only (guile)
                and-map or-map
                string-join string-concatenate
                object->string)
          (ice-9 match))

  (define expansion-environment (make-parameter #f))

  (define (syntax-module stx)
    (let ((mod (%syntax-module stx)))
      (match mod
        (#f #f)
        (('private . _)
         ;; Unlike Guile's psyntax, where R6RS modules are implemented by
         ;; macros and @@, and which has to deal with module system boot, in
         ;; Hoot we just have one kind of top-level reference: with respect
         ;; to a specific module, from the inside of that module.  This
         ;; corresponds with "private" from upstream psyntax, which is the
         ;; same as "hygiene" but without recapturing current-module within a
         ;; top-level sequence.  We don't have "bare", "public", or
         ;; "primitive".
         mod)
        (('hygiene . tail)
         ;; However, for references that were residualized by Guile's
         ;; expander, we may have "hygiene" references embedded in
         ;; syntax objects.  These are of two kinds: namespaced, for a
         ;; library-group expansion, or bare, if for some reason a
         ;; module was expanded on its own.  Probably the latter
         ;; shouldn't happen.  Anyway, strip off the namespace, if
         ;; present, so that those free variables resolve within the
         ;; module tree that was passed in as a value.
         (match tail
           (('% namespace . name) (cons 'private name))
           (name (cons 'private name)))))))

  (define (resolve-module* mod)
    (match mod
      (('private . modname)
       (resolve-module (expansion-environment) modname))))

  (define (resolve-variable mod var kt kf)
    (match (resolve-module* mod)
      (#f (kf))
      (mod (module-variable mod var #:private? #t
                            #:found kt #:not-found kf))))

  (define (top-level-eval x mod)
    (primitive-eval x (or (resolve-module* mod)
                          (syntax-violation #f "no module found" mod))))
  (define (local-eval x mod)
    (top-level-eval x mod))
  
  (define (install-syntax-definition! module type sym val)
    (module-define! module sym
                    (make-syntax-transformer type val)
                    #:allow-redefinition? #t))

  (define (maybe-name-value name val)
    (if (lambda? val)
        (let ((meta (lambda-meta val)))
          (if (assq 'name meta)
              val
              (make-lambda (tree-il-src val)
                           (acons 'name name meta)
                           (lambda-body val))))
        val))

  ;; output constructors
  (define build-void make-void)
  (define build-call make-call)
  (define build-conditional make-conditional)
  (define build-lexical-reference make-lexical-ref)
  (define (build-lexical-assignment sourcev name var exp)
    (make-lexical-set sourcev name var (maybe-name-value name exp)))

  (define (analyze-variable mod var modref-cont)
    (match mod
      (('private . mod)
       (modref-cont mod var #f))))

  (define (build-global-reference src var mod)
    (analyze-variable
     mod var
     (lambda (mod var public?) 
       (make-module-ref src mod var public?))))

  (define (build-global-assignment src var exp mod)
    (let ((exp (maybe-name-value var exp)))
      (analyze-variable
       mod var
       (lambda (mod var public?) 
         (make-module-set src mod var public? exp)))))

  (define (build-global-definition src mod var exp)
    (make-toplevel-define src (and mod (cdr mod)) var
                          (maybe-name-value var exp)))

  (define (build-simple-lambda src req rest vars meta exp)
    (make-lambda src meta
                 (make-lambda-case
                  ;; src req opt rest kw inits vars body else
                  src req #f rest #f '() vars exp #f)))

  (define build-case-lambda make-lambda)
  (define build-lambda-case make-lambda-case)
  (define build-primcall make-primcall)
  (define build-primref make-primitive-ref)
  (define build-data make-const)

  (define (build-sequence src exps)
    (match exps
      ((tail) tail)
      ((head . tail)
       (make-seq src head (build-sequence #f tail)))))

  (define (build-let src ids vars val-exps body-exp)
    (match (map maybe-name-value ids val-exps)
      (() body-exp)
      (val-exps (make-let src ids vars val-exps body-exp))))

  (define (build-named-let src ids vars val-exps body-exp)
    (match vars
      ((f . vars)
       (match ids
         ((f-name . ids)
          (let ((proc (build-simple-lambda src ids #f vars '() body-exp)))
            (make-letrec
             src #f
             (list f-name) (list f) (list (maybe-name-value f-name proc))
             (build-call src (build-lexical-reference src f-name f)
                         (map maybe-name-value ids val-exps)))))))))

  (define (build-letrec src in-order? ids vars val-exps body-exp)
    (match (map maybe-name-value ids val-exps)
      (() body-exp)
      (val-exps (make-letrec src in-order? ids vars val-exps body-exp))))

  (define (gen-lexical id)
    ;; Generate a unique symbol for a lexical variable.  These need to
    ;; be symbols as they are embedded in Tree-IL.  In future these
    ;; should be more globally unique, as in Guile.
    (gensym (symbol->string id)))

  (define no-source #f)

  (define (source-annotation x)
    (and (syntax? x) (syntax-sourcev x)))

  (define-syntax-rule (arg-check pred? e who)
    (let ((x e))
      (unless (pred? x) (syntax-violation who "invalid argument" x))))

  ;; compile-time environments

  ;; wrap and environment comprise two level mapping.
  ;;   wrap : id --> label
  ;;   env : label --> <element>

  ;; environments are represented in two parts: a lexical part and a
  ;; global part.  The lexical part is a simple list of associations
  ;; from labels to bindings.  The global part is implemented by (hoot
  ;; module)'s registry of module environments and associates symbols
  ;; with bindings.

  ;; global (assumed global variable) and displaced-lexical (see below)
  ;; do not show up in any environment; instead, they are fabricated by
  ;; resolve-identifier when it finds no other bindings.

  ;; <environment>              ::= ((<label> . <binding>)*)

  ;; identifier bindings include a type and a value

  ;; <binding> ::= (macro . <procedure>)           macros
  ;;               (syntax-parameter . <procedure>) syntax parameters
  ;;               (core . <procedure>)            core forms
  ;;               (begin)                         begin
  ;;               (define)                        define
  ;;               (define-syntax)                 define-syntax
  ;;               (define-syntax-parameter)       define-syntax-parameter
  ;;               (local-syntax . rec?)           let-syntax/letrec-syntax
  ;;               (eval-when)                     eval-when
  ;;               (syntax . (<var> . <level>))    pattern variables
  ;;               (global)                        assumed global variable
  ;;               (lexical . <var>)               lexical variables
  ;;               (ellipsis . <identifier>)       custom ellipsis
  ;;               (displaced-lexical)             displaced lexicals
  ;; <level>   ::= <non-negative integer>
  ;; <var>     ::= symbol returned by gen-lexical

  ;; a macro is a user-defined syntactic-form.  a core is a
  ;; system-defined syntactic form.  begin, define, define-syntax,
  ;; define-syntax-parameter, and eval-when are treated specially
  ;; since they are sensitive to whether the form is at top-level and
  ;; (except for eval-when) can denote valid internal definitions.

  ;; a pattern variable is a variable introduced by syntax-case and can
  ;; be referenced only within a syntax form.

  ;; any identifier for which no top-level syntax definition or local
  ;; binding of any kind has been seen is assumed to be a global
  ;; variable.

  ;; a lexical variable is a lambda- or letrec-bound variable.

  ;; an ellipsis binding is introduced by the 'with-ellipsis' special
  ;; form.

  ;; a displaced-lexical identifier is a lexical identifier removed from
  ;; its scope by the return of a syntax object containing the identifier.
  ;; a displaced lexical can also appear when a letrec-syntax-bound
  ;; keyword is referenced on the rhs of one of the letrec-syntax clauses.
  ;; a displaced lexical should never occur with properly written macros.

  (define-syntax make-binding
    (syntax-rules (quote)
      ((_ type value) (cons type value))
      ((_ 'type) '(type))
      ((_ type) (cons type '()))))
  (define (binding-type x) (car x))
  (define (binding-value x) (cdr x))
  (define null-env '())

  (define (extend-env labels bindings r)
    (match labels
      (() r)
      ((label . labels)
       (match bindings
         ((binding . bindings)
          (extend-env labels bindings (acons label binding r)))))))

  (define (extend-var-env labels vars r)
    ;; variant of extend-env that forms "lexical" binding
    (match labels
      (() r)
      ((label . labels)
       (match vars
         ((var . vars)
          (extend-var-env labels vars
                          (acons label (make-binding 'lexical var) r)))))))

  ;; we use a "macros only" environment in expansion of local macro
  ;; definitions so that their definitions can use local macros without
  ;; attempting to use other lexical identifiers.
  (define (macros-only-env r)
    (match r
      (() '())
      ((a . r)
       (match a
         ((k . ((or 'macro 'syntax-parameter 'ellipsis) . _))
          (cons a (macros-only-env r)))
         (_
          (macros-only-env r))))))

  ;; Conceptually, identifiers are always syntax objects.  Internally,
  ;; however, the wrap is sometimes maintained separately (a source of
  ;; efficiency and confusion), so that symbols are also considered
  ;; identifiers by id?.  Externally, they are always wrapped.

  (define (nonsymbol-id? x)
    (and (syntax? x)
         (symbol? (syntax-expression x))))

  (define (id? x)
    (cond
     ((symbol? x) #t)
     ((syntax? x) (symbol? (syntax-expression x)))
     (else #f)))

  (define (id-sym-name x)
    (if (syntax? x)
        (syntax-expression x)
        x))

  (define (id-sym-name&marks x w)
    (if (syntax? x)
        (values
         (syntax-expression x)
         (join-marks (wrap-marks w) (wrap-marks (syntax-wrap x))))
        (values x (wrap-marks w))))

  ;; syntax object wraps

  ;;      <wrap> ::= ((<mark> ...) . (<subst> ...))
  ;;     <subst> ::= shift | <subs>
  ;;      <subs> ::= #(ribcage #(<sym> ...) #(<mark> ...) #(<label> ...))
  ;;                 | #(ribcage (<sym> ...) (<mark> ...) (<label> ...))

  (define (make-wrap marks subst) (cons marks subst))
  (define (wrap-marks wrap) (car wrap))
  (define (wrap-subst wrap) (cdr wrap))

  (define (gen-unique)
    ;; Generate a unique value, used as a mark to identify a scope, or
    ;; as a label to associate an identifier with a lexical.  As with
    ;; gen-lexical, we should try to be more globally unique, to support
    ;; separate compilation.
    (vector (gensym "id")))

  ;; labels must be comparable with "eq?", have read-write invariance,
  ;; and distinct from symbols.  Pair labels are used for top-level
  ;; definition placeholders.  These labels are used for proper
  ;; lexicals.
  (define (gen-label)
    (gen-unique))

  (define (gen-labels ls)
    (match ls
      (() '())
      ((_ . ls) (cons (gen-label) (gen-labels ls)))))

  (define (make-ribcage symnames marks labels)
    (vector 'ribcage symnames marks labels))
  (define (ribcage-symnames ribcage) (vector-ref ribcage 1))
  (define (ribcage-marks ribcage) (vector-ref ribcage 2))
  (define (ribcage-labels ribcage) (vector-ref ribcage 3))
  (define (set-ribcage-symnames! ribcage x) (vector-set! ribcage 1 x))
  (define (set-ribcage-marks! ribcage x) (vector-set! ribcage 2 x))
  (define (set-ribcage-labels! ribcage x) (vector-set! ribcage 3 x))

  (define empty-wrap '(()))
  (define top-wrap '((top)))

  ;; Marks must be comparable with "eq?" and distinct from pairs and
  ;; the symbol top.  We do not use integers so that marks will remain
  ;; unique even across file compiles.

  (define the-anti-mark #f)

  (define (anti-mark w)
    (make-wrap (cons the-anti-mark (wrap-marks w))
               (cons 'shift (wrap-subst w))))

  (define (new-mark)
    (gen-unique))

  ;; make-empty-ribcage and extend-ribcage maintain list-based ribcages for
  ;; internal definitions, in which the ribcages are built incrementally
  (define (make-empty-ribcage)
    (make-ribcage '() '() '()))

  (define (extend-ribcage! ribcage id label)
    ;; must receive ids with complete wraps
    (set-ribcage-symnames! ribcage
                           (cons (syntax-expression id)
                                 (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage
                        (cons (wrap-marks (syntax-wrap id))
                              (ribcage-marks ribcage)))
    (set-ribcage-labels! ribcage
                         (cons label (ribcage-labels ribcage))))

  ;; make-binding-wrap creates vector-based ribcages
  (define (make-binding-wrap ids labels w)
    (match ids
      (() w)
      ((_ . _)
       (make-wrap
        (wrap-marks w)
        (cons
         (let* ((labelvec (list->vector labels))
                (n (vector-length labelvec))
                (symnamevec (make-vector n))
                (marksvec (make-vector n)))
           (let f ((ids ids) (i 0))
             (match ids
               (()
                (make-ribcage symnamevec marksvec labelvec))
               ((id . ids)
                (call-with-values
                    (lambda () (id-sym-name&marks id w))
                  (lambda (symname marks)
                    (vector-set! symnamevec i symname)
                    (vector-set! marksvec i marks)
                    (f ids (1+ i))))))))
         (wrap-subst w))))))

  (define (smart-append m1 m2)
    (if (null? m2)
        m1
        (append m1 m2)))

  (define (join-wraps w1 w2)
    (let ((m1 (wrap-marks w1)) (s1 (wrap-subst w1)))
      (if (null? m1)
          (if (null? s1)
              w2
              (make-wrap
               (wrap-marks w2)
               (smart-append s1 (wrap-subst w2))))
          (make-wrap
           (smart-append m1 (wrap-marks w2))
           (smart-append s1 (wrap-subst w2))))))

  (define (join-marks m1 m2)
    (smart-append m1 m2))

  (define (same-marks? x y)
    (or (eq? x y)
        (and (not (null? x))
             (not (null? y))
             (eq? (car x) (car y))
             (same-marks? (cdr x) (cdr y)))))

  (define (id-var-name id w mod)
    ;; Syntax objects use wraps to associate names with marked
    ;; identifiers.  This function returns the name corresponding to
    ;; the given identifier and wrap, or the original identifier if no
    ;; corresponding name was found.
    ;;
    ;; The name may be a string created by gen-label, indicating a
    ;; lexical binding, or another syntax object, indicating a
    ;; reference to a top-level definition created during a previous
    ;; macroexpansion.
    ;;
    ;; For lexical variables, finding a label simply amounts to
    ;; looking for an entry with the same symbolic name and the same
    ;; marks.  Finding a toplevel definition is the same, except we
    ;; also have to compare modules, hence the `mod' parameter.
    ;; Instead of adding a separate entry in the ribcage for modules,
    ;; which wouldn't be used for lexicals, we arrange for the entry
    ;; for the name entry to be a pair with the module in its car, and
    ;; the name itself in the cdr.  So if the name that we find is a
    ;; pair, we have to check modules.
    ;;
    ;; The identifer may be passed in wrapped or unwrapped.  In any
    ;; case, this routine returns either a symbol, a syntax object, or
    ;; a string label.
    ;;
    (define (search sym subst marks)
      (match subst
        (() #f)
        (('shift . subst)
         (match marks
           ((_ . marks)
            (search sym subst marks))))
        ((#('ribcage rsymnames rmarks rlabels) . subst)
         (define (search-list-rib)
           (let lp ((rsymnames rsymnames)
                    (rmarks rmarks)
                    (rlabels rlabels))
             (match rsymnames
               (() (search sym subst marks))
               ((rsym . rsymnames)
                (match rmarks
                  ((rmarks1 . rmarks)
                   (match rlabels
                     ((label . rlabels)
                      (if (and (eq? sym rsym) (same-marks? marks rmarks1))
                          (match label
                            ((mod* . label)
                             (if (equal? mod* mod)
                                 label
                                 (lp rsymnames rmarks rlabels)))
                            (_ label))
                          (lp rsymnames rmarks rlabels))))))))))
         (define (search-vector-rib)
           (let ((n (vector-length rsymnames)))
             (let lp ((i 0))
               (cond
                ((= i n) (search sym subst marks))
                ((and (eq? (vector-ref rsymnames i) sym)
                      (same-marks? marks (vector-ref rmarks i)))
                 (match (vector-ref rlabels i)
                   ((mod* . label)
                    (if (equal? mod* mod)
                        label
                        (lp (1+ i))))
                   (label
                    label)))
                (else (lp (1+ i)))))))
         (if (vector? rsymnames)
             (search-vector-rib)
             (search-list-rib)))))
    (cond
     ((symbol? id)
      (or (search id (wrap-subst w) (wrap-marks w)) id))
     ((syntax? id)
      (let ((id (syntax-expression id))
            (w1 (syntax-wrap id))
            (mod (or (syntax-module id) mod)))
        (let ((marks (join-marks (wrap-marks w) (wrap-marks w1))))
          (or (search id (wrap-subst w) marks)
              (search id (wrap-subst w1) marks)
              id))))
     (else (syntax-violation 'id-var-name "invalid id" id))))

  ;; A helper procedure for syntax-locally-bound-identifiers, which
  ;; itself is a helper for transformer procedures.
  ;; `locally-bound-identifiers' returns a list of all bindings
  ;; visible to a syntax object with the given wrap.  They are in
  ;; order from outer to inner.
  ;;
  ;; The purpose of this procedure is to give a transformer procedure
  ;; references on bound identifiers, that the transformer can then
  ;; introduce some of them in its output.  As such, the identifiers
  ;; are anti-marked, so that rebuild-macro-output doesn't apply new
  ;; marks to them.
  ;;
  (define (locally-bound-identifiers w mod)
    (define (scan subst results)
      (match subst
        (() results)
        (('shift . subst) (scan subst results))
        ((#('ribcage symnames marks labels) . subst*)
         (define (scan-list-rib)
           (let lp ((symnames symnames) (marks marks) (results results))
             (match symnames
               (() (scan subst* results))
               ((sym . symnames)
                (match marks
                  ((m . marks)
                   (lp symnames marks
                       (cons (wrap sym (anti-mark (make-wrap m subst)) mod)
                             results))))))))
         (define (scan-vector-rib)
           (let ((n (vector-length symnames)))
             (let lp ((i 0) (results results))
               (if (= i n)
                   (scan subst* results)
                   (lp (1+ i)
                       (let ((sym (vector-ref symnames i))
                             (m (vector-ref marks i)))
                         (cons (wrap sym (anti-mark (make-wrap m subst)) mod)
                               results)))))))
         (if (vector? symnames)
             (scan-vector-rib)
             (scan-list-rib)))))
    (scan (wrap-subst w) '()))

  ;; Returns three values: binding type, binding value, and the module
  ;; (for resolving toplevel vars).
  (define (resolve-identifier id w r mod resolve-syntax-parameters?)
    (define (resolve-global name mod)
      (resolve-variable
       mod name
       (lambda (var source-module name)
         ;; The expander needs to know when a top-level definition from
         ;; outside the compilation unit is a macro.
         ;;
         ;; Additionally if a macro is actually a syntax-parameter, we
         ;; might need to resolve its current binding.  If the syntax
         ;; parameter is locally bound (via syntax-parameterize), then
         ;; its variable will be present in `r', the expand-time
         ;; environment.  It's a kind of double lookup: first we see
         ;; that a name is bound to a syntax parameter, then we look
         ;; for the current binding of the syntax parameter.
         ;;
         ;; We use the variable (box) holding the syntax parameter
         ;; definition as the key for the second lookup.  We use the
         ;; variable for two reasons:
         ;;
         ;;   1. If the syntax parameter is redefined in parallel
         ;;   (perhaps via a parallel module compilation), the
         ;;   redefinition keeps the same variable.  We don't want to
         ;;   use a "key" that could change during a redefinition.  See
         ;;   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27476.
         ;;
         ;;   2. Using the variable instead of its (symname, modname)
         ;;   pair allows for syntax parameters to be renamed or
         ;;   aliased while preserving the syntax parameter's identity.
         ;;
         (let ((val (var))
               (mod (cons 'private (module-name source-module))))
           (if (syntax-transformer? val)
               (let ((type (syntax-transformer-type val))
                     (trans (syntax-transformer-value val)))
                 (if (eq? type 'syntax-parameter)
                     (if resolve-syntax-parameters?
                         (let ((lexical (assq-ref r var)))
                           ;; A resolved syntax parameter is
                           ;; indistinguishable from a macro.
                           (values 'macro
                                   (if lexical
                                       (binding-value lexical)
                                       trans)
                                   mod))
                         ;; Return var as value for use in second lookup.
                         (values type var mod))
                     (values type trans mod)))
               ;; Variable bound, but not to a syntax transformer;
               ;; return resolved name and module.
               (values 'global name mod))))
       (lambda ()
         ;; Variable unbound; return original name and module.
         (values 'global name mod))))
    (define (resolve-lexical label mod)
      (let ((b (assq-ref r label)))
        (if b
            (let ((type (binding-type b))
                  (value (binding-value b)))
              (if (eq? type 'syntax-parameter)
                  (if resolve-syntax-parameters?
                      (values 'macro value mod)
                      ;; If the syntax parameter was defined within
                      ;; this compilation unit, use its label as its
                      ;; lookup key.
                      (values type label mod))
                  (values type value mod)))
            (values 'displaced-lexical #f #f))))
    (let ((n (id-var-name id w mod)))
      (cond
       ((syntax? n)
        (cond
         ((not (eq? n id))
          ;; This identifier aliased another; recurse to allow
          ;; syntax-parameterize to override macro-introduced syntax
          ;; parameters.
          (resolve-identifier n w r mod resolve-syntax-parameters?))
         (else
          ;; Resolved to a free variable that was introduced by this
          ;; macro; continue to resolve this global by name.
          (resolve-identifier (syntax-expression n)
                              (syntax-wrap n)
                              r
                              (or (syntax-module n) mod)
                              resolve-syntax-parameters?))))
       ((symbol? n)
        (resolve-global n (or (and (syntax? id)
                                   (syntax-module id))
                              mod)))
       (else
        (resolve-lexical n (or (and (syntax? id)
                                    (syntax-module id))
                               mod))))))

  (define transformer-environment
    (make-parameter
     (lambda (k)
       (error "called outside the dynamic extent of a syntax transformer"))))

  (define (with-transformer-environment k)
    ((transformer-environment) k))

  ;; free-id=? must be passed fully wrapped ids since (free-id=? x y)
  ;; may be true even if (free-id=? (wrap x w) (wrap y w)) is not.

  (define (free-id=? i j)
    (let* ((mi (and (syntax? i) (syntax-module i)))
           (mj (and (syntax? j) (syntax-module j)))
           (ni (id-var-name i empty-wrap mi))
           (nj (id-var-name j empty-wrap mj)))
      (define (id-module-binding id mod)
        (resolve-variable mod (id-sym-name id)
                          (lambda (var mod name) var)
                          (lambda () #f)))
      (cond
       ((syntax? ni) (free-id=? ni j))
       ((syntax? nj) (free-id=? i nj))
       ((symbol? ni)
        ;; `i' is not lexically bound.  Assert that `j' is free,
        ;; and if so, compare their bindings, that they are either
        ;; bound to the same variable, or both unbound and have
        ;; the same name.
        (and (eq? nj (id-sym-name j))
             (let ((bi (id-module-binding i mi))
                   (bj (id-module-binding j mj)))
               (and (eq? bi bj)
                    (or bi (eq? ni nj))))))
       (else
        ;; Otherwise `i' is bound, so check that `j' is bound, and
        ;; bound to the same thing.
        (equal? ni nj)))))

  ;; bound-id=? may be passed unwrapped (or partially wrapped) ids as
  ;; long as the missing portion of the wrap is common to both of the ids
  ;; since (bound-id=? x y) iff (bound-id=? (wrap x w) (wrap y w))

  (define (bound-id=? i j)
    (if (and (syntax? i) (syntax? j))
        (and (eq? (syntax-expression i)
                  (syntax-expression j))
             (same-marks? (wrap-marks (syntax-wrap i))
                          (wrap-marks (syntax-wrap j))))
        (eq? i j)))

  ;; "valid-bound-ids?" returns #t if it receives a list of distinct ids.
  ;; valid-bound-ids? may be passed unwrapped (or partially wrapped) ids
  ;; as long as the missing portion of the wrap is common to all of the
  ;; ids.

  (define (valid-bound-ids? ids)
    (and (let all-ids? ((ids ids))
           (match ids
             (() #t)
             ((id . ids)
              (and (id? id) (all-ids? ids)))))
         (distinct-bound-ids? ids)))

  ;; distinct-bound-ids? expects a list of ids and returns #t if there are
  ;; no duplicates.  It is quadratic on the length of the id list; long
  ;; lists could be sorted to make it more efficient.  distinct-bound-ids?
  ;; may be passed unwrapped (or partially wrapped) ids as long as the
  ;; missing portion of the wrap is common to all of the ids.

  (define (distinct-bound-ids? ids)
    (let distinct? ((ids ids))
      (match ids
        (() #t)
        ((id . ids)
         (and (not (bound-id-member? id ids))
              (distinct? ids))))))

  (define (bound-id-member? x ids)
    (match ids
      (() #f)
      ((id . ids)
       (or (bound-id=? x id)
           (bound-id-member? x ids)))))

  ;; wrapping expressions and identifiers

  (define (wrap x w defmod)
    (source-wrap x w #f defmod))

  (define (wrap-syntax x w defmod)
    (make-syntax (syntax-expression x)
                 w
                 (or (syntax-module x) defmod)
                 (syntax-sourcev x)))
  (define (source-wrap x w s defmod)
    (cond
     ((and (null? (wrap-marks w))
           (null? (wrap-subst w))
           (not defmod)
           (not s))
      x)
     ((syntax? x) (wrap-syntax x (join-wraps w (syntax-wrap x)) defmod))
     ((null? x) x)
     (else (make-syntax x w defmod s))))

  ;; expanding

  (define (expand-sequence body r w s mod)
    (build-sequence s
                    (let lp ((body body))
                      (match body
                        (() '())
                        ((head . tail)
                         (let ((expr (expand head r w mod)))
                           (cons expr (lp tail))))))))

  ;; At top-level, we allow mixed definitions and expressions.  Like
  ;; expand-body we expand in two passes.
  ;;
  ;; First, from left to right, we expand just enough to know what
  ;; expressions are definitions, syntax definitions, and splicing
  ;; statements (`begin').  If we anything needs evaluating at
  ;; expansion-time, it is expanded directly.
  ;;
  ;; Otherwise we collect expressions to expand, in thunks, and then
  ;; expand them all at the end.  This allows all syntax expanders
  ;; visible in a toplevel sequence to be visible during the
  ;; expansions of all normal definitions and expressions in the
  ;; sequence.
  ;;
  (define (expand-top-sequence body r w s m esew mod)
    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
      (define (symbol-append . syms)
        (string->symbol (string-concatenate (map symbol->string syms))))
      (define (record-definition! id var)
        ;; Ribcages map symbol+marks to names, mostly for
        ;; resolving lexicals.  Here to add a mapping for toplevel
        ;; definitions we also need to match the module.  So, we
        ;; put it in the name instead, and make id-var-name handle
        ;; the special case of names that are pairs.  See the
        ;; comments in id-var-name for more.
        (extend-ribcage! ribcage id
                         (cons (or (syntax-module id) mod)
                               (wrap var top-wrap mod))))
      (define (macro-introduced-identifier? id)
        (not (equal? (wrap-marks (syntax-wrap id)) '(top))))
      (define (ensure-fresh-name var)
        ;; If a macro introduces a top-level identifier, we attempt
        ;; to give it a fresh name by appending the hash of the
        ;; expression in which it appears.  However, this can fail
        ;; for hash collisions, which is more common that one might
        ;; think: Guile's hash function stops descending into cdr's
        ;; at some point.  So, within an expansion unit, fall back
        ;; to appending a uniquifying integer.
        (define (ribcage-has-var? var)
          (let lp ((labels (ribcage-labels ribcage)))
            (match labels
              (() #f)
              (((_ . wrapped) . labels)
               (or (eq? (syntax-expression wrapped) var)
                   (lp labels))))))
        (let lp ((unique var) (n 1))
          (if (ribcage-has-var? unique)
              (let ((tail (string->symbol (number->string n))))
                (lp (symbol-append var '- tail) (1+ n)))
              unique)))
      (define (fresh-derived-name id orig-form)
        (ensure-fresh-name
         (symbol-append
          (syntax-expression id)
          '-
          (string->symbol
           ;; FIXME: This encodes hash values into the ABI of
           ;; compiled modules; a problem?
           (number->string
            (hash (syntax->datum orig-form) most-positive-fixnum)
            16)))))
      (define (parse body r w s m esew mod)
        (let lp ((body body))
          (match body
            (() '())
            ((head . tail)
             (let ((thunks (parse1 head r w s m esew mod)))
               (append thunks (lp tail)))))))
      (define (parse1 x r w s m esew mod)
        (call-with-values
            (lambda ()
              (syntax-type x r w (source-annotation x) ribcage mod #f))
          (lambda (type value form e w s mod)
            (case type
              ((define-form)
               (let* ((id (wrap value w mod))
                      (var (if (macro-introduced-identifier? id)
                               (fresh-derived-name id x)
                               (syntax-expression id))))
                 (record-definition! id var)
                 (list
                  (if (eq? m 'c&e)
                      (let ((x (build-global-definition s mod var (expand e r w mod))))
                        (top-level-eval x mod)
                        (lambda () x))
                      (call-with-values
                          (lambda () (resolve-identifier id empty-wrap r mod #t))
                        (lambda (type* value* mod*)
                          ;; If the identifier to be bound is currently bound to a
                          ;; macro, then immediately discard that binding.
                          (when (eq? type* 'macro)
                            (top-level-eval (build-global-definition
                                             s mod var (build-void s))
                                            mod))
                          (lambda ()
                            (build-global-definition s mod var (expand e r w mod)))))))))
              ((define-syntax-form define-syntax-parameter-form)
               (let* ((id (wrap value w mod))
                      (var (if (macro-introduced-identifier? id)
                               (fresh-derived-name id x)
                               (syntax-expression id))))
                 (record-definition! id var)
                 (case m
                   ((c)
                    (cond
                     ((memq 'compile esew)
                      (let ((e (expand-install-global mod var type (expand e r w mod))))
                        (top-level-eval e mod)
                        (if (memq 'load esew)
                            (list (lambda () e))
                            '())))
                     ((memq 'load esew)
                      (list (lambda ()
                              (expand-install-global mod var type (expand e r w mod)))))
                     (else '())))
                   ((c&e)
                    (let ((e (expand-install-global mod var type (expand e r w mod))))
                      (top-level-eval e mod)
                      (list (lambda () e))))
                   (else
                    (when (memq 'eval esew)
                      (top-level-eval
                       (expand-install-global mod var type (expand e r w mod))
                       mod))
                    '()))))
              ((begin-form)
               (syntax-case e ()
                 ((_ e1 ...)
                  (parse #'(e1 ...) r w s m esew mod))))
              ((local-syntax-form)
               (expand-local-syntax value e r w s mod
                                    (lambda (forms r w s mod)
                                      (parse forms r w s m esew mod))))
              ((eval-when-form)
               (syntax-case e ()
                 ((_ (x ...) e1 e2 ...)
                  (let ((when-list (parse-when-list e #'(x ...)))
                        (body #'(e1 e2 ...)))
                    (define (recurse m esew)
                      (parse body r w s m esew mod))
                    (cond
                     ((eq? m 'e)
                      (if (memq 'eval when-list)
                          (recurse (if (memq 'expand when-list) 'c&e 'e)
                                   '(eval))
                          (begin
                            (when (memq 'expand when-list)
                              (top-level-eval
                               (expand-top-sequence body r w s 'e '(eval) mod)
                               mod))
                            '())))
                     ((memq 'load when-list)
                      (if (or (memq 'compile when-list)
                              (memq 'expand when-list)
                              (and (eq? m 'c&e) (memq 'eval when-list)))
                          (recurse 'c&e '(compile load))
                          (if (memq m '(c c&e))
                              (recurse 'c '(load))
                              '())))
                     ((or (memq 'compile when-list)
                          (memq 'expand when-list)
                          (and (eq? m 'c&e) (memq 'eval when-list)))
                      (top-level-eval
                       (expand-top-sequence body r w s 'e '(eval) mod)
                       mod)
                      '())
                     (else
                      '()))))))
              (else
               (list
                (if (eq? m 'c&e)
                    (let ((x (expand-expr type value form e r w s mod)))
                      (top-level-eval x mod)
                      (lambda () x))
                    (lambda ()
                      (expand-expr type value form e r w s mod)))))))))
      (match (let lp ((thunks (parse body r w s m esew mod)))
               (match thunks
                 (() '())
                 ((thunk . thunks) (cons (thunk) (lp thunks)))))
        (() (build-void s))
        (exps (build-sequence s exps)))))
  
  (define (expand-install-global mod name type e)
    (build-global-definition
     no-source
     mod
     name
     (build-primcall
      no-source
      'make-syntax-transformer
      (list (build-data no-source
                        (if (eq? type 'define-syntax-parameter-form)
                            'syntax-parameter
                            'macro))
            e))))
  
  (define (parse-when-list e when-list)
    (let ((result (strip when-list)))
      (let lp ((l result))
        (match l
          (() result)
          ((x . l)
           (match x
             ((or 'compile 'load 'eval 'expand) (lp l))
             (_ (syntax-violation 'eval-when "invalid situation" e x))))))))

  (define (self-evaluating? x)
    (match x
      ((or ()
           (_ . _)
           (? vector?)) #f)
      (_ #t)))

  ;; syntax-type returns seven values: type, value, form, e, w, s, and
  ;; mod. The first two are described in the table below.
  ;;
  ;;    type                   value         explanation
  ;;    -------------------------------------------------------------------
  ;;    core                   procedure     core singleton
  ;;    core-form              procedure     core form
  ;;    lexical                name          lexical variable reference
  ;;    global                 name          global variable reference
  ;;    begin                  none          begin keyword
  ;;    define                 none          define keyword
  ;;    define-syntax          none          define-syntax keyword
  ;;    define-syntax-parameter none         define-syntax-parameter keyword
  ;;    local-syntax           rec?          letrec-syntax/let-syntax keyword
  ;;    eval-when              none          eval-when keyword
  ;;    syntax                 level         pattern variable
  ;;    displaced-lexical      none          displaced lexical identifier
  ;;    lexical-call           name          call to lexical variable
  ;;    global-call            name          call to global variable
  ;;    call                   none          any other call
  ;;    begin-form             none          begin expression
  ;;    define-form            id            variable definition
  ;;    define-syntax-form     id            syntax definition
  ;;    define-syntax-parameter-form id      syntax parameter definition
  ;;    local-syntax-form      rec?          syntax definition
  ;;    eval-when-form         none          eval-when form
  ;;    constant               none          self-evaluating datum
  ;;    other                  none          anything else
  ;;
  ;; form is the entire form.  For definition forms (define-form,
  ;; define-syntax-form, and define-syntax-parameter-form), e is the
  ;; rhs expression.  For all others, e is the entire form.  w is the
  ;; wrap for both form and e.  s is the source for the entire form.
  ;; mod is the module for both form and e.
  ;;
  ;; syntax-type expands macros and unwraps as necessary to get to one
  ;; of the forms above.  It also parses definition forms, although
  ;; perhaps this should be done by the consumer.

  (define (syntax-type e r w s rib mod for-car?)
    (cond
     ((symbol? e)
      (call-with-values (lambda () (resolve-identifier e w r mod #t))
        (lambda (type value mod*)
          (case type
            ((macro)
             (if for-car?
                 (values type value e e w s mod)
                 (syntax-type (expand-macro value e r w s rib mod)
                              r empty-wrap s rib mod #f)))
            ((global)
             ;; Toplevel definitions may resolve to bindings with
             ;; different names or in different modules.
             (values type value e value w s mod*))
            (else (values type value e e w s mod))))))
     ((pair? e)
      (let ((first (car e)))
        (call-with-values
            (lambda () (syntax-type first r w s rib mod #t))
          (lambda (ftype fval fform fe fw fs fmod)
            (case ftype
              ((lexical)
               (values 'lexical-call fval e e w s mod))
              ((global)
               ;; If we got here via an (@@ ...) expansion, we
               ;; need to make sure the fmod information is
               ;; propagated back correctly -- hence this
               ;; consing.
               (values 'global-call (make-syntax fval w fmod fs)
                       e e w s mod))
              ((macro)
               (syntax-type (expand-macro fval e r w s rib mod)
                            r empty-wrap s rib mod for-car?))
              ((module-ref)
               (call-with-values (lambda () (fval e r w mod))
                 (lambda (e r w s mod)
                   (syntax-type e r w s rib mod for-car?))))
              ((core)
               (values 'core-form fval e e w s mod))
              ((local-syntax)
               (values 'local-syntax-form fval e e w s mod))
              ((begin)
               (values 'begin-form #f e e w s mod))
              ((eval-when)
               (values 'eval-when-form #f e e w s mod))
              ((define)
               (syntax-case e ()
                 ((_ name val)
                  (id? #'name)
                  (values 'define-form #'name e #'val w s mod))
                 ((_ (name . args) e1 e2 ...)
                  (and (id? #'name)
                       (valid-bound-ids? (lambda-var-list #'args)))
                  ;; need lambda here...
                  (values 'define-form (wrap #'name w mod)
                          (wrap e w mod)
                          (source-wrap
                           (cons #'lambda (wrap #'(args e1 e2 ...) w mod))
                           empty-wrap s #f)
                          empty-wrap s mod))
                 ((_ name)
                  (id? #'name)
                  (values 'define-form (wrap #'name w mod)
                          (wrap e w mod)
                          #'(if #f #f)
                          empty-wrap s mod))))
              ((define-syntax)
               (syntax-case e ()
                 ((_ name val)
                  (id? #'name)
                  (values 'define-syntax-form #'name e #'val w s mod))))
              ((define-syntax-parameter)
               (syntax-case e ()
                 ((_ name val)
                  (id? #'name)
                  (values 'define-syntax-parameter-form #'name e #'val w s mod))))
              (else
               (values 'call #f e e w s mod)))))))
     ((syntax? e)
      (syntax-type (syntax-expression e)
                   r
                   (join-wraps w (syntax-wrap e))
                   (or (source-annotation e) s) rib
                   (or (syntax-module e) mod) for-car?))
     ((self-evaluating? e) (values 'constant #f e e w s mod))
     (else (values 'other #f e e w s mod))))

  (define (expand e r w mod)
    (call-with-values
        (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
      (lambda (type value form e w s mod)
        (expand-expr type value form e r w s mod))))

  (define (expand-expr type value form e r w s mod)
    (case type
      ((lexical)
       (build-lexical-reference s e value))
      ((core core-form)
       ;; apply transformer
       (value e r w s mod))
      ((module-ref)
       (call-with-values (lambda () (value e r w mod))
         (lambda (e r w s mod)
           (expand e r w mod))))
      ((lexical-call)
       (expand-call
        (let ((id (car e)))
          (build-lexical-reference (source-annotation id)
                                   (if (syntax? id)
                                       (syntax->datum id)
                                       id)
                                   value))
        e r w s mod))
      ((global-call)
       (expand-call
        (build-global-reference (or (source-annotation (car e)) s)
                                (if (syntax? value)
                                    (syntax-expression value)
                                    value)
                                (or (and (syntax? value)
                                         (syntax-module value))
                                    mod))
        e r w s mod))
      ((constant) (build-data s (strip e)))
      ((global) (build-global-reference s value mod))
      ((call) (expand-call (expand (car e) r w mod) e r w s mod))
      ((begin-form)
       (syntax-case e ()
         ((_ e1 e2 ...) (expand-sequence #'(e1 e2 ...) r w s mod))
         ((_)
          (syntax-violation #f "sequence of zero expressions"
                            (source-wrap e w s mod)))))
      ((local-syntax-form)
       (expand-local-syntax value e r w s mod expand-sequence))
      ((eval-when-form)
       (syntax-case e ()
         ((_ (x ...) e1 e2 ...)
          (let ((when-list (parse-when-list e #'(x ...))))
            (if (memq 'eval when-list)
                (expand-sequence #'(e1 e2 ...) r w s mod)
                (expand-void))))))
      ((define-form define-syntax-form define-syntax-parameter-form)
       (syntax-violation #f "definition in expression context, where definitions are not allowed,"
                         (source-wrap form w s mod)))
      ((syntax)
       (syntax-violation #f "reference to pattern variable outside syntax form"
                         (source-wrap e w s mod)))
      ((displaced-lexical)
       (syntax-violation #f "reference to identifier outside its scope"
                         (source-wrap e w s mod)))
      (else (syntax-violation #f "unexpected syntax"
                              (source-wrap e w s mod)))))

  (define (expand-call x e r w s mod)
    (syntax-case e ()
      ((e0 e1 ...)
       (build-call s x
                   (map (lambda (e) (expand e r w mod)) #'(e1 ...))))))

  ;; (What follows is my interpretation of what's going on here -- Andy)
  ;;
  ;; A macro takes an expression, a tree, the leaves of which are identifiers
  ;; and datums. Identifiers are symbols along with a wrap and a module. For
  ;; efficiency, subtrees that share wraps and modules may be grouped as one
  ;; syntax object.
  ;;
  ;; Going into the expansion, the expression is given an anti-mark, which
  ;; logically propagates to all leaves. Then, in the new expression returned
  ;; from the transfomer, if we see an expression with an anti-mark, we know it
  ;; pertains to the original expression; conversely, expressions without the
  ;; anti-mark are known to be introduced by the transformer.
  ;;
  ;; OK, good until now. We know this algorithm does lexical scoping
  ;; appropriately because it's widely known in the literature, and psyntax is
  ;; widely used. But what about modules? Here we're on our own. What we do is
  ;; to mark the module of expressions produced by a macro as pertaining to the
  ;; module that was current when the macro was defined -- that is, free
  ;; identifiers introduced by a macro are scoped in the macro's module, not in
  ;; the expansion's module. Seems to work well.
  ;;
  ;; The only wrinkle is when we want a macro to expand to code in another
  ;; module, as is the case for the r6rs `library' form -- the body expressions
  ;; should be scoped relative the the new module, the one defined by the macro.
  ;; For that, use `(@@ mod-name body)'.
  ;;
  ;; Part of the macro output will be from the site of the macro use and part
  ;; from the macro definition. We allow source information from the macro use
  ;; to pass through, but we annotate the parts coming from the macro with the
  ;; source location information corresponding to the macro use. It would be
  ;; really nice if we could also annotate introduced expressions with the
  ;; locations corresponding to the macro definition, but that is not yet
  ;; possible.
  (define (expand-macro p e r w s rib mod)
    (define (decorate-source x)
      (source-wrap x empty-wrap s #f))
    (define (map* f x)
      (match x
        (() '())
        ((x . x*) (cons (f x) (map* f x*)))
        (x (f x))))
    (define rebuild-macro-output
      (lambda (x m)
        (cond ((pair? x)
               (decorate-source
                (map* (lambda (x) (rebuild-macro-output x m)) x)))
              ((syntax? x)
               (let ((w (syntax-wrap x)))
                 (let ((ms (wrap-marks w)) (ss (wrap-subst w)))
                   (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                       ;; output is from original text
                       (wrap-syntax
                        x
                        (make-wrap (cdr ms)
                                   (if rib
                                       (cons rib (cdr ss))
                                       (cdr ss)))
                        mod)
                       ;; output introduced by macro
                       (wrap-syntax
                        x
                        (make-wrap (cons m ms)
                                   (if rib
                                       (cons rib (cons 'shift ss))
                                       (cons 'shift ss)))
                        mod)))))
              
              ((vector? x)
               (let* ((n (vector-length x))
                      (v (make-vector n)))
                 (do ((i 0 (1+ i)))
                     ((= i n) v)
                   (vector-set! v i
                                (rebuild-macro-output (vector-ref x i) m)))
                 (decorate-source v)))
              ((symbol? x)
               (syntax-violation #f "encountered raw symbol in macro output"
                                 (source-wrap e w (wrap-subst w) mod) x))
              (else (decorate-source x)))))
    (parameterize ((transformer-environment
                    (lambda (k) (k e r w s rib mod))))
      (rebuild-macro-output (p (source-wrap e (anti-mark w) s mod))
                            (new-mark))))

  (define (expand-body body outer-form r w mod)
    ;; In processing the forms of the body, we create a new, empty wrap.
    ;; This wrap is augmented (destructively) each time we discover that
    ;; the next form is a definition.  This is done:
    ;;
    ;;   (1) to allow the first nondefinition form to be a call to
    ;;       one of the defined ids even if the id previously denoted a
    ;;       definition keyword or keyword for a macro expanding into a
    ;;       definition;
    ;;   (2) to prevent subsequent definition forms (but unfortunately
    ;;       not earlier ones) and the first nondefinition form from
    ;;       confusing one of the bound identifiers for an auxiliary
    ;;       keyword; and
    ;;   (3) so that we do not need to restart the expansion of the
    ;;       first nondefinition form, which is problematic anyway
    ;;       since it might be the first element of a begin that we
    ;;       have just spliced into the body (meaning if we restarted,
    ;;       we'd really need to restart with the begin or the macro
    ;;       call that expanded into the begin, and we'd have to give
    ;;       up allowing (begin <defn>+ <expr>+), which is itself
    ;;       problematic since we don't know if a begin contains only
    ;;       definitions until we've expanded it).
    ;;
    ;; Before processing the body, we also create a new environment
    ;; containing a placeholder for the bindings we will add later and
    ;; associate this environment with each form.  In processing a
    ;; let-syntax or letrec-syntax, the associated environment may be
    ;; augmented with local keyword bindings, so the environment may
    ;; be different for different forms in the body.  Once we have
    ;; gathered up all of the definitions, we evaluate the transformer
    ;; expressions and splice into r at the placeholder the new variable
    ;; and keyword bindings.  This allows let-syntax or letrec-syntax
    ;; forms local to a portion or all of the body to shadow the
    ;; definition bindings.
    ;;
    ;; Subforms of a begin, let-syntax, or letrec-syntax are spliced
    ;; into the body.
    ;;
    ;; outer-form is fully wrapped w/source
    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))))
      (let parse ((body (map (lambda (x) (cons r (wrap x w mod))) body))
                  (ids '()) (labels '())
                  (var-ids '()) (vars '()) (vals '()) (bindings '())
                  (expand-tail-expr #f))
        (cond
         ((null? body)
          (unless expand-tail-expr
            (when (null? ids)
              (syntax-violation #f "empty body" outer-form))
            (syntax-violation #f "body should end with an expression" outer-form))
          (unless (valid-bound-ids? ids)
            (syntax-violation
             #f "invalid or duplicate identifier in definition"
             outer-form))
          (set-cdr! r (extend-env labels bindings (cdr r)))
          (let ((src (source-annotation outer-form)))
            (let lp ((var-ids var-ids) (vars vars) (vals vals)
                     (tail (expand-tail-expr)))
              (cond
               ((null? var-ids) tail)
               ((not (car var-ids))
                (lp (cdr var-ids) (cdr vars) (cdr vals)
                    (make-seq src ((car vals)) tail)))
               (else
                (let ((var-ids (map (lambda (id)
                                      (if id (syntax->datum id) '_))
                                    (reverse var-ids)))
                      (vars (map (lambda (var) (or var (gen-lexical '_)))
                                 (reverse vars)))
                      (vals (map (lambda (expand-expr id)
                                   (if id
                                       (expand-expr)
                                       (make-seq src
                                                 (expand-expr)
                                                 (build-void src))))
                                 (reverse vals) (reverse var-ids))))
                  (build-letrec src #t var-ids vars vals tail)))))))
         (expand-tail-expr
          (parse body ids labels
                 (cons #f var-ids)
                 (cons #f vars)
                 (cons expand-tail-expr vals)
                 bindings #f))
         (else
          (let ((e (cdar body)) (er (caar body)) (body (cdr body)))
            (call-with-values
                (lambda () (syntax-type e er empty-wrap (source-annotation e) ribcage mod #f))
              (lambda (type value form e w s mod)
                (case type
                  ((define-form)
                   (let ((id (wrap value w mod)) (label (gen-label)))
                     (let ((var (gen-var id)))
                       (extend-ribcage! ribcage id label)
                       (parse body
                              (cons id ids) (cons label labels)
                              (cons id var-ids)
                              (cons var vars)
                              (cons (let ((wrapped (source-wrap e w s mod)))
                                      (lambda ()
                                        (expand wrapped er empty-wrap mod)))
                                    vals)
                              (cons (make-binding 'lexical var) bindings)
                              #f))))
                  ((define-syntax-form)
                   (let ((id (wrap value w mod))
                         (label (gen-label))
                         (trans-r (macros-only-env er)))
                     (extend-ribcage! ribcage id label)
                     ;; As required by R6RS, evaluate the right-hand-sides of internal
                     ;; syntax definition forms and add their transformers to the
                     ;; compile-time environment immediately, so that the newly-defined
                     ;; keywords may be used in definition context within the same
                     ;; lexical contour.
                     (set-cdr! r (extend-env
                                  (list label)
                                  (list (make-binding
                                         'macro
                                         (eval-local-transformer
                                          (expand e trans-r w mod)
                                          mod)))
                                  (cdr r)))
                     (parse body (cons id ids)
                            labels var-ids vars vals bindings #f)))
                  ((define-syntax-parameter-form)
                   ;; Same as define-syntax-form, different binding type though.
                   (let ((id (wrap value w mod))
                         (label (gen-label))
                         (trans-r (macros-only-env er)))
                     (extend-ribcage! ribcage id label)
                     (set-cdr! r (extend-env
                                  (list label)
                                  (list (make-binding
                                         'syntax-parameter
                                         (eval-local-transformer
                                          (expand e trans-r w mod)
                                          mod)))
                                  (cdr r)))
                     (parse body (cons id ids)
                            labels var-ids vars vals bindings #f)))
                  ((begin-form)
                   (syntax-case e ()
                     ((_ e1 ...)
                      (parse (let f ((forms #'(e1 ...)))
                               (if (null? forms)
                                   body
                                   (cons (cons er (wrap (car forms) w mod))
                                         (f (cdr forms)))))
                             ids labels var-ids vars vals bindings #f))))
                  ((local-syntax-form)
                   (expand-local-syntax
                    value e er w s mod
                    (lambda (forms er w s mod)
                      (parse (let f ((forms forms))
                               (if (null? forms)
                                   body
                                   (cons (cons er (wrap (car forms) w mod))
                                         (f (cdr forms)))))
                             ids labels var-ids vars vals bindings #f))))
                  (else           ; An expression, not a definition.
                   (let ((wrapped (source-wrap e w s mod)))
                     (parse body ids labels var-ids vars vals bindings
                            (lambda ()
                              (expand wrapped er empty-wrap mod))))))))))))))

  (define (expand-local-syntax rec? e r w s mod k)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (let ((ids #'(id ...)))
         (if (not (valid-bound-ids? ids))
             (syntax-violation #f "duplicate bound keyword" e)
             (let ((labels (gen-labels ids)))
               (let ((new-w (make-binding-wrap ids labels w)))
                 (k #'(e1 e2 ...)
                    (extend-env
                     labels
                     (let ((w (if rec? new-w w))
                           (trans-r (macros-only-env r)))
                       (map (lambda (x)
                              (make-binding 'macro
                                            (eval-local-transformer
                                             (expand x trans-r w mod)
                                             mod)))
                            #'(val ...)))
                     r)
                    new-w
                    s
                    mod))))))
      (_ (syntax-violation #f "bad local syntax definition"
                           (source-wrap e w s mod)))))

  (define (eval-local-transformer expanded mod)
    (let ((p (local-eval expanded mod)))
      (unless (procedure? p)
        (syntax-violation #f "nonprocedure transformer" p))
      p))

  (define (expand-void)
    (build-void no-source))

  (define (ellipsis? e r mod)
    (and (nonsymbol-id? e)
         ;; If there is a binding for the special identifier
         ;; #{ $sc-ellipsis }# in the lexical environment of E,
         ;; and if the associated binding type is 'ellipsis',
         ;; then the binding's value specifies the custom ellipsis
         ;; identifier within that lexical environment, and the
         ;; comparison is done using 'bound-id=?'.
         (call-with-values
             (lambda () (resolve-identifier
                         (make-syntax '#{ $sc-ellipsis }#
                                      (syntax-wrap e)
                                      (or (syntax-module e) mod)
                                      #f)
                         empty-wrap r mod #f))
           (lambda (type value mod)
             (if (eq? type 'ellipsis)
                 (bound-id=? e value)
                 (free-id=? e #'(... ...)))))))

  (define (lambda-formals orig-args)
    (define (req args rreq)
      (syntax-case args ()
        (()
         (check (reverse rreq) #f))
        ((a . b) (id? #'a)
         (req #'b (cons #'a rreq)))
        (r (id? #'r)
           (check (reverse rreq) #'r))
        (else
         (syntax-violation 'lambda "invalid argument list" orig-args args))))
    (define (check req rest)
      (cond
       ((distinct-bound-ids? (if rest (cons rest req) req))
        (values req #f rest #f))
       (else
        (syntax-violation 'lambda "duplicate identifier in argument list"
                          orig-args))))
    (req orig-args '()))

  (define (expand-simple-lambda e r w s mod req rest meta body)
    (let* ((ids (if rest (append req (list rest)) req))
           (vars (map gen-var ids))
           (labels (gen-labels ids)))
      (build-simple-lambda
       s
       (map syntax->datum req) (and rest (syntax->datum rest)) vars
       meta
       (expand-body body (source-wrap e w s mod)
                    (extend-var-env labels vars r)
                    (make-binding-wrap ids labels w)
                    mod))))

  (define (lambda*-formals orig-args)
    (define (req args rreq)
      (syntax-case args ()
        (()
         (check (reverse rreq) '() #f '()))
        ((a . b) (id? #'a)
         (req #'b (cons #'a rreq)))
        ((a . b) (eq? (syntax->datum #'a) #:optional)
         (opt #'b (reverse rreq) '()))
        ((a . b) (eq? (syntax->datum #'a) #:key)
         (key #'b (reverse rreq) '() '()))
        ((a b) (eq? (syntax->datum #'a) #:rest)
         (rest #'b (reverse rreq) '() '()))
        (r (id? #'r)
           (rest #'r (reverse rreq) '() '()))
        (else
         (syntax-violation 'lambda* "invalid argument list" orig-args args))))
    (define (opt args req ropt)
      (syntax-case args ()
        (()
         (check req (reverse ropt) #f '()))
        ((a . b) (id? #'a)
         (opt #'b req (cons #'(a #f) ropt)))
        (((a init) . b) (id? #'a)
         (opt #'b req (cons #'(a init) ropt)))
        ((a . b) (eq? (syntax->datum #'a) #:key)
         (key #'b req (reverse ropt) '()))
        ((a b) (eq? (syntax->datum #'a) #:rest)
         (rest #'b req (reverse ropt) '()))
        (r (id? #'r)
           (rest #'r req (reverse ropt) '()))
        (else
         (syntax-violation 'lambda* "invalid optional argument list"
                           orig-args args))))
    (define (key args req opt rkey)
      (syntax-case args ()
        (()
         (check req opt #f (cons #f (reverse rkey))))
        ((a . b) (id? #'a)
         (with-syntax ((k (symbol->keyword (syntax->datum #'a))))
           (key #'b req opt (cons #'(k a #f) rkey))))
        (((a init) . b) (id? #'a)
         (with-syntax ((k (symbol->keyword (syntax->datum #'a))))
           (key #'b req opt (cons #'(k a init) rkey))))
        (((a init k) . b) (and (id? #'a)
                               (keyword? (syntax->datum #'k)))
         (key #'b req opt (cons #'(k a init) rkey)))
        ((aok) (eq? (syntax->datum #'aok) #:allow-other-keys)
         (check req opt #f (cons #t (reverse rkey))))
        ((aok a b) (and (eq? (syntax->datum #'aok) #:allow-other-keys)
                        (eq? (syntax->datum #'a) #:rest))
         (rest #'b req opt (cons #t (reverse rkey))))
        ((aok . r) (and (eq? (syntax->datum #'aok) #:allow-other-keys)
                        (id? #'r))
         (rest #'r req opt (cons #t (reverse rkey))))
        ((a b) (eq? (syntax->datum #'a) #:rest)
         (rest #'b req opt (cons #f (reverse rkey))))
        (r (id? #'r)
           (rest #'r req opt (cons #f (reverse rkey))))
        (else
         (syntax-violation 'lambda* "invalid keyword argument list"
                           orig-args args))))
    (define (rest args req opt kw)
      (syntax-case args ()
        (r (id? #'r)
           (check req opt #'r kw))
        (else
         (syntax-violation 'lambda* "invalid rest argument"
                           orig-args args))))
    (define (check req opt rest kw)
      (cond
       ((distinct-bound-ids?
         (append req (map car opt) (if rest (list rest) '())
                 (if (pair? kw) (map cadr (cdr kw)) '())))
        (values req opt rest kw))
       (else
        (syntax-violation 'lambda* "duplicate identifier in argument list"
                          orig-args))))
    (req orig-args '()))

  (define (expand-lambda-case e r w s mod get-formals clauses)
    (define (parse-req req opt rest kw body)
      (let ((vars (map gen-var req))
            (labels (gen-labels req)))
        (let ((r* (extend-var-env labels vars r))
              (w* (make-binding-wrap req labels w)))
          (parse-opt (map syntax->datum req)
                     opt rest kw body (reverse vars) r* w* '() '()))))
    (define (parse-opt req opt rest kw body vars r* w* out inits)
      (cond
       ((pair? opt)
        (syntax-case (car opt) ()
          ((id i)
           (let* ((v (gen-var #'id))
                  (l (gen-labels (list v)))
                  (r** (extend-var-env l (list v) r*))
                  (w** (make-binding-wrap (list #'id) l w*)))
             (parse-opt req (cdr opt) rest kw body (cons v vars)
                        r** w** (cons (syntax->datum #'id) out)
                        (cons (expand #'i r* w* mod) inits))))))
       (rest
        (let* ((v (gen-var rest))
               (l (gen-labels (list v)))
               (r* (extend-var-env l (list v) r*))
               (w* (make-binding-wrap (list rest) l w*)))
          (parse-kw req (if (pair? out) (reverse out) #f)
                    (syntax->datum rest)
                    (if (pair? kw) (cdr kw) kw)
                    body (cons v vars) r* w* 
                    (if (pair? kw) (car kw) #f)
                    '() inits)))
       (else
        (parse-kw req (if (pair? out) (reverse out) #f) #f
                  (if (pair? kw) (cdr kw) kw)
                  body vars r* w*
                  (if (pair? kw) (car kw) #f)
                  '() inits))))
    (define (parse-kw req opt rest kw body vars r* w* aok out inits)
      (cond
       ((pair? kw)
        (syntax-case (car kw) ()
          ((k id i)
           (let* ((v (gen-var #'id))
                  (l (gen-labels (list v)))
                  (r** (extend-var-env l (list v) r*))
                  (w** (make-binding-wrap (list #'id) l w*)))
             (parse-kw req opt rest (cdr kw) body (cons v vars)
                       r** w** aok
                       (cons (list (syntax->datum #'k)
                                   (syntax->datum #'id)
                                   v)
                             out)
                       (cons (expand #'i r* w* mod) inits))))))
       (else
        (parse-body req opt rest
                    (if (or aok (pair? out)) (cons aok (reverse out)) #f)
                    body (reverse vars) r* w* (reverse inits) '()))))
    (define (parse-body req opt rest kw body vars r* w* inits meta)
      (syntax-case body ()
        ((docstring e1 e2 ...) (string? (syntax->datum #'docstring))
         (parse-body req opt rest kw #'(e1 e2 ...) vars r* w* inits
                     (append meta 
                             `((documentation
                                . ,(syntax->datum #'docstring))))))
        ((#((k . v) ...) e1 e2 ...) 
         (parse-body req opt rest kw #'(e1 e2 ...) vars r* w* inits
                     (append meta (syntax->datum #'((k . v) ...)))))
        ((e1 e2 ...)
         (values meta req opt rest kw inits vars
                 (expand-body #'(e1 e2 ...) (source-wrap e w s mod)
                              r* w* mod)))))

    (syntax-case clauses ()
      (() (values '() #f))
      (((args e1 e2 ...) (args* e1* e2* ...) ...)
       (call-with-values (lambda () (get-formals #'args))
         (lambda (req opt rest kw)
           (call-with-values (lambda ()
                               (parse-req req opt rest kw #'(e1 e2 ...)))
             (lambda (meta req opt rest kw inits vars body)
               (call-with-values
                   (lambda ()
                     (expand-lambda-case e r w s mod get-formals
                                         #'((args* e1* e2* ...) ...)))
                 (lambda (meta* else*)
                   (values
                    (append meta meta*)
                    (build-lambda-case s req opt rest kw inits vars
                                       body else*)))))))))))

  ;; data

  ;; strips syntax objects, recursively.

  (define (strip x)
    (cond
     ((syntax? x)
      (strip (syntax-expression x)))
     ((pair? x)
      (cons (strip (car x)) (strip (cdr x))))
     ((vector? x)
      (list->vector (strip (vector->list x))))
     (else x)))

  ;; lexical variables

  (define (gen-var id)
    (let ((id (if (syntax? id) (syntax-expression id) id)))
      (gen-lexical id)))

  ;; appears to return a reversed list
  (define (lambda-var-list vars)
    (let lvl ((vars vars) (ls '()) (w empty-wrap))
      (cond
       ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w #f) ls) w))
       ((id? vars) (cons (wrap vars w #f) ls))
       ((null? vars) ls)
       ((syntax? vars)
        (lvl (syntax-expression vars)
             ls
             (join-wraps w (syntax-wrap vars))))
       ;; include anything else to be caught by subsequent error
       ;; checking
       (else (cons vars ls)))))

  ;; core transformers

  (define (expand-syntax-parameterize e r w s mod)
    (syntax-case e ()
      ((_ ((var val) ...) e1 e2 ...)
       (valid-bound-ids? #'(var ...))
       (let ((names
              (map (lambda (x)
                     (call-with-values
                         (lambda () (resolve-identifier x w r mod #f))
                       (lambda (type value mod)
                         (case type
                           ((displaced-lexical)
                            (syntax-violation 'syntax-parameterize
                                              "identifier out of context"
                                              e
                                              (source-wrap x w s mod)))
                           ((syntax-parameter)
                            value)
                           (else
                            (syntax-violation 'syntax-parameterize
                                              "invalid syntax parameter"
                                              e
                                              (source-wrap x w s mod)))))))
                   #'(var ...)))
             (bindings
              (let ((trans-r (macros-only-env r)))
                (map (lambda (x)
                       (make-binding
                        'syntax-parameter
                        (eval-local-transformer (expand x trans-r w mod) mod)))
                     #'(val ...)))))
         (expand-body #'(e1 e2 ...)
                      (source-wrap e w s mod)
                      (extend-env names bindings r)
                      w
                      mod)))
      (_ (syntax-violation 'syntax-parameterize "bad syntax"
                           (source-wrap e w s mod)))))

  (define (expand-quote e r w s mod)
    (syntax-case e ()
      ((_ e) (build-data s (strip #'e)))
      (_ (syntax-violation 'quote "bad syntax"
                           (source-wrap e w s mod)))))

  (define (expand-quote-syntax e r w s mod)
    (syntax-case (source-wrap e w s mod) ()
      ((_ e) (build-data s #'e))
      (e (syntax-violation 'quote "bad syntax" #'e))))

  (define expand-syntax
    (let ()
      (define (gen-syntax src e r maps ellipsis? mod)
        (if (id? e)
            (call-with-values (lambda ()
                                (resolve-identifier e empty-wrap r mod #f))
              (lambda (type value mod)
                (case type
                  ((syntax)
                   (call-with-values
                       (lambda () (gen-ref src (car value) (cdr value) maps))
                     (lambda (var maps)
                       (values `(ref ,var) maps))))
                  (else
                   (if (ellipsis? e r mod)
                       (syntax-violation 'syntax "misplaced ellipsis" src)
                       (values `(quote ,e) maps))))))
            (syntax-case e ()
              ((dots e)
               (ellipsis? #'dots r mod)
               (gen-syntax src #'e r maps (lambda (e r mod) #f) mod))
              ((x dots . y)
               ;; this could be about a dozen lines of code, except that we
               ;; choose to handle #'(x ... ...) forms
               (ellipsis? #'dots r mod)
               (let f ((y #'y)
                       (k (lambda (maps)
                            (call-with-values
                                (lambda ()
                                  (gen-syntax src #'x r
                                              (cons '() maps) ellipsis? mod))
                              (lambda (x maps)
                                (if (null? (car maps))
                                    (syntax-violation 'syntax "extra ellipsis"
                                                      src)
                                    (values (gen-map x (car maps))
                                            (cdr maps))))))))
                 (syntax-case y ()
                   ((dots . y)
                    (ellipsis? #'dots r mod)
                    (f #'y
                       (lambda (maps)
                         (call-with-values
                             (lambda () (k (cons '() maps)))
                           (lambda (x maps)
                             (if (null? (car maps))
                                 (syntax-violation 'syntax "extra ellipsis" src)
                                 (values (gen-mappend x (car maps))
                                         (cdr maps))))))))
                   (_ (call-with-values
                          (lambda () (gen-syntax src y r maps ellipsis? mod))
                        (lambda (y maps)
                          (call-with-values
                              (lambda () (k maps))
                            (lambda (x maps)
                              (values (gen-append x y) maps)))))))))
              ((x . y)
               (call-with-values
                   (lambda () (gen-syntax src #'x r maps ellipsis? mod))
                 (lambda (x maps)
                   (call-with-values
                       (lambda () (gen-syntax src #'y r maps ellipsis? mod))
                     (lambda (y maps) (values (gen-cons x y) maps))))))
              (#(e1 e2 ...)
               (call-with-values
                   (lambda ()
                     (gen-syntax src #'(e1 e2 ...) r maps ellipsis? mod))
                 (lambda (e maps) (values (gen-vector e) maps))))
              (x (eq? (syntax->datum #'x) #nil) (values '(quote #nil) maps))
              (() (values '(quote ()) maps))
              (_ (values `(quote ,e) maps)))))

      (define (gen-ref src var level maps)
        (if (zero? level)
            (values var maps)
            (if (null? maps)
                (syntax-violation 'syntax "missing ellipsis" src)
                (call-with-values
                    (lambda () (gen-ref src var (1- level) (cdr maps)))
                  (lambda (outer-var outer-maps)
                    (let ((b (assq outer-var (car maps))))
                      (if b
                          (values (cdr b) maps)
                          (let ((inner-var (gen-var 'tmp)))
                            (values inner-var
                                    (cons (cons (cons outer-var inner-var)
                                                (car maps))
                                          outer-maps))))))))))

      (define (gen-mappend e map-env)
        `(apply (primitive append) ,(gen-map e map-env)))

      (define (gen-map e map-env)
        (let ((formals (map cdr map-env))
              (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
          (cond
           ((eq? (car e) 'ref)
            ;; identity map equivalence:
            ;; (map (lambda (x) x) y) == y
            (car actuals))
           ((and-map
             (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
             (cdr e))
            ;; eta map equivalence:
            ;; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
            `(map (primitive ,(car e))
                  ,@(map (let ((r (map cons formals actuals)))
                           (lambda (x) (cdr (assq (cadr x) r))))
                         (cdr e))))
           (else `(map (lambda ,formals ,e) ,@actuals)))))

      (define (gen-cons x y)
        (case (car y)
          ((quote)
           (if (eq? (car x) 'quote)
               `(quote (,(cadr x) . ,(cadr y)))
               (if (eq? (cadr y) '())
                   `(list ,x)
                   `(cons ,x ,y))))
          ((list) `(list ,x ,@(cdr y)))
          (else `(cons ,x ,y))))

      (define (gen-append x y)
        (if (equal? y '(quote ()))
            x
            `(append ,x ,y)))

      (define (gen-vector x)
        (cond
         ((eq? (car x) 'list) `(vector ,@(cdr x)))
         ((eq? (car x) 'quote) `(quote #(,@(cadr x))))
         (else `(list->vector ,x))))


      (define (regen x)
        (case (car x)
          ((ref) (build-lexical-reference no-source (cadr x) (cadr x)))
          ((primitive) (build-primref no-source (cadr x)))
          ((quote) (build-data no-source (cadr x)))
          ((lambda)
           (if (list? (cadr x))
               (build-simple-lambda no-source (cadr x) #f (cadr x) '() (regen (caddr x)))
               (error "how did we get here" x)))
          (else (build-primcall no-source (car x) (map regen (cdr x))))))

      (lambda (e r w s mod)
        (let ((e (source-wrap e w s mod)))
          (syntax-case e ()
            ((_ x)
             (call-with-values
                 (lambda () (gen-syntax e #'x r '() ellipsis? mod))
               (lambda (e maps) (regen e))))
            (_ (syntax-violation 'syntax "bad `syntax' form" e)))))))

  (define (expand-lambda e r w s mod)
    (syntax-case e ()
      ((_ args e1 e2 ...)
       (call-with-values (lambda () (lambda-formals #'args))
         (lambda (req opt rest kw)
           (let lp ((body #'(e1 e2 ...)) (meta '()))
             (syntax-case body ()
               ((docstring e1 e2 ...) (string? (syntax->datum #'docstring))
                (lp #'(e1 e2 ...)
                    (append meta
                            `((documentation
                               . ,(syntax->datum #'docstring))))))
               ((#((k . v) ...) e1 e2 ...)
                (lp #'(e1 e2 ...)
                    (append meta (syntax->datum #'((k . v) ...)))))
               (_ (expand-simple-lambda e r w s mod req rest meta body)))))))
      (_ (syntax-violation 'lambda "bad lambda" e))))
  
  (define (expand-lambda* e r w s mod)
    (syntax-case e ()
      ((_ args e1 e2 ...)
       (call-with-values
           (lambda ()
             (expand-lambda-case e r w s mod
                                 lambda*-formals #'((args e1 e2 ...))))
         (lambda (meta lcase)
           (build-case-lambda s meta lcase))))
      (_ (syntax-violation 'lambda "bad lambda*" e))))

  (define (expand-case-lambda e r w s mod)
    (define (build-it meta clauses)
      (call-with-values
          (lambda ()
            (expand-lambda-case e r w s mod
                                lambda-formals
                                clauses))
        (lambda (meta* lcase)
          (build-case-lambda s (append meta meta*) lcase))))
    (syntax-case e ()
      ((_ (args e1 e2 ...) ...)
       (build-it '() #'((args e1 e2 ...) ...)))
      ((_ docstring (args e1 e2 ...) ...)
       (string? (syntax->datum #'docstring))
       (build-it `((documentation
                    . ,(syntax->datum #'docstring)))
                 #'((args e1 e2 ...) ...)))
      (_ (syntax-violation 'case-lambda "bad case-lambda" e))))

  (define (expand-case-lambda* e r w s mod)
    (define (build-it meta clauses)
      (call-with-values
          (lambda ()
            (expand-lambda-case e r w s mod
                                lambda*-formals
                                clauses))
        (lambda (meta* lcase)
          (build-case-lambda s (append meta meta*) lcase))))
    (syntax-case e ()
      ((_ (args e1 e2 ...) ...)
       (build-it '() #'((args e1 e2 ...) ...)))
      ((_ docstring (args e1 e2 ...) ...)
       (string? (syntax->datum #'docstring))
       (build-it `((documentation
                    . ,(syntax->datum #'docstring)))
                 #'((args e1 e2 ...) ...)))
      (_ (syntax-violation 'case-lambda "bad case-lambda*" e))))

  (define (expand-with-ellipsis e r w s mod)
    (syntax-case e ()
      ((_ dots e1 e2 ...)
       (id? #'dots)
       (let ((id (if (symbol? #'dots)
                     '#{ $sc-ellipsis }#
                     (make-syntax '#{ $sc-ellipsis }#
                                  (syntax-wrap #'dots)
                                  (syntax-module #'dots)
                                  (syntax-sourcev #'dots)))))
         (let ((ids (list id))
               (labels (list (gen-label)))
               (bindings (list (make-binding 'ellipsis (source-wrap #'dots w s mod)))))
           (let ((nw (make-binding-wrap ids labels w))
                 (nr (extend-env labels bindings r)))
             (expand-body #'(e1 e2 ...) (source-wrap e nw s mod) nr nw mod)))))
      (_ (syntax-violation 'with-ellipsis "bad syntax"
                           (source-wrap e w s mod)))))

  (define expand-let
    (let ()
      (define (expand-let e r w s mod constructor ids vals exps)
        (if (not (valid-bound-ids? ids))
            (syntax-violation 'let "duplicate bound variable" e)
            (let ((labels (gen-labels ids))
                  (new-vars (map gen-var ids)))
              (let ((nw (make-binding-wrap ids labels w))
                    (nr (extend-var-env labels new-vars r)))
                (constructor s
                             (map syntax->datum ids)
                             new-vars
                             (map (lambda (x) (expand x r w mod)) vals)
                             (expand-body exps (source-wrap e nw s mod)
                                          nr nw mod))))))
      (lambda (e r w s mod)
        (syntax-case e ()
          ((_ ((id val) ...) e1 e2 ...)
           (and-map id? #'(id ...))
           (expand-let e r w s mod
                       build-let
                       #'(id ...)
                       #'(val ...)
                       #'(e1 e2 ...)))
          ((_ f ((id val) ...) e1 e2 ...)
           (and (id? #'f) (and-map id? #'(id ...)))
           (expand-let e r w s mod
                       build-named-let
                       #'(f id ...)
                       #'(val ...)
                       #'(e1 e2 ...)))
          (_ (syntax-violation 'let "bad let" (source-wrap e w s mod)))))))

  (define (expand-letrec e r w s mod)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (and-map id? #'(id ...))
       (let ((ids #'(id ...)))
         (if (not (valid-bound-ids? ids))
             (syntax-violation 'letrec "duplicate bound variable" e)
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (let ((w (make-binding-wrap ids labels w))
                     (r (extend-var-env labels new-vars r)))
                 (build-letrec s #f
                               (map syntax->datum ids)
                               new-vars
                               (map (lambda (x) (expand x r w mod)) #'(val ...))
                               (expand-body #'(e1 e2 ...)
                                            (source-wrap e w s mod) r w mod)))))))
      (_ (syntax-violation 'letrec "bad letrec" (source-wrap e w s mod)))))

  (define (expand-letrec* e r w s mod)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (and-map id? #'(id ...))
       (let ((ids #'(id ...)))
         (if (not (valid-bound-ids? ids))
             (syntax-violation 'letrec* "duplicate bound variable" e)
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (let ((w (make-binding-wrap ids labels w))
                     (r (extend-var-env labels new-vars r)))
                 (build-letrec s #t
                               (map syntax->datum ids)
                               new-vars
                               (map (lambda (x) (expand x r w mod)) #'(val ...))
                               (expand-body #'(e1 e2 ...)
                                            (source-wrap e w s mod) r w mod)))))))
      (_ (syntax-violation 'letrec* "bad letrec*" (source-wrap e w s mod)))))

  (define (expand-set! e r w s mod)
    (syntax-case e ()
      ((_ id val)
       (id? #'id)
       (call-with-values
           (lambda () (resolve-identifier #'id w r mod #t))
         (lambda (type value id-mod)
           (case type
             ((lexical)
              (build-lexical-assignment s (syntax->datum #'id) value
                                        (expand #'val r w mod)))
             ((global)
              (build-global-assignment s value (expand #'val r w mod) id-mod))
             ((macro)
              (if (procedure-property value 'variable-transformer)
                  ;; As syntax-type does, call expand-macro with
                  ;; the mod of the expression. Hmm.
                  (expand (expand-macro value e r w s #f mod) r empty-wrap mod)
                  (syntax-violation 'set! "not a variable transformer"
                                    (wrap e w mod)
                                    (wrap #'id w id-mod))))
             ((displaced-lexical)
              (syntax-violation 'set! "identifier out of context"
                                (wrap #'id w mod)))
             (else
              (syntax-violation 'set! "bad set!" (source-wrap e w s mod)))))))
      ((_ (head tail ...) val)
       (call-with-values
           (lambda () (syntax-type #'head r empty-wrap no-source #f mod #t))
         (lambda (type value ee* ee ww ss modmod)
           (case type
             ((module-ref)
              (let ((val (expand #'val r w mod)))
                (call-with-values (lambda () (value #'(head tail ...) r w mod))
                  (lambda (e r w s* mod)
                    (syntax-case e ()
                      (e (id? #'e)
                         (build-global-assignment s (syntax->datum #'e)
                                                  val mod)))))))
             (else
              (build-call s
                          (expand #'(setter head) r w mod)
                          (map (lambda (e) (expand e r w mod))
                               #'(tail ... val))))))))
      (_ (syntax-violation 'set! "bad set!" (source-wrap e w s mod)))))

  (define (expand-public-ref e r w mod)
    (syntax-case e ()
      ((_ (mod ...) id)
       (and (and-map id? #'(mod ...)) (id? #'id))
       ;; Strip the wrap from the identifier and return top-wrap
       ;; so that the identifier will not be captured by lexicals.
       (values (syntax->datum #'id) r top-wrap #f
               (syntax->datum
                #'(public mod ...))))))

  (define (expand-private-ref e r w mod)
    (define (remodulate x mod)
      (cond ((pair? x)
             (cons (remodulate (car x) mod)
                   (remodulate (cdr x) mod)))
            ((syntax? x)
             (make-syntax
              (remodulate (syntax-expression x) mod)
              (syntax-wrap x)
              ;; hither the remodulation
              mod
              (syntax-sourcev x)))
            ((vector? x)
             (let* ((n (vector-length x)) (v (make-vector n)))
               (do ((i 0 (1+ i)))
                   ((= i n) v)
                 (vector-set! v i (remodulate (vector-ref x i) mod)))))
            (else x)))
    (syntax-case e (@@)
      ((_ (mod ...) id)
       (and (and-map id? #'(mod ...)) (id? #'id))
       ;; Strip the wrap from the identifier and return top-wrap
       ;; so that the identifier will not be captured by lexicals.
       (values (syntax->datum #'id) r top-wrap #f
               (syntax->datum
                #'(private mod ...))))
      ((_ @@ (mod ...) exp)
       (and-map id? #'(mod ...))
       ;; This is a special syntax used to support R6RS library forms.
       ;; Unlike the syntax above, the last item is not restricted to
       ;; be a single identifier, and the syntax objects are kept
       ;; intact, with only their module changed.
       (let ((mod (syntax->datum #'(private mod ...))))
         (values (remodulate #'exp mod)
                 r w (source-annotation #'exp)
                 mod)))))

  (define (expand-if e r w s mod)
    (syntax-case e ()
      ((_ test then)
       (build-conditional
        s
        (expand #'test r w mod)
        (expand #'then r w mod)
        (build-void no-source)))
      ((_ test then else)
       (build-conditional
        s
        (expand #'test r w mod)
        (expand #'then r w mod)
        (expand #'else r w mod)))))

  (define expand-syntax-case
    (let ()
      (define (convert-pattern pattern keys ellipsis?)
        ;; accepts pattern & keys
        ;; returns $sc-dispatch pattern & ids
        (define cvt*
          (lambda (p* n ids)
            (syntax-case p* ()
              ((x . y)
               (call-with-values
                   (lambda () (cvt* #'y n ids))
                 (lambda (y ids)
                   (call-with-values
                       (lambda () (cvt #'x n ids))
                     (lambda (x ids)
                       (values (cons x y) ids))))))
              (_ (cvt p* n ids)))))

        (define (v-reverse x)
          (let loop ((r '()) (x x))
            (if (not (pair? x))
                (values r x)
                (loop (cons (car x) r) (cdr x)))))

        (define cvt
          (lambda (p n ids)
            (if (id? p)
                (cond
                 ((bound-id-member? p keys)
                  (values (vector 'free-id p) ids))
                 ((free-id=? p #'_)
                  (values '_ ids))
                 (else
                  (values 'any (cons (cons p n) ids))))
                (syntax-case p ()
                  ((x dots)
                   (ellipsis? (syntax dots))
                   (call-with-values
                       (lambda () (cvt (syntax x) (1+ n) ids))
                     (lambda (p ids)
                       (values (if (eq? p 'any) 'each-any (vector 'each p))
                               ids))))
                  ((x dots . ys)
                   (ellipsis? (syntax dots))
                   (call-with-values
                       (lambda () (cvt* (syntax ys) n ids))
                     (lambda (ys ids)
                       (call-with-values
                           (lambda () (cvt (syntax x) (1+ n) ids))
                         (lambda (x ids)
                           (call-with-values
                               (lambda () (v-reverse ys))
                             (lambda (ys e)
                               (values `#(each+ ,x ,ys ,e) 
                                       ids))))))))
                  ((x . y)
                   (call-with-values
                       (lambda () (cvt (syntax y) n ids))
                     (lambda (y ids)
                       (call-with-values
                           (lambda () (cvt (syntax x) n ids))
                         (lambda (x ids)
                           (values (cons x y) ids))))))
                  (() (values '() ids))
                  (#(x ...)
                   (call-with-values
                       (lambda () (cvt (syntax (x ...)) n ids))
                     (lambda (p ids) (values (vector 'vector p) ids))))
                  (x (values (vector 'atom (strip p)) ids))))))
        (cvt pattern 0 '()))

      (define (build-dispatch-call pvars exp y r mod)
        (let ((ids (map car pvars)) (levels (map cdr pvars)))
          (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
            (build-primcall
             no-source
             'apply
             (list (build-simple-lambda no-source (map syntax->datum ids) #f new-vars '()
                                        (expand exp
                                                (extend-env
                                                 labels
                                                 (map (lambda (var level)
                                                        (make-binding 'syntax `(,var . ,level)))
                                                      new-vars
                                                      (map cdr pvars))
                                                 r)
                                                (make-binding-wrap ids labels empty-wrap)
                                                mod))
                   y)))))

      (define (gen-clause x keys clauses r pat fender exp mod)
        (call-with-values
            (lambda () (convert-pattern pat keys (lambda (e) (ellipsis? e r mod))))
          (lambda (p pvars)
            (cond
             ((not (and-map (lambda (x) (not (ellipsis? (car x) r mod))) pvars))
              (syntax-violation 'syntax-case "misplaced ellipsis" pat))
             ((not (distinct-bound-ids? (map car pvars)))
              (syntax-violation 'syntax-case "duplicate pattern variable" pat))
             (else
              (let ((y (gen-var 'tmp)))
                ;; fat finger binding and references to temp variable y
                (build-call no-source
                            (build-simple-lambda no-source (list 'tmp) #f (list y) '()
                                                 (let ((y (build-lexical-reference no-source 'tmp y)))
                                                   (build-conditional no-source
                                                                      (syntax-case fender ()
                                                                        (#t y)
                                                                        (_ (build-conditional no-source
                                                                                              y
                                                                                              (build-dispatch-call pvars fender y r mod)
                                                                                              (build-data no-source #f))))
                                                                      (build-dispatch-call pvars exp y r mod)
                                                                      (gen-syntax-case x keys clauses r mod))))
                            (list (if (eq? p 'any)
                                      (build-primcall no-source 'list (list x))
                                      (build-call
                                       no-source
                                       (build-global-reference
                                        no-source
                                        '$sc-dispatch
                                        '(private hoot expander))
                                       (list x (build-data no-source p))))))))))))

      (define (gen-syntax-case x keys clauses r mod)
        (if (null? clauses)
            (build-primcall no-source 'syntax-violation
                            (list (build-data no-source #f)
                                  (build-data no-source
                                              "source expression failed to match any pattern")
                                  x))
            (syntax-case (car clauses) ()
              ((pat exp)
               (if (and (id? #'pat)
                        (and-map (lambda (x) (not (free-id=? #'pat x)))
                                 (cons #'(... ...) keys)))
                   (if (free-id=? #'pat #'_)
                       (expand #'exp r empty-wrap mod)
                       (let ((labels (list (gen-label)))
                             (var (gen-var #'pat)))
                         (build-call no-source
                                     (build-simple-lambda
                                      no-source (list (syntax->datum #'pat)) #f (list var)
                                      '()
                                      (expand #'exp
                                              (extend-env labels
                                                          (list (make-binding 'syntax `(,var . 0)))
                                                          r)
                                              (make-binding-wrap #'(pat)
                                                                 labels empty-wrap)
                                              mod))
                                     (list x))))
                   (gen-clause x keys (cdr clauses) r
                               #'pat #t #'exp mod)))
              ((pat fender exp)
               (gen-clause x keys (cdr clauses) r
                           #'pat #'fender #'exp mod))
              (_ (syntax-violation 'syntax-case "invalid clause"
                                   (car clauses))))))

      (lambda (e r w s mod)
        (let ((e (source-wrap e w s mod)))
          (syntax-case e ()
            ((_ val (key ...) m ...)
             (if (and-map (lambda (x) (and (id? x) (not (ellipsis? x r mod))))
                          #'(key ...))
                 (let ((x (gen-var 'tmp)))
                   ;; fat finger binding and references to temp variable x
                   (build-call s
                               (build-simple-lambda no-source (list 'tmp) #f (list x) '()
                                                    (gen-syntax-case (build-lexical-reference no-source 'tmp x)
                                                                     #'(key ...) #'(m ...)
                                                                     r
                                                                     mod))
                               (list (expand #'val r empty-wrap mod))))
                 (syntax-violation 'syntax-case "invalid literals list" e))))))))

  (define (install-core-syntax-definitions! module)
    (define (install! kind name value)
      (install-syntax-definition! module kind name value)
      (module-export! module name))
    (install! 'local-syntax 'letrec-syntax #t)
    (install! 'local-syntax 'let-syntax #f)
    (install! 'core 'syntax-parameterize expand-syntax-parameterize)
    (install! 'core 'quote expand-quote)
    (install! 'core 'quote-syntax expand-quote-syntax)
    (install! 'core 'syntax expand-syntax)
    (install! 'core 'lambda expand-lambda)
    (install! 'core 'lambda* expand-lambda*)
    (install! 'core 'case-lambda expand-case-lambda)
    (install! 'core 'case-lambda* expand-case-lambda*)
    (install! 'core 'with-ellipsis expand-with-ellipsis)
    (install! 'core 'let expand-let)
    (install! 'core 'letrec expand-letrec)
    (install! 'core 'letrec* expand-letrec*)
    (install! 'core 'set! expand-set!)
    (install! 'module-ref '@ expand-public-ref)
    (install! 'module-ref '@@ expand-private-ref)
    (install! 'core 'if expand-if)
    (install! 'begin 'begin '())
    (install! 'define 'define '())
    (install! 'define-syntax 'define-syntax '())
    (install! 'define-syntax-parameter 'define-syntax-parameter '())
    (install! 'eval-when 'eval-when '())
    (install! 'core 'syntax-case expand-syntax-case))

  (define (make-variable-transformer proc)
    (if (procedure? proc)
        (let ((trans (lambda (x)
                       #((macro-type . variable-transformer))
                       (proc x))))
          (set-procedure-property! trans 'variable-transformer #t)
          trans)
        (error "variable transformer not a procedure" proc)))

  (define (identifier? x)
    (nonsymbol-id? x))

  (define (generate-temporaries ls)
    (arg-check list? ls 'generate-temporaries)
    (arg-check module? (expansion-environment) 'generate-temporaries)
    (let ((mod (cons 'private (module-name (expansion-environment)))))
      (map (lambda (x)
             (wrap (gen-var 't) top-wrap mod))
           ls)))

  (define (free-identifier=? x y)
    (arg-check nonsymbol-id? x 'free-identifier=?)
    (arg-check nonsymbol-id? y 'free-identifier=?)
    (free-id=? x y))

  (define (bound-identifier=? x y)
    (arg-check nonsymbol-id? x 'bound-identifier=?)
    (arg-check nonsymbol-id? y 'bound-identifier=?)
    (bound-id=? x y))

  ;; Anywhere that would catch a syntax-violation should pull source off
  ;; the form and subform, instead of expecting it as a &source
  ;; annotation.
  (define* (syntax-local-binding id #:key (resolve-syntax-parameters? #t))
    (arg-check nonsymbol-id? id 'syntax-local-binding)
    (with-transformer-environment
     (lambda (e r w s rib mod)
       (define (strip-anti-mark w)
         (let ((ms (wrap-marks w)) (s (wrap-subst w)))
           (if (and (pair? ms) (eq? (car ms) the-anti-mark))
               ;; output is from original text
               (make-wrap (cdr ms) (if rib (cons rib (cdr s)) (cdr s)))
               ;; output introduced by macro
               (make-wrap ms (if rib (cons rib s) s)))))
       (call-with-values (lambda ()
                           (resolve-identifier
                            (syntax-expression id)
                            (strip-anti-mark (syntax-wrap id))
                            r
                            (or (syntax-module id) mod)
                            resolve-syntax-parameters?))
         (lambda (type value mod)
           (case type
             ((lexical) (values 'lexical value))
             ((macro) (values 'macro value))
             ((syntax-parameter) (values 'syntax-parameter value))
             ((syntax) (values 'pattern-variable value))
             ((displaced-lexical) (values 'displaced-lexical #f))
             ((global) (values 'global (cons value (cdr mod))))
             ((ellipsis)
              (values 'ellipsis
                      (wrap-syntax value (anti-mark (syntax-wrap value))
                                   mod)))
             (else (values 'other #f))))))))

  (define* (syntax-violation who message form #:optional subform)
    (define (false-or-string-or-symbol? x)
      (or (eq? x #f) (string? x) (symbol? x)))
    (check-type who false-or-string-or-symbol? 'syntax-violation)
    (check-type message string? 'syntax-violation)
    (raise (make-syntax-violation who message form subform)))

  ;; FIXME: Wire up a syntax-module-bindings and put in
  ;; core-syntax-helpers.
  (define (syntax-locally-bound-identifiers id)
    (arg-check nonsymbol-id? id 'syntax-locally-bound-identifiers)
    (locally-bound-identifiers (syntax-wrap id)
                               (syntax-module id)))

  (cross-compilation-case
   (#f
    (%initialize-syntax-helpers!
     #:make-variable-transformer make-variable-transformer
     #:identifier? identifier?
     #:generate-temporaries generate-temporaries
     #:free-identifier=? free-identifier=?
     #:bound-identifier=? bound-identifier=?
     #:syntax-local-binding syntax-local-binding
     #:syntax-violation syntax-violation)))

  ;; $sc-dispatch expects an expression and a pattern.  If the expression
  ;; matches the pattern a list of the matching expressions for each
  ;; "any" is returned.  Otherwise, #f is returned.  (This use of #f will
  ;; not work on r4rs implementations that violate the ieee requirement
  ;; that #f and () be distinct.)

  ;; The expression is matched with the pattern as follows:

  ;; pattern:                           matches:
  ;;   ()                                 empty list
  ;;   any                                anything
  ;;   (<pattern>1 . <pattern>2)          (<pattern>1 . <pattern>2)
  ;;   each-any                           (any*)
  ;;   #(free-id <key>)                   <key> with free-identifier=?
  ;;   #(each <pattern>)                  (<pattern>*)
  ;;   #(each+ p1 (p2_1 ... p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
  ;;   #(vector <pattern>)                (list->vector <pattern>)
  ;;   #(atom <object>)                   <object> with "equal?"

  ;; Vector cops out to pair under assumption that vectors are rare.  If
  ;; not, should convert to:
  ;;   #(vector <pattern>*)               #(<pattern>*)

  (define ($sc-dispatch e p)
    (define (match-each e p w mod)
      (cond
       ((pair? e)
        (let ((first (match (car e) p w '() mod)))
          (and first
               (let ((rest (match-each (cdr e) p w mod)))
                 (and rest (cons first rest))))))
       ((null? e) '())
       ((syntax? e)
        (match-each (syntax-expression e)
                    p
                    (join-wraps w (syntax-wrap e))
                    (or (syntax-module e) mod)))
       (else #f)))

    (define (match-each+ e x-pat y-pat z-pat w r mod)
      (let f ((e e) (w w))
        (cond
         ((pair? e)
          (call-with-values (lambda () (f (cdr e) w))
            (lambda (xr* y-pat r)
              (if r
                  (if (null? y-pat)
                      (let ((xr (match (car e) x-pat w '() mod)))
                        (if xr
                            (values (cons xr xr*) y-pat r)
                            (values #f #f #f)))
                      (values
                       '()
                       (cdr y-pat)
                       (match (car e) (car y-pat) w r mod)))
                  (values #f #f #f)))))
         ((syntax? e)
          (f (syntax-expression e)
             (join-wraps w (syntax-wrap e))))
         (else
          (values '() y-pat (match e z-pat w r mod))))))

    (define (match-each-any e w mod)
      (cond
       ((pair? e)
        (let ((l (match-each-any (cdr e) w mod)))
          (and l (cons (wrap (car e) w mod) l))))
       ((null? e) '())
       ((syntax? e)
        (match-each-any (syntax-expression e)
                        (join-wraps w (syntax-wrap e))
                        mod))
       (else #f)))

    (define (match-empty p r)
      (cond
       ((null? p) r)
       ((eq? p '_) r)
       ((eq? p 'any) (cons '() r))
       ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
       ((eq? p 'each-any) (cons '() r))
       (else
        (case (vector-ref p 0)
          ((each) (match-empty (vector-ref p 1) r))
          ((each+) (match-empty (vector-ref p 1)
                                (match-empty
                                 (reverse (vector-ref p 2))
                                 (match-empty (vector-ref p 3) r))))
          ((free-id atom) r)
          ((vector) (match-empty (vector-ref p 1) r))))))

    (define (combine r* r)
      (if (null? (car r*))
          r
          (cons (map car r*) (combine (map cdr r*) r))))

    (define (match* e p w r mod)
      (cond
       ((null? p) (and (null? e) r))
       ((pair? p)
        (and (pair? e) (match (car e) (car p) w
                              (match (cdr e) (cdr p) w r mod)
                              mod)))
       ((eq? p 'each-any)
        (let ((l (match-each-any e w mod))) (and l (cons l r))))
       (else
        (case (vector-ref p 0)
          ((each)
           (if (null? e)
               (match-empty (vector-ref p 1) r)
               (let ((l (match-each e (vector-ref p 1) w mod)))
                 (and l
                      (let collect ((l l))
                        (if (null? (car l))
                            r
                            (cons (map car l) (collect (map cdr l)))))))))
          ((each+)
           (call-with-values
               (lambda ()
                 (match-each+ e (vector-ref p 1) (vector-ref p 2) (vector-ref p 3) w r mod))
             (lambda (xr* y-pat r)
               (and r
                    (null? y-pat)
                    (if (null? xr*)
                        (match-empty (vector-ref p 1) r)
                        (combine xr* r))))))
          ((free-id) (and (id? e) (free-id=? (wrap e w mod) (vector-ref p 1)) r))
          ((atom) (and (equal? (vector-ref p 1) (strip e)) r))
          ((vector)
           (and (vector? e)
                (match (vector->list e) (vector-ref p 1) w r mod)))))))

    (define (match e p w r mod)
      (cond
       ((not r) #f)
       ((eq? p '_) r)
       ((eq? p 'any) (cons (wrap e w mod) r))
       ((syntax? e)
        (match*
         (syntax-expression e)
         p
         (join-wraps w (syntax-wrap e))
         r
         (or (syntax-module e) mod)))
       (else (match* e p w r mod))))

    (cond
     ((eq? p 'any) (list e))
     ((eq? p '_) '())
     ((syntax? e)
      (match* (syntax-expression e)
              p (syntax-wrap e) '() (syntax-module e)))
     (else (match* e p empty-wrap '() #f))))

(begin
  ;; This useless begin is here to so that the following forms indent to
  ;; column 0, which lets this file have the same indent as Guile's
  ;; ice-9/psyntax.scm, so that we can easily diff against it.
  )

(define %with-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ () e1 e2 ...)
       #'(let () e1 e2 ...))
      ((_ ((out in)) e1 e2 ...)
       #'(syntax-case in ()
           (out (let () e1 e2 ...))))
      ((_ ((out in) ...) e1 e2 ...)
       #'(syntax-case (list in ...) ()
           ((out ...) (let () e1 e2 ...)))))))

(define %syntax-error
  (lambda (x)
    (syntax-case x ()
      ;; Extended internal syntax which provides the original form
      ;; as the first operand, for improved error reporting.
      ((_ (keyword . operands) message arg ...)
       (string? (syntax->datum #'message))
       (syntax-violation (syntax->datum #'keyword)
                         (string-join (cons (syntax->datum #'message)
                                            (map (lambda (x)
                                                   (object->string
                                                    (syntax->datum x)))
                                                 #'(arg ...))))
                         (and (syntax->datum #'keyword)
                              #'(keyword . operands))))
      ;; Standard R7RS syntax
      ((_ message arg ...)
       (string? (syntax->datum #'message))
       #'(syntax-error (#f) message arg ...)))))

(define %syntax-rules
  (lambda (xx)
    (define (expand-clause clause)
      ;; Convert a 'syntax-rules' clause into a 'syntax-case' clause.
      (syntax-case clause (syntax-error)
        ;; If the template is a 'syntax-error' form, use the extended
        ;; internal syntax, which adds the original form as the first
        ;; operand for improved error reporting.
        (((keyword . pattern) (syntax-error message arg ...))
         (string? (syntax->datum #'message))
         #'((dummy . pattern) #'(syntax-error (dummy . pattern) message arg ...)))
        ;; Normal case
        (((keyword . pattern) template)
         #'((dummy . pattern) #'template))))
    (define (expand-syntax-rules dots keys docstrings clauses)
      (with-syntax
          (((k ...) keys)
           ((docstring ...) docstrings)
           ((((keyword . pattern) template) ...) clauses)
           ((clause ...) (map expand-clause clauses)))
        (with-syntax
            ((form #'(lambda (x)
                       docstring ...  ; optional docstring
                       #((macro-type . syntax-rules)
                         (patterns pattern ...)) ; embed patterns as procedure metadata
                       (syntax-case x (k ...)
                         clause ...))))
          (if dots
              (with-syntax ((dots dots))
                #'(with-ellipsis dots form))
              #'form))))
    (syntax-case xx ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       (expand-syntax-rules #f #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ (k ...) docstring ((keyword . pattern) template) ...)
       (string? (syntax->datum #'docstring))
       (expand-syntax-rules #f #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) ((keyword . pattern) template) ...)
       (identifier? #'dots)
       (expand-syntax-rules #'dots #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) docstring ((keyword . pattern) template) ...)
       (and (identifier? #'dots) (string? (syntax->datum #'docstring)))
       (expand-syntax-rules #'dots #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...))))))

(define %define-syntax-rule
  (lambda (x)
    (syntax-case x ()
      ((_ (name . pattern) template)
       #'(define-syntax name
           (syntax-rules ()
             ((_ . pattern) template))))
      ((_ (name . pattern) docstring template)
       (string? (syntax->datum #'docstring))
       #'(define-syntax name
           (syntax-rules ()
             docstring
             ((_ . pattern) template)))))))

(define %let*
  (lambda (x)
    (syntax-case x ()
      ((let* ((x v) ...) e1 e2 ...)
       (and-map identifier? #'(x ...))
       (let f ((bindings #'((x v)  ...)))
         (if (null? bindings)
             #'(let () e1 e2 ...)
             (with-syntax ((body (f (cdr bindings)))
                           (binding (car bindings)))
               #'(let (binding) body))))))))

(define %quasiquote
  (let ()
    (define (quasi p lev)
      (syntax-case p (unquote quasiquote)
        ((unquote p)
         (if (zero? lev)
             #'("value" p)
             (quasicons #'("quote" unquote) (quasi #'(p) (1- lev)))))
        ((quasiquote p) (quasicons #'("quote" quasiquote) (quasi #'(p) (1+ lev))))
        ((p . q)
         (syntax-case #'p (unquote unquote-splicing)
           ((unquote p ...)
            (if (zero? lev)
                (quasilist* #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                 (quasicons #'("quote" unquote) (quasi #'(p ...) (1- lev)))
                 (quasi #'q lev))))
           ((unquote-splicing p ...)
            (if (zero? lev)
                (quasiappend #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                 (quasicons #'("quote" unquote-splicing) (quasi #'(p ...) (1- lev)))
                 (quasi #'q lev))))
           (_ (quasicons (quasi #'p lev) (quasi #'q lev)))))
        (#(x ...) (quasivector (vquasi #'(x ...) lev)))
        (p #'("quote" p))))
    (define (vquasi p lev)
      (syntax-case p ()
        ((p . q)
         (syntax-case #'p (unquote unquote-splicing)
           ((unquote p ...)
            (if (zero? lev)
                (quasilist* #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                 (quasicons #'("quote" unquote) (quasi #'(p ...) (1- lev)))
                 (vquasi #'q lev))))
           ((unquote-splicing p ...)
            (if (zero? lev)
                (quasiappend #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                 (quasicons
                  #'("quote" unquote-splicing)
                  (quasi #'(p ...) (1- lev)))
                 (vquasi #'q lev))))
           (_ (quasicons (quasi #'p lev) (vquasi #'q lev)))))
        (() #'("quote" ()))))
    (define (quasicons x y)
      (with-syntax ((x x) (y y))
        (syntax-case #'y ()
          (("quote" dy)
           (syntax-case #'x ()
             (("quote" dx) #'("quote" (dx . dy)))
             (_ (if (null? #'dy) #'("list" x) #'("list*" x y)))))
          (("list" . stuff) #'("list" x . stuff))
          (("list*" . stuff) #'("list*" x . stuff))
          (_ #'("list*" x y)))))
    (define (quasiappend x y)
      (syntax-case y ()
        (("quote" ())
         (cond
          ((null? x) #'("quote" ()))
          ((null? (cdr x)) (car x))
          (else (with-syntax (((p ...) x)) #'("append" p ...)))))
        (_
         (cond
          ((null? x) y)
          (else (with-syntax (((p ...) x) (y y)) #'("append" p ... y)))))))
    (define (quasilist* x y)
      (let f ((x x))
        (if (null? x)
            y
            (quasicons (car x) (f (cdr x))))))
    (define (quasivector x)
      (syntax-case x ()
        (("quote" (x ...)) #'("quote" #(x ...)))
        (_
         (let f ((y x) (k (lambda (ls) #`("vector" #,@ls))))
           (syntax-case y ()
             (("quote" (y ...)) (k #'(("quote" y) ...)))
             (("list" y ...) (k #'(y ...)))
             (("list*" y ... z) (f #'z (lambda (ls) (k (append #'(y ...) ls)))))
             (else #`("list->vector" #,x)))))))
    (define (emit x)
      (syntax-case x ()
        (("quote" x) #''x)
        (("list" x ...) #`(list #,@(map emit #'(x ...))))
        ;; could emit list* for 3+ arguments if implementation supports
        ;; list*
        (("list*" x ... y)
         (let f ((x* #'(x ...)))
           (if (null? x*)
               (emit #'y)
               #`(cons #,(emit (car x*)) #,(f (cdr x*))))))
        (("append" x ...) #`(append #,@(map emit #'(x ...))))
        (("vector" x ...) #`(vector #,@(map emit #'(x ...))))
        (("list->vector" x) #`(list->vector #,(emit #'x)))
        (("value" x) #'x)))
    (lambda (x)
      (syntax-case x ()
        ;; convert to intermediate language, combining introduced (but
        ;; not unquoted source) quote expressions where possible and
        ;; choosing optimal construction code otherwise, then emit
        ;; Scheme code corresponding to the intermediate language forms.
        ((_ e) (emit (quasi #'e 0))))))) 

#;
(define call-with-include-port
  (let ((syntax-dirname (lambda (stx)
                          (define src (syntax-source stx))
                          (define filename (and src (assq-ref src 'filename)))
                          (and (string? filename)
                               (dirname filename)))))
    (lambda* (filename proc #:key (dirname (syntax-dirname filename)))
      "Like @code{call-with-input-file}, except relative paths are
searched relative to the @var{dirname} instead of the current working
directory.  Also, @var{filename} can be a syntax object; in that case,
and if @var{dirname} is not specified, the @code{syntax-source} of
@var{filename} is used to obtain a base directory for relative file
names."
      (let* ((filename (syntax->datum filename))
             (p (open-input-file
                 (cond ((absolute-file-name? filename)
                        filename)
                       (dirname
                        (in-vicinity dirname filename))
                       (else
                        (error
                         "attempt to include relative file name but could not determine base dir")))))
             (enc (file-encoding p)))

        ;; Choose the input encoding deterministically.
        (set-port-encoding! p (or enc "UTF-8"))

        (call-with-values (lambda () (proc p))
          (lambda results
            (close-port p)
            (apply values results)))))))

(define %include
  (lambda (stx)
    (syntax-violation 'include
                      "include not supported yet on wasm target"
                      stx)
    #;
    (syntax-case stx ()
      ((_ filename)
       (call-with-include-port
        #'filename
        (lambda (p)
          ;; In Guile, (cons #'a #'b) is the same as #'(a . b).
          (cons #'begin
                (let lp ()
                  (let ((x (read-syntax p)))
                    (if (eof-object? x)
                        #'()
                        (cons (datum->syntax #'filename x) (lp))))))))))))

(define %include-ci
  (lambda (x)
    (syntax-violation 'include-ci
                      "include-ci not supported yet on wasm target"
                      x)))

(define %include-from-path
  (lambda (x)
    (syntax-violation 'include-from-path
                      "include-from-path not supported yet on wasm target"
                      x)
    #;
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax->datum #'filename)))
         (with-syntax ((fn (datum->syntax
                            #'filename
                            (canonicalize-path
                             (or (%search-load-path fn)
                                 (syntax-violation 'include-from-path
                                                   "file not found in path"
                                                   x #'filename))))))
           #'(include fn)))))))

(define %unquote
  (lambda (x)
    (syntax-violation 'unquote
                      "expression not valid outside of quasiquote"
                      x)))

(define %unquote-splicing
  (lambda (x)
    (syntax-violation 'unquote-splicing
                      "expression not valid outside of quasiquote"
                      x)))

(define %identifier-syntax
  (lambda (xx)
    (syntax-case xx (set!)
      ((_ e)
       #'(lambda (x)
           #((macro-type . identifier-syntax))
           (syntax-case x ()
             (id
              (identifier? #'id)
              #'e)
             ((_ x (... ...))
              #'(e x (... ...))))))
      ((_ (id exp1) ((set! var val) exp2))
       (and (identifier? #'id) (identifier? #'var))
       #'(make-variable-transformer
          (lambda (x)
            #((macro-type . variable-transformer))
            (syntax-case x (set!)
              ((set! var val) #'exp2)
              ((id x (... ...)) #'(exp1 x (... ...)))
              (id (identifier? #'id) #'exp1))))))))

(define %define*
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) b0 b1 ...)
       #'(define id (lambda* args b0 b1 ...)))
      ((_ id val) (identifier? #'id)
       #'(define id val)))))

(define %and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ;; Avoid ellipsis, which would lead to quadratic expansion time.
    ((_ x . y) (if x (and . y) #f))))

(define %or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ;; Avoid ellipsis, which would lead to quadratic expansion time.
    ((_ x . y) (let ((t x)) (if t t (or . y))))))

(define %quasisyntax
  (lambda (e)

    ;; Expand returns a list of the form
    ;;    [template[t/e, ...] (replacement ...)]
    ;; Here template[t/e ...] denotes the original template
    ;; with unquoted expressions e replaced by fresh
    ;; variables t, followed by the appropriate ellipses
    ;; if e is also spliced.
    ;; The second part of the return value is the list of
    ;; replacements, each of the form (t e) if e is just
    ;; unquoted, or ((t ...) e) if e is also spliced.
    ;; This will be the list of bindings of the resulting
    ;; with-syntax expression.

    (define (expand x level)
      (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
        ((quasisyntax e)
         (with-syntax (((k _)     x) ;; original identifier must be copied
                       ((e* reps) (expand (syntax e) (1+ level))))
           (syntax ((k e*) reps))))
        ((unsyntax e)
         (zero? level)
         (with-syntax (((t) (generate-temporaries '(t))))
           (syntax (t ((t e))))))
        (((unsyntax e ...) . r)
         (zero? level)
         (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                       ((t ...)        (generate-temporaries (syntax (e ...)))))
           (syntax ((t ... . r*)
                    ((t e) ... rep ...)))))
        (((unsyntax-splicing e ...) . r)
         (zero? level)
         (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                       ((t ...)        (generate-temporaries (syntax (e ...)))))
           (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
             (syntax ((t ... ... . r*)
                      (((t ...) e) ... rep ...))))))
        ((k . r)
         (and (not (zero? level))
              (identifier? (syntax k))
              (or (free-identifier=? (syntax k) (syntax unsyntax))
                  (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
         (with-syntax (((r* reps) (expand (syntax r) (1- level))))
           (syntax ((k . r*) reps))))
        ((h . t)
         (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                       ((t* (rep2 ...)) (expand (syntax t) level)))
           (syntax ((h* . t*)
                    (rep1 ... rep2 ...)))))
        (#(e ...)
         (with-syntax ((((e* ...) reps)
                        (expand (vector->list (syntax #(e ...))) level)))
           (syntax (#(e* ...) reps))))
        (other
         (syntax (other ())))))

    (syntax-case e ()
      ((_ template)
       (with-syntax (((template* replacements) (expand (syntax template) 0)))
         (syntax
          (with-syntax replacements (syntax template*))))))))

(define %unsyntax
  (lambda (e)
    (syntax-violation 'unsyntax "Invalid expression" e)))

(define %unsyntax-splicing
  (lambda (e)
    (syntax-violation 'unsyntax "Invalid expression" e)))

(define %when
  (syntax-rules ()
    ((_ test stmt stmt* ...)
     (if test (let () stmt stmt* ...)))))

(define %unless
  (syntax-rules ()
    ((_ test stmt stmt* ...)
     (if test (if #f #f) (let () stmt stmt* ...)))))

(define %else
  (lambda (x)
    (syntax-violation 'else "bad use of 'else' syntactic keyword" x x)))

(define %=>
  (lambda (x)
    (syntax-violation '=> "bad use of '=>' syntactic keyword" x x)))

(define %...
  (lambda (x)
    (syntax-violation '... "bad use of '...' syntactic keyword" x x)))

(define %_
  (lambda (x)
    (syntax-violation '_ "bad use of '_' syntactic keyword" x x)))

(define %cond
  (lambda (whole-expr)
    (define (fold f seed xs)
      (let loop ((xs xs) (seed seed))
        (if (null? xs) seed
            (loop (cdr xs) (f (car xs) seed)))))
    (define (reverse-map f xs)
      (fold (lambda (x seed) (cons (f x) seed))
            '() xs))
    (syntax-case whole-expr ()
      ((_ clause clauses ...)
       #`(begin
           #,@(fold (lambda (clause-builder tail)
                      (clause-builder tail))
                    #'()
                    (reverse-map
                     (lambda (clause)
                       (define* (bad-clause #:optional (msg "invalid clause"))
                         (syntax-violation 'cond msg whole-expr clause))
                       (syntax-case clause (=> else)
                         ((else e e* ...)
                          (lambda (tail)
                            (if (null? tail)
                                #'((let () e e* ...))
                                (bad-clause "else must be the last clause"))))
                         ((else . _) (bad-clause))
                         ((test => receiver)
                          (lambda (tail)
                            #`((let ((t test))
                                 (if t
                                     (receiver t)
                                     #,@tail)))))
                         ((test => receiver ...)
                          (bad-clause "wrong number of receiver expressions"))
                         ((generator guard => receiver)
                          (lambda (tail)
                            #`((call-with-values (lambda () generator)
                                 (lambda vals
                                   (if (apply guard vals)
                                       (apply receiver vals)
                                       #,@tail))))))
                         ((generator guard => receiver ...)
                          (bad-clause "wrong number of receiver expressions"))
                         ((test)
                          (lambda (tail)
                            #`((let ((t test))
                                 (if t t #,@tail)))))
                         ((test e e* ...)
                          (lambda (tail)
                            #`((if test
                                   (let () e e* ...)
                                   #,@tail))))
                         (_ (bad-clause))))
                     #'(clause clauses ...))))))))

(define %case
  (syntax-rules (else)
    ((_ (k . k*) . clauses)
     (let ((val (k . k*)))
       (case val . clauses)))
    ((_ k) (if #f #f))
    ((_ k (else e ...))
     (let () e ...))
    ((_ k ((v ...) e ...) . clauses)
     (if (memv k '(v ...))
         (let () e ...)
         (case k . clauses)))))

(define %do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
              (lambda (var ...)
                (if test
                    (begin
                      (if #f #f)
                      expr ...)
                    (begin
                      command
                      ...
                      (loop (do "step" var step ...)
                        ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))


  (begin
    ;; Hey we're back to normal indentation!
    )

  (define (install-derived-syntax-definitions! module)
    (define (install-macro! name val)
      (install-syntax-definition! module 'macro name val)
      (module-export! module name))
    (install-macro! 'with-syntax %with-syntax)
    (install-macro! 'syntax-error %syntax-error)
    (install-macro! 'syntax-rules %syntax-rules)
    (install-macro! 'define-syntax-rule %define-syntax-rule)
    (install-macro! 'let* %let*)
    (install-macro! 'quasiquote %quasiquote)
    (install-macro! 'include %include)
    (install-macro! 'include-ci %include-ci)
    (install-macro! 'include-from-path %include-from-path)
    (install-macro! 'unquote %unquote)
    (install-macro! 'unquote-splicing %unquote-splicing)
    (install-macro! 'identifier-syntax %identifier-syntax)
    (install-macro! 'define* %define*)
    (install-macro! '_ %_)
    (install-macro! '... %...)
    (install-macro! '=> %=>)
    (install-macro! 'else %else)
    (install-macro! 'and %and)
    (install-macro! 'or %or)
    (install-macro! 'quasisyntax %quasisyntax)
    (install-macro! 'unsyntax %unsyntax)
    (install-macro! 'unsyntax-splicing %unsyntax-splicing)
    (install-macro! 'cond %cond)
    (install-macro! 'when %when)
    (install-macro! 'unless %unless)
    (install-macro! 'case %case)
    (install-macro! 'do %do))
  
  ;; While we boot, our definitions get installed into the (hoot
  ;; core-syntax) module.
  (define (initialize-core-syntax! mod)
    (install-core-syntax-definitions! mod)
    (install-derived-syntax-definitions! mod)
    (values))

  ;; The portable macroexpand seeds expand-top's mode m with 'e (for
  ;; evaluating) and esew (which stands for "eval syntax expanders
  ;; when") with '(eval).  In Chez Scheme, m is set to 'c instead of e
  ;; if we are compiling a file, and esew is set to
  ;; (eval-syntactic-expanders-when), which defaults to the list
  ;; '(compile load eval).  This means that, by default, top-level
  ;; syntactic definitions are evaluated immediately after they are
  ;; expanded, and the expanded definitions are also residualized into
  ;; the object file if we are compiling a file.
  (define* (macroexpand stx env #:key
                        (mode 'e) (eval-syntax-expanders-when '(eval)))
    (arg-check syntax? stx 'macroexpand)
    (arg-check module? env 'macroexpand)
    (parameterize ((expansion-environment env))
      (expand-top-sequence (list stx) null-env top-wrap #f mode
                           eval-syntax-expanders-when
                           (cons 'private (module-name env))))))
