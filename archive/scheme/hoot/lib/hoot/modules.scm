;;; Modules
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
;;; Run-time representation of module trees.
;;;
;;; Code:

(library (hoot modules)
  (export module?
          the-root-module
          make-empty-module
          module-name
          module-export!
          module-import!
          module-exported-names
          module-variable
          module-local-variable
          module-bound?
          module-ref
          module-set!
          module-add!
          module-define!
          submodule-ref
          submodule-define!
          resolve-module
          current-module)
  (import (hoot cond-expand)
          (hoot eq)
          (hoot errors)
          (hoot exceptions)
          (hoot hashtables)
          (hoot lists)
          (only (hoot numbers) exact-integer? negative?)
          (hoot not)
          (hoot pairs)
          (hoot parameters)
          (hoot records)
          (hoot symbols)
          (hoot syntax)
          (hoot syntax-objects)
          (hoot values)
          (hoot vectors)
          (ice-9 match)
          (only (hoot primitives)
                guile:syntax-module-bindings))

  (define-exception-type &binding-error &violation
    make-binding-error binding-error?
    (module binding-error-module)
    (name binding-error-name))
  (define-exception-type &undefined-variable &binding-error
    make-undefined-variable-error undefined-variable-error?)
  (define-exception-type &duplicate-definition &binding-error
    make-duplicate-definition-error duplicate-definition-error?)
  (define-exception-type &export-exists &binding-error
    make-export-exists-error export-exists-error?)
  (define-exception-type &immutable-import &binding-error
    make-immutable-import-error immutable-import-error?)

  (define-record-type <module>
    (%make-module name exports defs submodules state root)
    module?
    ;; list of name-component
    (name module-name)
    ;; public-name -> private-name | var
    (exports module-exports)
    ;; name -> #(var original-mod original-name)
    (defs module-defs)
    ;; name-component -> module
    (submodules module-submodules)
    ;; A module can be in four states, represented by the following
    ;; symbols:
    ;;
    ;;  detached: The module is not referenced as a submodule of any
    ;;    parent module.  Any code that has a reference to the module
    ;;    object can add new definitions.
    ;;
    ;;  attached: The module is a submodule, but isn't yet open for
    ;;    definitions.
    ;;
    ;;  open: The module is not only attached, but it has formally been
    ;;    created and is accumulating definitions.
    ;;
    ;;  closed: The module is attached, complete, and does not accept
    ;;    any more definitions.
    (state module-state set-module-state!)
    ;; #f | module
    (root module-root))

  (define the-root-module (make-parameter #f))

  (define* (make-module #:key
                        (name '())
                        (exports (make-eq-hashtable))
                        (defs (make-eq-hashtable))
                        (submodules (make-eq-hashtable))
                        (state 'detached)
                        (root #f))
    (%make-module name exports defs submodules state root))

  (define (name-component? x)
    (or (symbol? x)
        (and (exact-integer? x) (not (negative? x)))))
  (define (valid-module-name? name)
    (match name
      (((? name-component?) ...) name)
      (_ #f)))

  (define* (make-empty-module #:key (name '()) (root #f))
    (check-type name valid-module-name? 'make-empty-module)
    (when root (check-type root module? 'make-empty-module))
    (make-module #:name name #:root root))

  (define (module-exported-names mod)
    (check-type mod module? 'module-exported-names)
    (hashtable-keys (module-exports mod)))

  (define (module-local-variable mod name)
    (check-type mod module? 'module-local-variable)
    (check-type name symbol? 'module-local-variable)
    (match (hashtable-ref (module-defs mod) name)
      (#(var original-module original-name)
       (and (eq? mod original-module)
            var))
      (#f #f)))

  (define* (module-variable mod name #:key
                            (private? #f)
                            (found values)
                            (not-found
                             (lambda ()
                               (raise
                                (make-undefined-variable-error mod name)))))
    (if private?
        (match (hashtable-ref (module-defs mod) name)
          (#(var original-module original-name)
           (found var original-module original-name))
          (#f
           (not-found)))
        (match (hashtable-ref (module-exports mod) name)
          (#f (not-found))
          (private (module-variable mod private
                                    #:private? #t
                                    #:found found
                                    #:not-found not-found)))))

  (define* (module-bound? mod name #:key (private? #f))
    (module-variable mod name
                     #:private? private?
                     #:found (lambda (var mod name) #t)
                     #:not-found (lambda () #f)))

  (define (module-attached? mod)
    (match (module-state mod)
      ('detached #f)
      ((or 'attached 'open 'closed) #t)))

  (define (module-accepts-definitions? mod)
    (match (module-state mod)
      ((or 'detached 'open) #t)
      ((or 'attached 'closed) #f)))

  (define* (module-export! mod name #:optional (src-name name))
    (check-type mod module? 'module-export!)
    (check-type mod module-accepts-definitions? 'module-export!)
    (check-type name symbol? 'module-export!)
    (check-type src-name symbol? 'module-export!)
    (when (hashtable-ref (module-exports mod) name)
      (raise (make-export-exists-error mod name)))
    ;; Lazily resolve the var, so as to allow for forward declaration of
    ;; exports.
    (hashtable-set! (module-exports mod) name src-name)
    (values))

  (define (resolve-export mod name)
    (match (hashtable-ref (module-exports mod) name)
      (#f (raise (make-undefined-variable-error mod name)))
      ((? symbol? private-name)
       (let ((v (hashtable-ref (module-defs mod) private-name)))
         (unless v
           (raise (make-undefined-variable-error mod name)))
         v))
      (v v)))

  (define* (module-import! dst src name #:optional (src-name name))
    ;; Does not check that the module graph is acyclic.
    (check-type dst module? 'module-import!)
    (check-type dst module-accepts-definitions? 'module-import!)
    (check-type src module? 'module-import!)
    (check-type name symbol? 'module-import!)
    (check-type src-name symbol? 'module-import!)
    (when (module-bound? dst name)
      (raise (make-duplicate-definition-error dst name)))
    (match (resolve-export src src-name)
      ((and v #(var original-mod original-name))
       (hashtable-set! (module-defs dst) name v)
       var)))

  (define* (module-define! mod name value #:key (allow-redefinition? #f)
                           (mutable? #t))
    (check-type mod module? 'module-define!)
    (check-type mod module-accepts-definitions? 'module-define!)
    (check-type name symbol? 'module-define!)
    (unless allow-redefinition?
      (when (module-bound? mod name)
        (raise (make-duplicate-definition-error mod name))))
    (let ((var (if mutable?
                   (case-lambda
                    (() value)
                    ((new-value) (set! value new-value)))
                   (case-lambda
                    (() value)
                    ((new-value)
                     (raise (make-immutable-import-error mod name)))))))
      (hashtable-set! (module-defs mod) name (vector var mod name))
      var))

  (define* (module-ref mod name #:key (private? #f))
    (module-variable mod name
                     #:found (lambda (var mod name) (var))
                     #:private? private?))

  (define* (module-set! mod name value #:key (mutable-imports? #f)
                        (private? #f))
    (check-type mod module? 'module-set!)
    (check-type name symbol? 'module-set!)
    (module-variable mod name
                     #:private? private?
                     #:found
                     (lambda (var original-mod original-name)
                       (unless (or (eq? mod original-mod) mutable-imports?)
                         (raise (make-immutable-import-error mod name)))
                       (var value))))

  (define (module-add! mod name var)
    ;; For use by compiler-emitted code in hackable mode.
    (hashtable-set! (module-defs mod) name (vector var mod name)))

  (define (submodule-ref mod name)
    (match name
      (()
       (match (module-state mod)
         ('attached #f)
         ((or 'detached 'open 'closed) mod)))
      ((name . name*)
       (match (hashtable-ref (module-submodules mod) name)
         (#f #f)
         (mod (submodule-ref mod name*))))))

  (define* (submodule-define! mod name #:optional
                              (root (or (module-root mod)
                                        (and (null? (module-name mod))
                                             mod))))
    (match name
      (()
       (match (module-state mod)
         ('detached mod)
         ('attached (set-module-state! mod 'open) mod)
         ((or 'open 'closed)
          (raise (make-exception
                 (make-assertion-violation)
                 (make-exception-with-message "Module already registered")
                 (make-exception-with-irritants (list mod)))))))
      ((name . name*)
       (submodule-define!
        (or (hashtable-ref (module-submodules mod) name)
            (let ((sub (make-module #:name (append (module-name mod) (list name))
                                    #:state 'attached
                                    #:root root)))
              (hashtable-set! (module-submodules mod) name sub)
              sub))
        name*
        root))))

  (define* (resolve-module mod name #:key require-bound?)
    (check-type name valid-module-name? 'resolve-module)
    (define (find-relative-tail a b)
      (match a
        (() b)
        ((a . a*)
         (match b
           ((b . b*)
            (and (eq? a b) (find-relative-tail a* b*)))
           (() #f)))))
    (let ((tail (find-relative-tail (module-name mod) name)))
      (if tail
          (submodule-ref mod tail)
          (let ((root (module-root mod)))
            (and root (submodule-ref root name))))))

  (define-syntax current-module
    (lambda (stx)
      (syntax-case stx ()
        ((current-module)
         #'(current-module current-module))
        ((_ id)
         (let ((modname (datum->syntax
                         #'id
                         (match (syntax-module #'id)
                           ((_ '% namespace . modname) modname)
                           ((_ . modname) modname)))))
           #`(let ((root (the-root-module)))
               (or
                (and root (submodule-ref root '#,modname))
                (cond-expand
                 (runtime-modules
                  (error "Unexpected error resolving module" '#,modname))
                 (else
                  (error
                   "No run-time module registry; recompile with -g")))))))))))
