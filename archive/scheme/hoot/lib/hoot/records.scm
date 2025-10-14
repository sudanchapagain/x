;;; Records
;;; Copyright (C) 2024, 2025 Igalia, S.L.
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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
;;; Records.
;;;
;;; Code:

(library (hoot records)
  (export define-record-type
          record-type-parents
          record?
          write-record

          <applicable-record>
          applicable-record?
          applicable-record-procedure)
  (import (only (hoot primitives)
                %struct? %make-struct %struct-vtable
                %struct-ref %struct-set! %eq? %string?
                guile:make-record-type guile:record-type-parents)
          (hoot cond-expand)
          (hoot apply)
          (hoot pairs)
          (hoot numbers)
          (hoot eq)
          (hoot inline-wasm)
          (hoot ports)
          (hoot lists)
          (hoot keywords)
          (hoot procedures)
          (hoot symbols)
          (hoot syntax)
          (hoot syntax-objects)
          (hoot values)
          (hoot vectors)
          (hoot errors)
          (hoot match)
          (hoot bitwise))

  (define-syntax-rule (%make-vtable nfields field-names printer name
                                    constructor properties parents
                                    mutable-fields compare applicable?)
    (cond-expand
     (guile-vm
      (let ()
        (define (assq-ref alist k)
          (and (pair? alist)
               (if (eq? (caar alist) k)
                   (cdar alist)
                   (assq-ref (cdr alist) k))))
        (guile:make-record-type
         name
         field-names
         (and printer
              (lambda (s p)
                (error "guile-side I/O not implemented")))
         ;; Rely on define-record-type to do lazy initialization.
         #:parent (if (vector? parents) #f parents)
         #:uid (assq-ref properties 'uid)
         #:extensible? (assq-ref properties 'extensible?)
         #:allow-duplicate-field-names? #t
         #:opaque? (assq-ref properties 'opaque?))))
     (else
      (%inline-wasm
       '(func (param $nfields (ref eq))
              (param $printer (ref eq))
              (param $name (ref eq))
              (param $constructor (ref eq))
              (param $properties (ref eq))
              (param $parents (ref eq))
              (param $mutable-fields (ref eq))
              (param $compare (ref eq))
              (param $applicable? (ref eq))
              (result (ref eq))
              (struct.new $vtable
                          (i32.const 0)
                          (global.get $root-vtable)
                          (local.get $nfields)
                          (local.get $printer)
                          (local.get $name)
                          (local.get $constructor)
                          (local.get $properties)
                          (local.get $parents)
                          (local.get $mutable-fields)
                          (local.get $compare)
                          (local.get $applicable?)))
       nfields printer name constructor properties parents mutable-fields
       compare applicable?))))
  (define (%vtable-applicable? vtable)
    (cond-expand
     ;; Unsure what to do here because Guile's record types cannot be
     ;; made applicable.  The <applicable-struct> vtable is an
     ;; entirely separate thing and vtable flags are not accessible
     ;; from Scheme.
     (guile-vm
      #f)
     (hoot
      (%inline-wasm
       '(func (param $vtable (ref $vtable)) (result (ref eq))
              (struct.get $vtable $applicable? (local.get $vtable)))
       vtable))))
  (define (%vtable-mark-applicable! vtable)
    (cond-expand
     (guile-vm
      (values))
     (hoot
      (%inline-wasm
       '(func (param $vtable (ref $vtable))
              (struct.set $vtable $applicable?
                          (local.get $vtable)
                          (ref.i31 (i32.const 17))))
       vtable))))

  (define (record-type-parents rtd)
    (cond-expand
     (guile-vm
      (guile:record-type-parents rtd))
     (else
      (match (%inline-wasm
              '(func (param $vtable (ref $vtable)) (result (ref eq))
                     (struct.get $vtable $parents (local.get $vtable)))
              rtd)
        ((? vector? parentv) parentv)
        (parent
         (let ((grandparents (record-type-parents parent)))
           (define parents
             (make-vector (1+ (vector-length grandparents)) parent))
           (vector-copy! parents 0 grandparents 0)
           (%inline-wasm
            '(func (param $vtable (ref $vtable)) (param $parentv (ref eq))
                   (struct.set $vtable $parents (local.get $vtable)
                               (local.get $parentv)))
            rtd parents)
           parents))))))
  (define-syntax define-record-type
    (lambda (stx)
      (define (acons x y z) (cons (cons x y) z))
      (define (parse-kwargs args k)
        (let lp ((args args) (kwargs '()))
          (syntax-case args ()
            ((kw val . args) (keyword? (syntax->datum #'kw))
             (lp #'args (append kwargs (list (syntax->datum #'kw) #'val))))
            (args (k #'args kwargs)))))
      (define* (parse-body id body #:key (printer #'#f) (parent #'#f) (uid #'#f)
                           (extensible? #'#f) (allow-duplicate-field-names? #'#f)
                           (opaque? #'#f))
        (define properties
          (datum->syntax
           #'nothing
           ((syntax-case extensible? ()
              (#t (lambda (props) (acons 'extensible? #t props)))
              (#f (lambda (props) props)))
            ((syntax-case opaque? ()
               (#t (lambda (props) (acons 'opaque? #t props)))
               (#f (lambda (props) props)))
             ((syntax-case uid ()
                (#f (lambda (props) props))
                (_ (%string? (syntax->datum uid))
                   (lambda (props) (acons 'uid (syntax->datum uid) props))))
              '())))))
        (define id-str (symbol->string (syntax->datum id)))
        (define-values (parent-count
                        parent-fields
                        parent-mutable-fields
                        parent-applicable?
                        parents)
          (syntax-case parent ()
            (#f (values 0 '() 0 #f #'#()))
            (_
             (let-values (((kind value) (syntax-local-binding parent)))
               (define (err reason)
                 (syntax-violation 'define-record-type reason stx parent))
               (unless (and (eq? kind 'macro)
                            (procedure-property value 'record-type?))
                 (err "expected a record type as #:parent"))
               (unless (procedure-property value 'extensible?)
                 (err "parent record type is final"))
               (when (procedure-property value 'opaque?)
                 (unless (syntax-case opaque? () (#f #f) (_ #t))
                   (err "can't make non-opaque subtype of opaque type")))
               (let ((parent-count (procedure-property value 'parent-count)))
                 (values
                  (1+ parent-count)
                  (procedure-property value 'fields)
                  (procedure-property value 'mutable-fields)
                  #`(%vtable-applicable? #,parent)
                  (if (eq? parent-count 0)
                      #`(vector #,parent)
                      ;; Lazily initialize parentv on first access;
                      ;; mentioning all of the vtables would make it
                      ;; harder for peval / dce to elide unused vtables.
                      parent)))))))
        (define (valid-constructor-args? cfields fields)
          (define (check-parent-fields cfields parent-fields)
            (cond
             ((null? parent-fields)
              (check-fields cfields fields))
             (else
              (syntax-case cfields ()
                (() #f)
                ((cfield . cfields)
                 (and (identifier? #'cfield)
                      (eq? (syntax->datum #'cfield) (car parent-fields))
                      (check-parent-fields #'cfields (cdr parent-fields))))))))
          (define (check-fields cfields fields)
            (syntax-case cfields ()
              (() (syntax-case fields () (() #t) (_ #f)))
              ((cfield . cfields)
               (syntax-case fields ()
                 ((field . fields)
                  (and (free-identifier=? #'field #'cfield)
                       (check-fields #'cfields #'fields)))
                 (_ #f)))))
          (check-parent-fields cfields parent-fields))
        (define (compute-mutable-fields setters)
          (let lp ((setters setters) (out parent-mutable-fields)
                   (i (length parent-fields)))
            (syntax-case setters ()
              (() out)
              ((() . setters) (lp #'setters out (1+ i)))
              (((_) . setters) (lp #'setters (logior out (ash 1 i)) (1+ i))))))
        (syntax-case body ()
          (((constructor cfield ...) predicate (field getter . setter) ...)
           (and (identifier? #'constructor)
                (identifier? #'predicate)
                (valid-constructor-args? #'(cfield ...) #'(field ...)))
           #`(begin
               (define (constructor cfield ...)
                 (%make-struct #,id cfield ...))
               (define-syntax #,id
                 (lambda (stx)
                   #((record-type? . #t)
                     (parent-count . #,parent-count)
                     (fields cfield ...)
                     (mutable-fields . #,(compute-mutable-fields #'(setter ...)))
                     #,@properties)
                   (syntax-case stx ()
                     (x (identifier? #'x) #'vtable))))
               ;; Note that the procedures stored in record vtables are
               ;; treated as "trusted": they do no type checks.  They
               ;; shouldn't be exposed to users because it may be that
               ;; they can apply to objects of different types but the
               ;; same shape.
               (define vtable
                 (%make-vtable
                  #,(length #'(cfield ...))
                  '(cfield ...)
                  #,(syntax-case printer ()
                      (#f
                       (syntax-case opaque? ()
                         (#t
                          #`(lambda (x port write-field)
                              (write-string "#<" port)
                              (write-string #,id-str port)
                              (write-string ">" port)))
                         (#f
                          #`(lambda (x port write-field)
                              (write-string "#<" port)
                              (write-string #,id-str port)
                              #,@(let lp ((fields (map syntax->datum
                                                       #'(cfield ...)))
                                          (i 0))
                                   (cond
                                    ((null? fields) #'())
                                    (else
                                     (let ((name (symbol->string (car fields)))
                                           (fields (cdr fields)))
                                       #`((write-string " " port)
                                          (write-field #,name (%struct-ref x #,i) port)
                                          . #,(lp fields (1+ i)))))))
                              (write-string ">" port)))))
                      (_ #`(let ((p #,printer))
                             (lambda (x port write-field) (p x port)))))
                  '#,id
                  (lambda (vtable cfield ...)
                    (%make-struct vtable cfield ...))
                  '#,properties
                  #,parents
                  #,(compute-mutable-fields #'(setter ...))
                  #,(syntax-case opaque? ()
                      (#t
                       #`(lambda (x y equal?) #f))
                      (#f
                       #`(lambda (x y equal?)
                           (and . #,(let lp ((fields #'(cfield ...))
                                             (i 0))
                                      (syntax-case fields ()
                                        (() #'())
                                        ((f . fields)
                                         #`((equal? (%struct-ref x #,i)
                                                    (%struct-ref y #,i))
                                            . #,(lp #'fields (1+ i))))))))))
                  #,parent-applicable?))
               (define (predicate x)
                 (and (%struct? x)
                      #,(syntax-case extensible? ()
                          (#f #`(%eq? (%struct-vtable x) #,id))
                          (#t
                           #`(let ((rtd (%struct-vtable x)))
                               (or (%eq? rtd #,id)
                                   (let ((parents (record-type-parents rtd)))
                                     (and (< #,parent-count
                                             (vector-length parents))
                                          (%eq? (vector-ref parents #,parent-count)
                                                #,id)))))))))
               .
               #,(let lp ((accessors #'((getter . setter) ...))
                          (i (length parent-fields)))
                   (syntax-case accessors ()
                     (() #'())
                     (((get) . accessors)
                      #`((define (get x)
                           (check-type x predicate 'get)
                           (%struct-ref x #,i))
                         . #,(lp #'accessors (1+ i))))
                     (((get set!) . accessors)
                      #`((define (set! obj val)
                           (check-type obj predicate 'set!)
                           (%struct-set! obj #,i val))
                         . #,(lp #'((get) . accessors) i)))))))))
      (syntax-case stx ()
        ((_ id arg ...)
         (parse-kwargs
          #'(arg ...)
          (lambda (tail kwargs)
            (apply parse-body #'id tail kwargs)))))))

  (define (record? x)
    (%struct? x))

  (define (write-record record port write)
    (define printer-field 1)
    (define (write-field name value port)
      (write-string name port)
      (write-string ": " port)
      (write value port))
    (match (%struct-ref (%struct-vtable record) printer-field)
      (#f (write-string "#<record with no printer!>" port))
      (print (print record port write-field))))

  ;; TODO: Applicable records with setter.
  (define-record-type <applicable-record>
    #:extensible? #t
    (make-applicable-record procedure)
    applicable-record?
    (procedure applicable-record-procedure))
  (%vtable-mark-applicable! <applicable-record>))
