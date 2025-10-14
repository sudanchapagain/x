;;; WebAssembly resolver
;;; Copyright (C) 2023, 2025 Igalia, S.L.
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
;;; Lowers WASM with human readable identifiers to WASM with only
;;; index references.
;;;
;;; Code:

(define-module (wasm resolve)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (resolve-wasm))

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define (alist-sort alist)
  (sort alist (lambda (a b) (< (car a) (car b)))))

(define (make-name-store)
  (let ((count 0)
        (ids (make-hash-table)))
    (values (lambda (id)
              (let ((idx count))
                (set! count (1+ count))
                (when id (hashq-set! ids id idx))
                idx))
            (lambda (id)
              (cond
               ((exact-integer? id) id)
               ((hashq-ref ids id))
               (else (error "unbound identifier" id))))
            (lambda ()
              (alist-sort
               (hash-fold (lambda (name idx result)
                            (cons (cons idx name) result))
                          '() ids))))))

(define (make-indirect-name-store)
  (let ((table (make-hash-table)))
    (values (lambda (parent-id parent-idx id)
              (match (hashq-ref table parent-idx)
                (#f
                 (let-values (((add-id! resolve-id name-map) (make-name-store)))
                   (let ((procs (list add-id! resolve-id name-map)))
                     (hashq-set! table parent-idx procs)
                     (when parent-id
                       (hashq-set! table parent-id procs)))
                   (add-id! id)))
                ((add-id! resolve-id name-map)
                 (add-id! id))))
            (lambda (parent-id-or-idx id-or-idx)
              (if (exact-integer? id-or-idx)
                  id-or-idx
                  (match (hashq-ref table parent-id-or-idx)
                    ((add-id! resolve-id name-map)
                     (resolve-id id-or-idx)))))
            (lambda ()
              (alist-sort
               (hash-fold
                (lambda (id-or-idx procs result)
                  (if (exact-integer? id-or-idx)
                      (match procs
                        ((_ _ name-map)
                         (match (name-map)
                           ((name-map ..1) (cons (cons id-or-idx name-map) result))
                           (_ result))))
                      result))
                '()
                table))))))

(define-syntax match-inst
  (syntax-rules (_)
    ((match-inst inst ((head . tail) . body) ... (_ . default))
     (match inst
       ((op . args)
        (match op
          (head (match args (tail . body)))
          ...
          (_ . default)))))))

(define* (resolve-wasm mod #:key emit-names?)
  (define-values (add-type-id! resolve-type type-name-map) (make-name-store))
  (define-values (add-func-id! resolve-func func-name-map) (make-name-store))
  (define-values (add-table-id! resolve-table table-name-map) (make-name-store))
  (define-values (add-memory-id! resolve-memory memory-name-map) (make-name-store))
  (define-values (add-global-id! resolve-global global-name-map) (make-name-store))
  (define-values (add-elem-id! resolve-elem elem-name-map) (make-name-store))
  (define-values (add-data-id! resolve-data data-name-map) (make-name-store))
  (define-values (add-tag-id! resolve-tag tag-name-map) (make-name-store))
  (define-values (add-struct-field! resolve-struct-field struct-field-name-map)
    (make-indirect-name-store))
  (define-values (add-func-local! resolve-func-local func-local-name-map)
    (make-indirect-name-store))
  (define-values (add-func-label! resolve-func-label func-label-name-map)
    (make-indirect-name-store))
  (define (add-func-locals! func)
    (match func
      (($ <func> id ($ <type-use> _ type) locals)
       (let ((idx (resolve-func id)))
         (for-each (lambda (local-id)
                     (add-func-local! id idx local-id))
                   (append (map param-id (func-sig-params type))
                           (map local-id locals)))))))
  (define (add-func-labels! func)
    (match func
      (($ <func> id _ _ body)
       (let ((idx (resolve-func id)))
         (let loop ((insts body))
           (match insts
             (() #t)
             ((((or 'block 'loop) label _ body) . rest)
              (add-func-label! id idx label)
              (loop body)
              (loop rest))
             ((('if label _ consequent alternate) . rest)
              (add-func-label! id idx label)
              (loop consequent)
              (loop alternate)
              (loop rest))
             ((_ . rest)
              (loop rest))))))))

  (define (resolve-memarg memarg)
    (match memarg
      (($ <mem-arg> id offset align)
       (make-mem-arg (resolve-memory id) offset align))))

  (define interned-strings (make-hash-table))
  (define interned-string-count 0)
  (define (intern-string string)
    (or (hash-ref interned-strings string)
        (let ((idx interned-string-count))
          (hash-set! interned-strings string idx)
          (set! interned-string-count (1+ idx))
          idx)))

  (define functions-used-as-values (make-hash-table))
  (define (record-function-used-as-value idx)
    (unless (exact-integer? idx) (error "expected resolved idx"))
    (hashv-set! functions-used-as-values idx #t)
    idx)

  (define (type-use-matcher params results)
    (define param-type (match-lambda (($ <param> id type) type)))
    (lambda (rec type-id type-idx supers type)
      (and (null? supers)
           (match type
             (($ <func-sig> params' results')
              (and (equal? (map param-type params)
                           (map param-type params'))
                   (equal? results results')
                   (make-type-use type-idx (make-func-sig params results))))
             (_ #f)))))

  (define (adjoin-types-from-type-uses types funcs imports tags)
    (define (adjoin-type-use type types)
      (match type
        (($ <type-use> #f ($ <func-sig> params results))
         (if (find-type (type-use-matcher params results) types)
             types
             (append types
                     (list (make-type #f (make-func-sig params results))))))
        (($ <type-use>) types)))
    (define (adjoin-type-uses-from-tag-type type types)
      (match type
        (($ <tag-type> attribute type)
         (adjoin-type-use type types))))
    (define (adjoin-type-uses-from-import import types)
      (match import
        (($ <import> mod name 'func id type)
         (adjoin-type-use type types))
        (($ <import> mod name 'tag id type)
         (adjoin-type-uses-from-tag-type type types))
        (($ <import>) types)))
    (define (adjoin-type-uses-from-tag tag types)
      (match tag
        (($ <tag> id type)
         (adjoin-type-uses-from-tag-type type types))))
    (define (adjoin-type-uses-from-func func types)
      (define (adjoin-type-use-for-block-type x types)
        (match x
          (($ <type-use> #f ($ <func-sig> () (or () (_))))
           types)
          (_ (adjoin-type-use x types))))
      (define (adjoin-type-uses-for-inst inst types)
        (match-inst inst
          (((or 'block 'loop) label type body)
           (fold1 adjoin-type-uses-for-inst body
                  (adjoin-type-use-for-block-type type types)))
          (('if label type consequent alternate)
           (adjoin-type-uses-from-body
            consequent
            (adjoin-type-uses-from-body
             alternate
             (adjoin-type-use-for-block-type type types))))
          (('try label type body ((tag . catches) ...) catch-all)
           (fold1 adjoin-type-uses-from-body (cons body catches)
                  (adjoin-type-use-for-block-type
                   type
                   (if catch-all
                       (adjoin-type-uses-from-body catch-all types)
                       types))))
          (('try_delegate label type body handler)
           (adjoin-type-uses-from-body
            body
            (adjoin-type-use-for-block-type type types)))
          (((or 'call_indirect 'return_call_indirect) table type)
           (adjoin-type-use type types))
          (_ types)))
      (define (adjoin-type-uses-from-body insts types)
        (fold1 adjoin-type-uses-for-inst insts types))
      (match func
        (($ <func> id type locals body)
         (adjoin-type-uses-from-body body (adjoin-type-use type types)))))
    (fold1 adjoin-type-uses-from-func funcs
           (fold1 adjoin-type-uses-from-tag tags
                  (fold1 adjoin-type-uses-from-import imports types))))

  (match mod
    (($ <wasm> id %types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (define (generate-names)
       (make-names id
                   (func-name-map)
                   (func-local-name-map)
                   (func-label-name-map)
                   (type-name-map)
                   (table-name-map)
                   (memory-name-map)
                   (global-name-map)
                   (elem-name-map)
                   (data-name-map)
                   (struct-field-name-map)
                   (tag-name-map)))
     (define types (adjoin-types-from-type-uses %types funcs imports tags))

     (for-each (match-lambda (($ <type> id type) (add-type-id! id))
                             (($ <rec-group> (($ <type> id type) ...))
                              (for-each add-type-id! id)))
               types)
     (for-each (match-lambda (($ <import> mod name kind id type)
                              (match kind
                                ('func (add-func-id! id))
                                ('global (add-global-id! id))
                                ('table (add-table-id! id))
                                ('memory (add-memory-id! id))
                                ('tag (add-tag-id! id)))))
               imports)
     (for-each (match-lambda (($ <func> id type locals body)
                              (add-func-id! id)))
               funcs)
     (for-each (match-lambda (($ <table> id type init)
                              (add-table-id! id)))
               tables)
     (for-each (match-lambda (($ <memory> id type)
                              (add-memory-id! id)))
               memories)
     (for-each (match-lambda (($ <global> id type init)
                              (add-global-id! id)))
               globals)
     (for-each (match-lambda (($ <elem> id mode table type offset init)
                              (add-elem-id! id)))
               elems)
     (for-each (match-lambda (($ <data> id mode mem offset init)
                              (add-data-id! id)))
               datas)
     (for-each (match-lambda (($ <tag> id type)
                              (add-tag-id! id)))
               tags)
     (for-each intern-string strings)
     (find-type (lambda (rec type-id type-idx supers type)
                  (match type
                    (($ <struct-type>
                        (($ <field> field-id mutable? type) ...))
                     (for-each
                      (lambda (field-id)
                        (add-struct-field! type-id type-idx field-id))
                      field-id))
                    (_ (values)))
                  #f)
                types)
     (when emit-names?
       (for-each add-func-locals! funcs)
       (for-each add-func-labels! funcs))

     (define (type-by-idx idx)
       (or (find-type (lambda (rec type-id type-idx supers type)
                        (and (eqv? type-idx idx)
                             type))
                      types)
           (error "unknown type" idx)))

     (define (resolve-heap-type ht)
       (match ht
         ((or 'func 'extern
              'any 'eq 'i31 'noextern 'nofunc 'struct 'array 'none
              'string 'stringview_wtf8 'stringview_wtf16 'stringview_iter)
          ht)
         (_ (resolve-type ht))))

     (define (resolve-val-type vt)
       (match vt
         ((or 'i32 'i64 'f32 'f64 'v128
              'funcref 'externref 'anyref 'eqref 'i31ref
              'nullexternref 'nullfuncref
              'structref 'arrayref 'nullref
              'stringref
              'stringview_wtf8ref 'stringview_wtf16ref 'stringview_iterref)
          vt)
         (($ <ref-type> nullable? ht)
          (make-ref-type nullable? (resolve-heap-type ht)))))

     (define (resolve-ref-type rt)
       (resolve-val-type rt))

     (define (resolve-storage-type type)
       (match type
         ((or 'i8 'i16) type)
         (_ (resolve-val-type type))))

     (define (resolve-param param)
       (match param
         (($ <param> id type)
          (make-param id (resolve-val-type type)))))

     (define (resolve-type-use x)
       ;; Transform symbolic or anonymous type uses to indexed type
       ;; uses.
       (define (lookup-type-use params results)
         (or (find-type (type-use-matcher params results) types)
             (error "unreachable")))
       (match x
         (($ <type-use> idx (and use-sig ($ <func-sig> params results)))
          (if idx
              (let ((idx (resolve-type idx)))
                (let ((def-sig (type-by-idx idx)))
                  (make-type-use idx
                                 (if (and (null? params) (null? results))
                                     def-sig
                                     use-sig))))
              (match (lookup-type-use params results)
                (($ <type-use> idx ($ <func-sig> params results))
                 (let ((params (map resolve-param params))
                       (results (map resolve-val-type results)))
                   (make-type-use idx (make-func-sig params results)))))))))

     (define (resolve-type-use-as-idx x)
       (match (resolve-type-use x)
         (($ <type-use> idx func-sig)
          idx)))

     (define (resolve-block-type x)
       (match x
         (($ <type-use> #f ($ <func-sig> () ()))
          x)
         (($ <type-use> #f ($ <func-sig> () (ret)))
          (let ((ret (resolve-val-type ret)))
            (make-type-use #f (make-func-sig '() (list ret)))))
         (_ (resolve-type-use-as-idx x))))

     (define (enumerate-locals params locals)
       (define table (make-hash-table))
       (define (visit-params)
         (let lp ((params params) (idx 0))
           (match params
             (() (visit-locals idx))
             ((($ <param> id _) . params)
              (when id (hashq-set! table id idx))
              (lp params (1+ idx))))))
       (define (visit-locals first-local)
         (let lp ((locals locals) (idx first-local))
           (match locals
             (() resolve-local)
             ((($ <local> id _) . locals)
              (when id (hashq-set! table id idx))
              (lp locals (1+ idx))))))
       (define (resolve-local id)
         (match id
           ((? exact-integer?) id)
           (_
            (or (hashq-ref table id)
                (error "unbound local" id locals)))))
       (visit-params))

     (define (resolve-instructions insts resolve-local)
       (define (resolve-i32 x)
         (if (< x (ash 1 31)) x (- x (ash 1 32))))
       (define (resolve-i64 x)
         (if (< x (ash 1 63)) x (- x (ash 1 64))))
       (define (resolve* insts labels)
         (map (lambda (inst) (resolve1 inst labels)) insts))
       (define (resolve1 inst labels)
         (define (resolve-label label)
           (match label
             ((? exact-integer?) label)
             (_
              (let lp ((ls labels) (idx 0))
                (match ls
                  (() (error "unbound label" label labels))
                  ((l . ls)
                   (if (eq? l label)
                       idx
                       (lp ls (1+ idx)))))))))
         (match inst
           ((op . args)
            (match-inst inst
              (((or 'block 'loop) label type body)
               (let ((labels (cons label labels)))
                 `(,op ,label ,(resolve-block-type type)
                       ,(resolve* body labels))))
              (('if label type consequent alternate)
               (let ((labels (cons label labels)))
                 `(if ,label ,(resolve-block-type type)
                      ,(resolve* consequent labels)
                      ,(resolve* alternate labels))))
              (('try label type body catches catch-all)
               (let ((labels (cons label labels)))
                 `(try ,label ,(resolve-block-type type)
                       ,(resolve* body labels)
                       ,(map (match-lambda
                               ((tag . body)
                                (cons (resolve-tag tag)
                                      (resolve* body labels))))
                             catches)
                       ,(resolve* catch-all labels))))
              (('try_delegate label type body handler)
               (let ((labels (cons label labels)))
                 `(try_delegate ,label ,(resolve-block-type type)
                                ,(resolve* body labels)
                                ,(resolve-label handler))))
              (('throw tag) `(,op ,(resolve-tag tag)))
              (((or 'br 'br_if 'rethrow) label)
               `(,op ,(resolve-label label)))
              (('br_table targets default)
               `(br_table ,(map resolve-label targets) ,(resolve-label default)))
              (((or 'call 'return_call) label)
               `(,op ,(resolve-func label)))
              (('call_indirect table type)
               `(call_indirect ,(resolve-table table) ,(resolve-type-use-as-idx type)))
              (((or 'call_ref 'return_call_ref) type)
               `(,op ,(resolve-type type)))
              (('select . _)
               (match args
                 (() inst)
                 ((types) `(select ,(map resolve-val-type types)))))
              (((or 'local.get 'local.set 'local.tee) local)
               `(,op ,(resolve-local local)))
              (((or 'global.get 'global.set) global)
               `(,op ,(resolve-global global)))
              (((or 'table.get 'table.set) table)
               `(,op ,(resolve-table table)))
              (((or 'memory.size 'memory.grow) mem)
               `(,op ,(resolve-memory mem)))
              (((or 'i32.load 'i64.load 'f32.load 'f64.load
                    'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                    'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                    'i64.load32_s 'i64.load32_u
                    'i32.store 'i64.store 'f32.store 'f64.store
                    'i32.store8 'i32.store16
                    'i64.store8 'i64.store16 'i64.store32)
                mem)
               `(,op ,(resolve-memarg mem)))
              (('i32.const x) `(i32.const ,(resolve-i32 x)))
              (('i64.const x) `(i64.const ,(resolve-i64 x)))
              (('ref.null ht) `(ref.null ,(resolve-heap-type ht)))
              (('ref.func f) `(ref.func ,(record-function-used-as-value
                                          (resolve-func f))))

              ;; GC instructions.
              (((or 'ref.test 'ref.cast) rt)
               `(,op ,(resolve-ref-type rt)))
              (((or 'br_on_cast 'br_on_cast_fail) label rt1 rt2)
               `(,op ,(resolve-label label)
                     ,(resolve-ref-type rt1) ,(resolve-ref-type rt2)))
              (((or 'struct.get 'struct.get_s 'struct.get_u 'struct.set)
                type field)
               `(,op ,(resolve-type type) ,(resolve-struct-field type field)))
              (((or 'struct.new 'struct.new_default) type)
               `(,op ,(resolve-type type)))
              (((or 'array.get 'array.get_s 'array.get_u 'array.set) type)
               `(,op ,(resolve-type type)))
              (('array.new_fixed type len)
               `(array.new_fixed ,(resolve-type type) ,len))
              (((or 'array.new 'array.new_default) type)
               `(,op ,(resolve-type type)))
              (((or 'array.new_data 'array.init_data) type data)
               `(,op ,(resolve-type type) ,(resolve-data data)))
              (((or 'array.new_elem 'array.init_elem) type elem)
               `(,op ,(resolve-type type) ,(resolve-elem elem)))
              (('array.fill type)
               `(array.fill ,(resolve-type type)))
              (('array.copy dst src)
               `(array.copy ,(resolve-type dst) ,(resolve-type src)))

              ;; Stringref instructions.
              (('string.const (? string? str))
               `(string.const ,(intern-string str)))
              (((or 'string.new_utf8 'string.new_lossy_utf8 'string.new_wtf8
                    'string.new_wtf16
                    'string.encode_utf8 'string.encode_lossy_utf8
                    'string.encode_wtf8 'string.encode_wtf16
                    'stringview_wtf8.encode_utf8
                    'stringview_wtf8.encode_lossy_utf8
                    'stringview_wtf8.encode_wtf8
                    'stringview_wtf16.encode)
                mem)
               `(,op ,(resolve-memarg mem)))

              ;; Misc instructions.
              (('memory.init data mem)
               `(memory.init ,(resolve-data data) ,(resolve-memory mem)))
              (('data.drop data)
               `(data.drop ,(resolve-data data)))
              (('memory.copy dst src)
               `(memory.copy ,(resolve-memory dst) ,(resolve-memory src)))
              (('memory.fill mem)
               `(memory.fill ,(resolve-memory mem)))
              (('table.init table elem)
               `(table.init ,(resolve-table table) ,(resolve-elem elem)))
              (('elem.drop elem)
               `(elem.drop ,(resolve-elem elem)))
              (('table.copy dst src)
               `(table.copy ,(resolve-table dst) ,(resolve-table src)))
              (((or 'table.grow 'table.size 'table.fill) table)
               `(,op ,(resolve-table table)))

              ;; Not yet implemented: simd mem ops, atomic mem ops.
              (_ inst)))
           ((? symbol? op) (list op))))
       (resolve* insts '()))

     (define (resolve-expression insts)
       (define (no-locals id)
         (error "no locals in expression" id))
       (resolve-instructions insts no-locals))

     (define (visit-type type)
       (define (resolve-field field)
         (match field
           (($ <field> id mutable? type)
            (make-field id mutable? (resolve-storage-type type)))))
       (define (resolve-base type)
         (match type
           (($ <func-sig> params results)
            (make-func-sig (map resolve-param params)
                           (map resolve-val-type results)))
           (($ <array-type> mutable? type)
            (make-array-type mutable? (resolve-storage-type type)))
           (($ <struct-type> fields)
            (make-struct-type (map resolve-field fields)))))
       (define (resolve-sub type)
         (match type
           (($ <type> id type)
            (make-type id
                       (match type
                         (($ <sub-type> final? supers type)
                          (make-sub-type final?
                                         (map resolve-heap-type supers)
                                         (resolve-base type)))
                         (_ (resolve-base type)))))))
       (match type
         (($ <rec-group> sub-types)
          (make-rec-group (map resolve-sub sub-types)))
         (_ (resolve-sub type))))

     (define (visit-import import)
       (match import
         (($ <import> mod name 'func id type)
          (make-import mod name 'func id (resolve-type-use type)))
         (($ <import> mod name 'global id ($ <global-type> mutable? type))
          (make-import mod name 'global id
                       (make-global-type mutable? (resolve-val-type type))))
         ((and import ($ <import> mod name 'memory))
          import)
         (($ <import> mod name 'table id ($ <table-type> limits type))
          (make-import mod name 'table id
                       (make-table-type limits (resolve-val-type type))))
         (($ <import> mod name 'tag id ($ <tag-type> attribute type))
          (make-import mod name 'tag id
                       (make-tag-type attribute (resolve-type-use type))))))

     (define (visit-export export)
       (match export
         (($ <export> name 'func id)
          (make-export name 'func (resolve-func id)))
         (($ <export> name 'table id)
          (make-export name 'table (resolve-table id)))
         (($ <export> name 'memory id)
          (make-export name 'memory (resolve-memory id)))
         (($ <export> name 'global id)
          (make-export name 'global (resolve-global id)))
         (($ <export> name 'tag id)
          (make-export name 'tag (resolve-tag id)))))

     (define (strip-declarative-segments elems)
       (filter (match-lambda
                (($ <elem> id mode) (not (eq? mode 'declarative))))
               elems))
     (define (add-declarative-segment elems)
       (match (sort (hash-map->list (lambda (k v) k) functions-used-as-values)
                    <)
         (() elems)
         (funcs
          (let ((declarative (make-elem #f 'declarative #f 'funcref #f
                                        (map (lambda (func-idx)
                                               `((ref.func ,func-idx)))
                                             funcs))))
            (append elems (list declarative))))))

     (define (visit-elem elem)
       (match elem
         (($ <elem> id mode table type offset init)
          (make-elem id mode (and table (resolve-table table))
                     (resolve-val-type type)
                     (and offset (resolve-expression offset))
                     (map (lambda (init)
                            (resolve-expression init))
                          init)))))

     (define (visit-data data)
       (match data
         (($ <data> id mode mem offset init)
          (make-data id mode (and mem (resolve-memory mem))
                     (and offset (resolve-expression offset))
                     init))))

     (define (visit-start start)
       (and start (resolve-func start)))

     (define (visit-func func)
       (define (visit-local local)
         (match local
           (($ <local> id type)
            (make-local id (resolve-val-type type)))))
       (match func
         (($ <func> id type locals body)
          (match (resolve-type-use type)
            ((and type ($ <type-use> idx ($ <func-sig> params _)))
             (let ((resolve-locals (enumerate-locals params locals)))
               (make-func id type (map visit-local locals)
                          (resolve-instructions body resolve-locals))))))))

     (define (visit-table table)
       (match table
         (($ <table> id ($ <table-type> limits type) init)
          (make-table id
                      (make-table-type limits (resolve-val-type type))
                      (and init (resolve-expression init))))))

     (define (visit-memory mem) mem)

     (define (visit-global global)
       (match global
         (($ <global> id ($ <global-type> mutable? type) init)
          (make-global id
                       (make-global-type mutable? (resolve-val-type type))
                       (resolve-expression init)))))

     (define (visit-tag tag)
       (match tag
         (($ <tag> id ($ <tag-type> attribute type))
          (make-tag id (make-tag-type attribute (resolve-type-use type))))))

     (let ((types (map visit-type types))
           (imports (map visit-import imports))
           (exports (map visit-export exports))
           (%elems (map visit-elem (strip-declarative-segments elems)))
           (datas (map visit-data datas))
           (start (visit-start start))
           (funcs (map visit-func funcs))
           (tables (map visit-table tables))
           (memories (map visit-memory memories))
           (globals (map visit-global globals))
           (tags (map visit-tag tags))
           (custom (if emit-names?
                       (cons (generate-names) custom)
                       custom)))
       (define strings
         (map car
              (sort (hash-map->list cons interned-strings)
                    (match-lambda*
                     (((s1 . idx1) (s2 . idx2)) (< idx1 idx2))))))
       (define elems (add-declarative-segment %elems))
       (make-wasm #f types imports funcs tables memories globals exports start
                  elems datas tags strings custom)))))
