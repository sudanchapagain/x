;;; WebAssembly linker
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
;;; Linker for WebAssembly, to augment a wasm module by pulling in
;;; missing definitions from a standard library.
;;;
;;; Code:

(define-module (wasm link)
  #:use-module (ice-9 match)
  #:use-module (wasm types)
  #:use-module (wasm types)
  #:export (add-stdlib))

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define-syntax match-inst
  (syntax-rules (_)
    ((match-inst inst ((head . tail) . body) ... (_ . default))
     (match inst
       ((op . args)
        (match op
          (head (match args (tail . body)))
          ...
          (_ . default)))))))

(define (sort-types types)
  (define visited (make-hash-table))
  (define (visited? type) (hashq-ref visited type))
  (define (mark-visited! type) (hashq-set! visited type #t))
  (define all-types (make-hash-table))
  (define (add-type! name type) (hashq-set! all-types name type))
  (for-each (lambda (type)
              (match type
                (($ <type> id _)
                 (add-type! id type))
                (($ <rec-group> (($ <type> id) ...))
                 (for-each (lambda (id) (add-type! id type)) id))))
            types)
  (define (lookup-type name) (hashq-ref all-types name))
  (define (visit-heap-type type order)
    (match (lookup-type type)
      (#f order)
      (type (visit-type type order))))
  (define (visit-val-type type order)
    (match type
      (($ <ref-type> nullable? ht)
       (visit-heap-type ht order))
      (_ order)))
  (define (visit-storage-type type order)
    (visit-val-type type order))
  (define (visit-successors type order)
    (define (visit-base type order)
      (match type
        (($ <array-type> mutable? type)
         (visit-storage-type type order))
        (($ <struct-type> fields)
         (fold1 (lambda (field order)
                  (match field
                    (($ <field> id mutable? type)
                     (visit-storage-type type order))))
                fields order))
        (($ <func-sig> params results)
         (fold1 (lambda (param order)
                  (match param
                    (($ <param> id type)
                     (visit-val-type type order))))
                params (fold1 visit-val-type results order)))))
    (define (visit-sub type order)
      (match type
        (($ <sub-type> final? supers type)
         (visit-base type (fold1 visit-heap-type supers order)))
        (_ (visit-base type order))))
    (match type
      (($ <rec-group> (($ <type> id type) ...))
       (fold1 visit-sub type order))
      (($ <type> id type)
       (visit-sub type order))))
  (define (visit-type type order)
    (cond
     ((visited? type) order)
     (else
      ;; After visiting successors, add label to the reverse post-order.
      (mark-visited! type)
      (cons type (visit-successors type order)))))
  (reverse (fold1 visit-type types '())))

(define* (link wasm #:key
               (link-type (lambda (id) #f))
               (link-import (lambda (id kind) #f))
               (link-func (lambda (id) #f))
               (link-table (lambda (id) #f))
               (link-memory (lambda (id) #f))
               (link-global (lambda (id) #f))
               (link-data (lambda (id) #f))
               (link-elem (lambda (id) #f))
               (link-tag (lambda (id) #f)))
  (define (for-each-instruction f body)
    (define (visit* body)
      (for-each visit1 body))
    (define (visit1 inst)
      (f inst)
      (match-inst inst
        (((or 'block 'loop) label type insts)
         (visit* insts))
        (('if label type consequent alternate)
         (visit* consequent)
         (visit* alternate))
        (('try label type body ((tag . catch) ...) catch-all)
         (visit* body)
         (for-each visit* catch)
         (when catch-all (visit* catch-all)))
        (('try_delegate label type body handler)
         (visit* body))
        (_ (values))))
    (visit* body))

  (match wasm
    (($ <wasm>
        %id %types %imports %funcs %tables %memories %globals
        %exports %start %elems %datas %tags %strings %custom)

     (define (visit-heap-type! type)
       (match type
         ((or 'func 'extern 'any 'eq 'i31 'noextern 'nofunc 'struct 'array 'none
              'string 'stringview_wtf8 'stringview_wtf16 'stringview_iter)
          (values))
         (_
          (link-type! type))))
     (define (visit-val-type! type)
       (match type
         ((or 'i32 'i64 'f32 'f64 'v128
              'funcref 'externref 'anyref 'eqref 'i31ref
              'nullexternref 'nullfuncref
              'structref 'arrayref
              'nullref
              'stringref
              'stringview_wtf8ref 'stringview_wtf16ref 'stringview_iterref)
          (values))
         (($ <ref-type> nullable? ht)
          (visit-heap-type! ht))))
     (define (visit-storage-type! type)
       (match type
         ((or 'i8 'i16) (values))
         (_ (visit-val-type! type))))
     (define (visit-func-sig! params results)
       (for-each (match-lambda
                   (($ <param> id type)
                    (visit-val-type! type)))
                 params)
       (for-each visit-val-type! results))
     (define (visit-ref-type! type)
       (match type
         (($ <ref-type> nullable? ht)
          (visit-heap-type! ht))
         (_ (values))))
     (define (visit-func-type! type)
       (visit-heap-type! type))
     (define (visit-type-use! type)
       (match type
         (($ <type-use> idx ($ <func-sig> params results))
          (visit-func-sig! params results)
          (when (symbol? idx)
            (visit-func-type! idx)))))
     (define (visit-type! type)
       (define (visit-base! type)
         (match type
           (($ <array-type> mutable? type)
            (visit-storage-type! type))
           (($ <struct-type> fields)
            (for-each (lambda (field)
                        (match field
                          (($ <field> id mutable? type)
                           (visit-storage-type! type))))
                      fields))
           (($ <func-sig> params results)
            (visit-func-sig! params results))))
       (define (visit-sub! type)
         (match type
           (($ <sub-type> final? supers type)
            (visit-base! type)
            (for-each visit-heap-type! supers))
           (_ (visit-base! type))))
       (match type
         (($ <rec-group> (($ <type> id type) ...))
          (for-each visit-sub! type))
         (($ <type> id type)
          (visit-sub! type))))
     (define (visit-import! import)
       (match import
         (($ <import> mod name 'func id type)
          (visit-type-use! type))
         (($ <import> mod name 'table id ($ <table-type> limits type))
          (visit-val-type! type))
         (($ <import> mod name 'memory id type)
          (values))
         (($ <import> mod name 'global id ($ <global-type> mutable? type))
          (visit-val-type! type))
         (($ <import> mod name 'tag id ($ <tag-type> attribute type))
          (visit-type-use! type))))
     (define (visit-body! body)
       (for-each-instruction
        (lambda (inst)
          (match-inst inst
            (((or 'block 'loop 'if 'try_delegate) label type . _)
             (when type
               (visit-type-use! type)))
            (('try label type body ((tags . _) ...) _)
             (when type
               (visit-type-use! type))
             (for-each link-tag! tags))
            (('throw tag)
             (link-tag! tag))
            (((or 'call 'return_call 'ref.func) label)
             (link-func! label))
            (((or 'call_indirect 'return_call_indirect) table type)
             (link-table! table)
             (visit-type-use! type))
            (((or 'call_ref 'return_call_ref) type)
             (visit-heap-type! type))
            (((or 'global.get 'global.set) label)
             (link-global! label))
            (((or 'table.get 'table.set
                  'table.grow 'table.size 'table.fill) label)
             (link-table! label))
            (('table.init elem table)
             (link-elem! elem)
             (link-table! table))
            (('table.copy dst src)
             (link-table! dst)
             (link-table! src))
            (((or 'i32.load 'i64.load 'f32.load 'f64.load
                  'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                  'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                  'i64.load32_s 'i64.load32_u
                  'i32.store 'i64.store 'f32.store 'f64.store
                  'i32.store8 'i32.store16 'i64.store8 'i64.store16
                  'i64.store32)
              ($ <mem-arg> id offset align))
             (link-memory! id))
            (((or 'memory.size 'memory.grow 'memory.init 'memory.fill) id)
             (link-memory! id))
            (('memory.copy dst src)
             (link-memory! dst)
             (link-memory! src))
            (('select type ...)
             (for-each visit-val-type! type))
            (('ref.null type)
             (visit-heap-type! type))
            (((or 'ref.test 'ref.cast) ($ <ref-type> nullable? type))
             (visit-heap-type! type))
            (((or 'br_on_cast 'br_on_cast_fail) idx rt1 rt2)
             (visit-ref-type! rt1)
             (visit-ref-type! rt2))
            (((or 'struct.get 'struct.get_s 'struct.get_u
                  'struct.set) type field)
             (visit-heap-type! type))
            (((or 'struct.new 'struct.new_default
                  'array.new 'array.new_default
                  'array.get 'array.get_s 'array.get_u
                  'array.set) type)
             (visit-heap-type! type))
            (('array.copy dst src)
             (visit-heap-type! dst)
             (visit-heap-type! src))
            (('array.new_fixed type count)
             (visit-heap-type! type))
            (((or 'array.new_data 'array.init_data) type data)
             (visit-heap-type! type)
             (link-data! data))
            (((or 'array.new_elem 'array.init_elem) type elem)
             (visit-heap-type! type)
             (link-elem! elem))
            (_ (values))))
        body))
     (define (visit-func! func)
       (match func
         (($ <func> id type (($ <local> lid ltype) ...) body)
          (visit-type-use! type)
          (for-each visit-val-type! ltype)
          (visit-body! body))))
     (define (visit-table! table)
       (match table
         (($ <table> id ($ <table-type> limits type) init)
          (visit-val-type! type)
          (when init
            (visit-body! init)))))
     (define (visit-memory! memory)
       ;; Nothing to do.
       (values))
     (define (visit-global! global)
       (match global
         (($ <global> id ($ <global-type> mutable? type) init)
          (visit-val-type! type)
          (visit-body! init))))
     (define (visit-export! export)
       (match export
         (($ <export> name kind id)
          (match kind
            ('func (link-func! id))
            ('table (link-table! id))
            ('global (link-global! id))
            ('memory (link-memory! id))
            ('tag (link-tag! id))))))
     (define (visit-start! start)
       (link-func! start))
     (define (visit-elem! elem)
       (match elem
         (($ <elem> id mode table type offset inits)
          (visit-body! inits)
          (visit-val-type! type)
          (when offset
            (visit-body! offset)))))
     (define (visit-data! data)
       (match data
        (($ <data> id mode mem offset init)
         (when (eq? mode 'active)
           (link-memory! mem)))))
     (define (visit-tag! tag)
       (match tag
         (($ <tag> id ($ <tag-type> attribute type))
          (visit-type-use! type))))

     (define-syntax-rule (define-linker (link! record!)
                           %elts elt-id link-elt import-kind visit-elt!)
       (begin
         (define table (make-hash-table))
         (define (record! id)
           (hashq-set! table id #t))
         (define (record-elt! elt)
           (record! (elt-id elt)))
         (define (link! id)
           (unless (hashq-ref table id)
             (match (link-elt id)
               (#f
                (if import-kind
                    (link-import! id import-kind)
                    (error "dangling reference" id)))
               (elt
                (unless (eq? id (elt-id elt)) (error "what"))
                (when (hashq-ref table id) (error "unexpected!"))
                (record! id)
                (set! %elts (cons elt %elts))
                (visit-elt! elt)))))
         (for-each record-elt! %elts)))

     (define %types-by-id (make-hash-table))
     (define (link-type! type)
       (unless (hashq-ref %types-by-id type)
         (let ((type (or (link-type type)
                         (error "unknown heap type" type))))
           (record-type! type)
           (set! %types (cons type %types))
           (visit-type! type))))
     (define (record-type! type)
       (define (record! id)
         (hashq-set! %types-by-id id type))
       (match type
         (($ <rec-group> (($ <type> id _) ...)) (for-each record! id))
         (($ <type> id _) (record! id))))

     (define (link-import! id kind)
       (let ((import (link-import id kind)))
         (unless import
           (error "dangling reference" id kind))
         (set! %imports (cons import %imports))
         (record-import! import)
         (visit-import! import)))
     (define (record-import! import)
       (match import
         (($ <import> mod name 'func id) (record-func! id))
         (($ <import> mod name 'table id) (record-table! id))
         (($ <import> mod name 'global id) (record-global! id))
         (($ <import> mod name 'memory id) (record-memory! id))
         (($ <import> mod name 'tag id) (record-tag! id))))

     (define-linker (link-func! record-func!)
       %funcs func-id link-func 'func visit-func!)
     (define-linker (link-table! record-table!)
       %tables table-id link-table 'table visit-table!)
     (define-linker (link-memory! record-memory!)
       %memories memory-id link-memory 'memory visit-memory!)
     (define-linker (link-global! record-global!)
       %globals global-id link-global 'global visit-global!)
     (define-linker (link-data! record-data!)
       %datas data-id link-data #f visit-data!)
     (define-linker (link-elem! record-elem!)
       %elems elem-id link-elem #f visit-elem!)
     (define-linker (link-tag! record-tag!)
       %tags tag-id link-tag 'tag visit-tag!)

     (for-each record-type! %types)
     (for-each record-import! %imports)
     (for-each visit-type! %types)
     (for-each visit-import! %imports)
     (for-each visit-func! %funcs)
     (for-each visit-table! %tables)
     (for-each visit-memory! %memories)
     (for-each visit-global! %globals)
     (for-each visit-export! %exports)
     (when %start (visit-start! %start))
     (for-each visit-elem! %elems)
     (for-each visit-data! %datas)
     (for-each visit-tag! %tags)

     (make-wasm %id (sort-types %types) %imports %funcs %tables %memories
                %globals %exports %start %elems %datas %tags %strings
                %custom))))

(define* (add-stdlib wasm stdlib #:key
                     (synthesize-type (lambda (id) #f))
                     (synthesize-import (lambda (id kind) #f)))
  (match stdlib
    (($ <wasm> std-id std-types std-imports std-funcs std-tables std-memories
        std-globals std-exports std-start std-elems std-datas std-tags
        std-strings std-custom)
     (define types (make-hash-table))
     (define imports (make-hash-table))
     (define funcs (make-hash-table))
     (define tables (make-hash-table))
     (define memories (make-hash-table))
     (define globals (make-hash-table))
     (define elems (make-hash-table))
     (define datas (make-hash-table))
     (define tags (make-hash-table))

     (for-each (match-lambda
                 ((and t ($ <type> id _)) (hashq-set! types id t))
                 ((and t ($ <rec-group> (($ <type> id) ...)))
                  (for-each (lambda (id) (hashq-set! types id t)) id)))
               std-types)
     (for-each (match-lambda
                 ((and import ($ <import> mode name kind id type))
                  (hash-set! imports (cons id kind) import)))
               std-imports)
     (for-each (match-lambda
                 ((and func ($ <func> id type locals body))
                  (hashq-set! funcs id func)))
               std-funcs)
     (for-each (match-lambda
                 ((and table ($ <table> id type init))
                  (hashq-set! tables id table)))
               std-tables)
     (for-each (match-lambda
                 ((and memory ($ <memory> id type))
                  (hashq-set! memories id memory)))
               std-memories)
     (for-each (match-lambda
                 ((and global ($ <global> id type init))
                  (hashq-set! globals id global)))
               std-globals)
     (for-each (match-lambda
                 ((and elem ($ <elem> id mode table type offset init))
                  (hashq-set! elems id elem)))
               std-elems)
     (for-each (match-lambda
                 ((and data ($ <data> id mode mem offset init))
                  (hashq-set! datas id data)))
               std-datas)
     (for-each (match-lambda
                 ((and tag ($ <tag> id type))
                  (hashq-set! tags id tag)))
               std-tags)

     (link wasm
           #:link-type (lambda (id)
                         (or (hashq-ref types id)
                             (synthesize-type id)))
           #:link-import (lambda (id kind)
                           (or (hash-ref imports (cons id kind))
                               (synthesize-import id kind)))
           #:link-func (lambda (id) (hashq-ref funcs id))
           #:link-table (lambda (id) (hashq-ref tables id))
           #:link-memory (lambda (id) (hashq-ref memories id))
           #:link-global (lambda (id) (hashq-ref globals id))
           #:link-elem (lambda (id) (hashq-ref elems id))
           #:link-data (lambda (id) (hashq-ref datas id))
           #:link-tag (lambda (id) (hashq-ref tags id))))))
