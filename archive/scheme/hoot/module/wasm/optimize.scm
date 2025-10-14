;;; WebAssembly assembler
;;; Copyright (C) 2023, 2024 Igalia, S.L.
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
;;; Optimizer for WebAssembly.
;;;
;;; We rely on most of the main optimizations to be done by Tree-IL
;;; (e.g. partial evaluation) and CPS (e.g. CSE, DCE) phases.  However
;;; there are some specificities about the WebAssembly target that are
;;; better dealt with on the WebAssembly level.
;;;
;;; Notably, when emitting CPS, it is most natural to emit loads and
;;; stores from named locations, for example virtual machine registers.
;;; But WebAssembly really wants to have more implicit data flow via its
;;; stack machine.  Of course, these are just two ways of encoding the
;;; same thing, but to produce small WebAssembly files with a minimal
;;; set of locals (and a minimal amount of local.get and local.set), we
;;; have this explicit low-level pass.
;;;
;;; The basic idea of this pass is to parse WebAssembly functions (and
;;; other expressions) to a sequence of *packets* that take their
;;; arguments and define their results directly from and to named
;;; locals.  This eliminates stack effects, facilitating reordering of
;;; packets.  Each packet is left with some set of read, write, and
;;; control effects; we can use these effects to determine when it is
;;; permissible to swap two adjacent packets.  The optimization comes in
;;; a pass that attempts to coalesce packets with a greedy bottom-up
;;; algorithm, reordering as necessary and possible.  Coalescing two
;;; packets eliminates defs from a predecessor and uses from a
;;; successor, reducing the total number of locals, and allowing for
;;; data to flow on the stack instead of through locals.  Finally, we
;;; lower packets back to a sequence of wasm instructions,
;;; re-introducing local.get / local.set terms and computing the set of
;;; needed locals for the function.
;;;
;;; Code:

(define-module (wasm optimize)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map fold fold-right))
  #:use-module (srfi srfi-9)
  #:use-module (wasm effects)
  #:use-module (wasm stack)
  #:use-module (wasm symbolify)
  #:use-module (wasm types)
  #:export (optimize-wasm))

(define-record-type <packet>
  (make-packet code uses defs effect)
  packet?
  (code packet-code) ; list of inst
  (uses packet-uses) ; list of local id, <func-sig> order
  (defs packet-defs) ; list of local id, <func-sig> order
  (effect packet-effect)) ; <effect>

(define (optimize-locals wasm)
  (define (symbol-list->hashq syms)
    (define table (make-hash-table))
    (for-each (lambda (name)
                (unless (symbol? name) (error "unnamed local"))
                (hashq-set! table name #t))
              syms)
    table)
  (define* (make-gensym names #:optional (stem "tmp"))
    (define counter 0)
    (define (gensym)
      (let ((sym (string->symbol (format #f "~a~a" stem counter))))
        (set! counter (1+ counter))
        (if (hashq-ref names sym)
            (gensym)
            sym)))
    gensym)

  (define (parse-packets body ctx local-types gensym)
    (define (introduce-local! val-type)
      (let ((id (gensym)))
        (hashq-set! local-types id val-type)
        id))

    (define (block-entry-packet param-types)
      (make-packet '() '() (map introduce-local! param-types) nofx))

    (define (block-exit-packet ctx stack)
      (let* ((effect (fallthrough-stack-effect ctx))
             (ctx (apply-stack-effect ctx effect)))
        (cond
         ((unreachable-ctx? ctx)
          (make-packet '() '() '() nofx))
         ((invalid-ctx? ctx)
          (error "validation error: " (invalid-ctx-reason ctx)))
         (else
          (make-packet '() (reverse stack) '() nofx)))))

    (define (visit-body body ctx stack)
      (match body
        (() (list (block-exit-packet ctx stack)))
        ((('local.tee id) . body)
         (visit-body `((local.set ,id) (local.get ,id) . body) ctx stack))
        ((inst . body)
         (call-with-values (lambda () (visit-inst inst ctx stack))
           (lambda (packet ctx stack)
             (cons packet (visit-body body ctx stack)))))))

    (define (%visit-block kind label param-types result-types body ctx)
      (let* ((entry (block-entry-packet param-types))
             (ctx (push-block ctx label kind param-types result-types))
             (stack (match (packet-defs entry)
                      (((id . type) ...) (reverse id)))))
        (cons entry (visit-body body ctx stack))))

    (define (visit-block kind label type body ctx)
      (match type
        (($ <type-use> _ ($ <func-sig> (($ <param> _ params) ...) results))
         (%visit-block kind label params results body ctx))))

    (define (visit-catch tag-id try-label try-type body ctx)
      (match (lookup-tag ctx tag-id)
        (($ <tag-type>
            _ ($ <type-use> _ ($ <func-sig> (($ <param> _ tag-params)) ())))
         (match try-type
           (($ <type-use> _ ($ <func-sig> try-params try-results))
            (%visit-block 'catch try-label tag-params try-results body
                          ctx))))))

    (define (visit-catch-all try-label try-type body ctx)
      (match try-type
        (($ <type-use> _ ($ <func-sig> try-params try-results))
         (%visit-block 'catch-all try-label '() try-results body ctx))))

    (define (visit-inst inst ctx stack)
      (define stack-effect (compute-stack-effect ctx inst))
      (define effect (compute-effect inst))
      (define params (stack-effect-params stack-effect))
      (define results (or (stack-effect-results stack-effect) '()))
      (match (apply-stack-effect ctx stack-effect)
        (($ <invalid-ctx> reason)
        (error "validation error: " reason))
        ((and ctx ($ <unreachable-ctx>))
         (values (make-packet '() '() '() nofx) ctx stack))
        (ctx*
         (define uses (reverse (list-head stack (length params))))
         (define defs (map introduce-local! results))
         (define stack* (fold cons (list-tail stack (length params)) defs))
         (define packet
           (match inst
             (('local.get id)
              (unless (null? uses) (error "unexpected" inst))
              (make-packet '() `(,id) defs effect))
             (('local.set id)
              (unless (null? defs) (error "unexpected" inst))
              (make-packet '() uses `(,id) effect))
             (_
              (define code
                (list
                 (match inst
                   (('block label type body)
                    `(block ,label ,type
                            ,(visit-block 'block label type body ctx)))
                   (('loop label type body)
                    `(loop ,label ,type
                           ,(visit-block 'loop label type body ctx)))
                   (('if label type consequent alternate)
                    `(if ,label ,type
                         ,(visit-block 'if label type consequent ctx)
                         ,(visit-block 'if label type alternate ctx)))
                   (('try label type body catches catch-all)
                    `(try ,label ,type
                          ,(visit-block 'try label type body ctx)
                          ,(map
                            (match-lambda
                              ((tag-id . body)
                               (visit-catch tag-id label type body ctx)))
                            catches)
                          ,(and catch-all
                                (visit-catch-all label type body ctx))))
                   (('try_delegate label type body handler)
                    `(try_delegate ,label ,type
                                   ,(visit-block 'try label type body ctx)
                                   ,handler))
                   (_ inst))))
              (make-packet code uses defs effect))))
         (values packet ctx* stack*))))

    (visit-body body ctx '()))

  (define (schedule-packets packets)
    ;; Not yet implemented.
    ;;
    ;; Sketch: For each packet from last to first, reorder uses, then
    ;; attempt coalesce.  To reorder uses, visit each use in the packet
    ;; in stack order.  Is var used just once?  If so, find packet that
    ;; def of that var.  Try to reorder it forwards.  If it reaches the
    ;; packet, merge packets: union the effects, cancel defs/uses at
    ;; boundary, append uses of first packet, append code.
    packets)

  (define (lower-packets packets local-types)
    (define used-locals (make-hash-table))
    (define (record-local! id)
      (hashq-set! used-locals id #t)
      id)

    (define (lower-inst inst)
      (match inst
        (('block label type body)
         `(block ,label ,type ,(lower-body body)))
        (('loop label type body)
         `(loop ,label ,type ,(lower-body body)))
        (('if label type consequent alternate)
         `(if ,label ,type
              ,(lower-body consequent)
              ,(lower-body alternate)))
        (('try label type body catches catch-all)
         `(try ,label ,type
               ,(lower-body body)
               ,(map lower-body catches)
               ,(and=> catch-all lower-body)))
        (('try_delegate label type body handler)
         `(try_delegate ,label ,type
                        ,(lower-body body)
                        ,handler))
        (inst inst)))

    (define (lower-body packets)
      (define (local.get id)
        `(local.get ,(record-local! id)))
      (define (local.set id)
        `(local.get ,(record-local! id)))
      (fold-right
       (lambda (packet out)
         (match packet
           ((($ <packet> code uses defs fx) . packets)
            (fold local.get
                  (fold-right (lambda (inst out)
                                (cons (lower-inst inst) out))
                              code
                              (fold local.set
                                    (lower-body packets)
                                    defs))
                  uses))))
       '() packets))

    (define (build-locals)
      (define locals-by-type (make-hash-table))
      (define (add-local id type)
        (hash-set! locals-by-type type
                   (cons id (hash-ref locals-by-type type '()))))
      (hash-for-each (lambda (id val)
                       (match (hashq-ref local-types id)
                         (#f
                          ;; A local.ref / local.set to a param.
                          #f)
                         (type
                          (add-local id type))))
                     used-locals)
    
      (define (symbol<? a b)
        (string<? (symbol->string a) (symbol->string b)))
      (define (type<? t1 t2)
        (cond
         ((symbol? t1)
          (if (symbol? t2)
              (symbol<? t1 t2)
              #t))
         ((symbol? t2) #f)
         (else
          (match t1
            (($ <ref-type> nullable?1 ht1)
             (match t2
               (($ <ref-type> nullable?2 ht2)
                (if (eq? ht1 ht2)
                    nullable?2
                    (symbol<? ht1 ht2)))))))))
      (append-map
       cdr
       (sort
        (hash-map->list
         (lambda (type ids)
           (cons type
                 (map (lambda (id)
                        (make-local id type))
                      (sort ids symbol<?))))
         locals-by-type)
        (lambda (a b)
          (match a ((t1 . _) (match b ((t2 . _) (type<? t1 t2)))))))))

    (let* ((code (lower-body packets))
           (locals (build-locals)))
      (values locals code)))

  (define (optimize-func port func)
    (match func
      (($ <func> id type (($ <local> lid ltype) ...) body)
       (define gensym
         (make-gensym (symbol-list->hashq lid)))
       (define param-ids
         (map param-id (func-sig-params (type-use-sig type))))
       (define local-types (make-hash-table))
       (for-each (lambda (id type)
                   (hashq-set! local-types id type))
                 lid ltype)
       (call-with-values (lambda ()
                           (lower-packets
                            (schedule-packets
                             (parse-packets body
                                            (initial-ctx wasm func)
                                            local-types gensym))
                            local-types))
         (lambda (locals body)
           (make-func id type locals body))))))

  (match wasm
    (($ <wasm> id types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (let ((funcs (map optimize-func funcs)))
       (make-wasm id types imports funcs tables memories globals exports start
                  elems datas tags strings custom)))))

(define (optimize-wasm wasm)
  (optimize-locals (symbolify-wasm wasm)))
