#!/usr/bin/env guile
!#

(use-modules (ice-9 textual-ports)
             (ice-9 match)
             (ice-9 format)
             ((srfi srfi-1) #:select (append-map partition))
             (srfi srfi-9)
             (web uri))

;; decl := edge | node | attr-decl | graph
(define-record-type <edge>
  (make-edge src dst attrs)
  edge?
  (src edge-src)
  (dst edge-dst)
  (attrs edge-attrs))

(define-record-type <node>
  (make-node id attrs)
  node?
  (id node-id)
  (attrs node-attrs))

(define-record-type <attr-decl>
  (make-attr-decl kind attrs)
  attr-decl?
  (kind attr-decl-kind) ; 'node 'graph or 'edge
  (attrs attr-decl-attrs))

(define-record-type <graph>
  (make-graph id decls attrs)
  graph?
  (id graph-id)
  (decls graph-decls)
  (attrs graph-attrs))

(define (compute-node-attrs name)
  `((href
     . ,(string-append "https://gitlab.com/spritely/guile-hoot/-/blob/main/lib/"
                       (string-join (map symbol->string name) "/")
                       ".scm"))
    (fontname . Valkyrie)
    (tooltip . ,(object->string name))))

(define (module->decls name imports)
  (cons (make-node name (compute-node-attrs name))
        (map (lambda (mod)
               (make-edge name mod '((headport . n)
                                     (tailport . s))))
             imports)))

(define (visit-r6rs-library name imports)
  ;; fixme: versions
  (define (import-name spec)
    (match spec
      (('only spec . _) (import-name spec))
      (('rename spec . _) (import-name spec))
      (('except spec . _) (import-name spec))
      (('prefix spec _) (import-name spec))
      (('library name) name)
      (spec spec)))
  (module->decls name (map import-name imports)))

(define (visit-guile-library name imports)
  (define (import-name spec)
    (match spec
      ((name #:select _) name)
      ((name #:hide _) name)
      ((name #:prefix _) name)
      ((name #:renamer _) name)
      (name name)))
  (module->decls name (map import-name imports)))

(define (keyword-like-symbol? x)
  (and (symbol? x)
       (string-prefix? ":" (symbol->string x))))

(define (visit-file file)
  (call-with-input-file file
    (lambda (port)
      (match (read port)
        (('library name exports ('import . specs) . body)
         (visit-r6rs-library name specs))
        (('define-module name . args)
         (let lp ((args args) (imports '()) (pure? #f))
           (match args
             (()
              (let ((imports (if pure? imports (cons '(guile) imports))))
                (visit-guile-library name imports)))
             (((? keyword-like-symbol? kw) . args)
              (lp (cons (keyword-like-symbol->keyword kw) args) imports pure?))
             ((#:pure . args) (lp args imports #t))
             ((#:no-backtrace . args) (lp args imports #t))
             ((#:use-module spec . args) (lp args (cons spec imports) pure?))
             ((#:autoload spec bindings . args) (lp args (cons spec imports) pure?))
             (((? keyword?) kwarg . args) (lp args imports pure?))
             (_ (error "unexpected define-module args" args)))))
        (expr
         (format (current-error-port) "~a: not a recognized library\n" file)
         '())))))

(define (write-graph graph)
  (define (id-repr id)
    (match id
      (#f #f)
      ((? string?) id)
      (_ (object->string id))))
  (define (write-attr attr)
    (match attr
      ((k . v) (format #t "~s=~s;" (id-repr k) (id-repr v)))))
  (define (write-attr-stmt attr)
    (write-attr attr)
    (newline))
  (define (write-attr-list attrs)
    (unless (null? attrs)
      (format #t " [")
      (for-each write-attr attrs)
      (format #t "]")))
  (define (write-endpoint ep)
    (match ep
      (($ <graph>) (write-decl ep))
      (id (format #t "~s" (id-repr id)))))
  (define (write-decl decl)
    (match decl
      (($ <node> id attrs)
       (format #t "~s" (id-repr id))
       (write-attr-list attrs)
       (format #t ";\n"))
      (($ <edge> src dst attrs)
       (write-endpoint src)
       (format #t " -> ")
       (write-endpoint dst)
       (write-attr-list attrs)
       (format #t ";\n"))
      (($ <attr-decl> kind attrs)
       (format #t "~a" kind)
       (write-attr-list attrs)
       (format #t ";\n"))
      (($ <graph> id decls attrs)
       (format #t "subgraph ~@[~s ~]{\n" (id-repr id))
       (for-each write-attr-stmt attrs)
       (for-each write-decl decls)
       (format #t "}\n"))))
  (match graph
    (($ <graph> id decls attrs)
     (format #t "strict digraph ~@[~s ~]{\n" (id-repr id))
     (for-each write-attr-stmt attrs)
     (for-each write-decl decls)
     (format #t "}\n"))))

(define (compute-graph decls)
  (define colors
    '(indianred steelblue limegreen aquamarine purple gold lightgrey hotpink))
  (define attributed-colors (make-hash-table))
  (define (get-color id)
    (or (hash-ref attributed-colors id)
        (let ((color (car colors)))
          (set! colors (cdr colors))
          (hash-set! attributed-colors id color)
          color)))
  (define (add-node-attrs id attrs)
    (match id
      ((id0 . id+)
       (append (match id+
                 ((id1) `((label . ,id1)))
                 (_ '()))
               `((color . ,(get-color id0))
                 (style . filled)
                 (shape . box))
               attrs))))
  (call-with-values (lambda () (partition node? decls))
    (lambda (nodes decls)
      (define node-defs (make-hash-table))
      (for-each (match-lambda
                  (($ <node> id attrs)
                   (hash-set! node-defs id #t)))
                nodes)
      (define synthesized-nodes '())
      (define (maybe-synthesize! id)
        (unless (hash-ref node-defs id)
          (hash-set! node-defs id #t)
          (set! synthesized-nodes
                (cons (make-node id (add-node-attrs id '()))
                      synthesized-nodes))))
      (for-each (match-lambda
                  (($ <edge> src dst attrs)
                   (maybe-synthesize! src)
                   (maybe-synthesize! dst)))
                decls)
      (make-graph
       #f
       (append synthesized-nodes
               (map (lambda (node)
                      (match node
                        (($ <node> id attrs)
                         (make-node id (add-node-attrs id attrs)))))
                    nodes)
               decls)
       '((concentrate . true)
         (nodesep . "0.02"))))))

(when (batch-mode?)
  (match (program-arguments)
    ((arg0)
     (format (current-error-port) "usage: ~a FILE...\n" arg0)
     (exit 1))
    ((arg0 . libs)
     (write-graph (compute-graph (append-map visit-file libs))))))
