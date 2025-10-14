;;; Tree-IL
;;; Copyright (C) 2024 Igalia, S.L.
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
;;; Tree-IL.
;;;
;;; Code:

(library (hoot tree-il)
  (export tree-il? tree-il-src
          <void> void? make-void
          <const> const? make-const const-exp
          <primitive-ref> primitive-ref? make-primitive-ref primitive-ref-name
          <lexical-ref> lexical-ref? make-lexical-ref lexical-ref-name lexical-ref-gensym
          <lexical-set> lexical-set? make-lexical-set lexical-set-name lexical-set-gensym lexical-set-exp
          <module-ref> module-ref? make-module-ref module-ref-mod module-ref-name module-ref-public?
          <module-set> module-set? make-module-set module-set-mod module-set-name module-set-public? module-set-exp
          <toplevel-ref> toplevel-ref? make-toplevel-ref toplevel-ref-mod toplevel-ref-name
          <toplevel-set> toplevel-set? make-toplevel-set toplevel-set-mod toplevel-set-name toplevel-set-exp
          <toplevel-define> toplevel-define? make-toplevel-define toplevel-define-mod toplevel-define-name toplevel-define-exp
          <conditional> conditional? make-conditional conditional-test conditional-consequent conditional-alternate
          <call> call? make-call call-proc call-args
          <primcall> primcall? make-primcall primcall-name primcall-args
          <seq> seq? make-seq seq-head seq-tail
          <lambda> lambda? make-lambda lambda-meta lambda-body
          <lambda-case> lambda-case? make-lambda-case
                        lambda-case-req lambda-case-opt lambda-case-rest lambda-case-kw
                        lambda-case-inits lambda-case-gensyms
                        lambda-case-body lambda-case-alternate
          <let> let? make-let let-names let-gensyms let-vals let-body
          <letrec> letrec? make-letrec letrec-in-order? letrec-names letrec-gensyms letrec-vals letrec-body
          <fix> fix? make-fix fix-names fix-gensyms fix-vals fix-body
          <let-values> let-values? make-let-values let-values-exp let-values-body
          <prompt> prompt? make-prompt prompt-escape-only? prompt-tag prompt-body prompt-handler
          <abort> abort? make-abort abort-tag abort-args abort-tail)
  (import (hoot syntax)
    (hoot errors)
    (hoot records))

  (define-record-type <tree-il>
    #:extensible? #t
    (make-tree-il src)
    tree-il?
    (src tree-il-src))

  (define-record-type <void>
    #:parent <tree-il>
    (make-void src)
    void?)

  (define-record-type <const>
    #:parent <tree-il>
    (make-const src exp)
    const?
    (exp const-exp))

  (define-record-type <primitive-ref>
    #:parent <tree-il>
    (make-primitive-ref src name)
    primitive-ref?
    (name primitive-ref-name))

  (define-record-type <lexical-ref>
    #:parent <tree-il>
    (make-lexical-ref src name gensym)
    lexical-ref?
    (name lexical-ref-name)
    (gensym lexical-ref-gensym))

  (define-record-type <lexical-set>
    #:parent <tree-il>
    (make-lexical-set src name gensym exp)
    lexical-set?
    (name lexical-set-name)
    (gensym lexical-set-gensym)
    (exp lexical-set-exp))

  (define-record-type <module-ref>
    #:parent <tree-il>
    (make-module-ref src mod name public?)
    module-ref?
    (mod module-ref-mod)
    (name module-ref-name)
    (public? module-ref-public?))

  (define-record-type <module-set>
    #:parent <tree-il>
    (make-module-set src mod name public? exp)
    module-set?
    (mod module-set-mod)
    (name module-set-name)
    (public? module-set-public?)
    (exp module-set-exp))

  (define-record-type <toplevel-ref>
    #:parent <tree-il>
    (make-toplevel-ref src mod name)
    toplevel-ref?
    (mod toplevel-ref-mod)
    (name toplevel-ref-name))

  (define-record-type <toplevel-set>
    #:parent <tree-il>
    (make-toplevel-set src mod name exp)
    toplevel-set?
    (mod toplevel-set-mod)
    (name toplevel-set-name)
    (exp toplevel-set-exp))

  (define-record-type <toplevel-define>
    #:parent <tree-il>
    (make-toplevel-define src mod name exp)
    toplevel-define?
    (mod toplevel-define-mod)
    (name toplevel-define-name)
    (exp toplevel-define-exp))

  (define-record-type <conditional>
    #:parent <tree-il>
    (make-conditional src test consequent alternate)
    conditional?
    (test conditional-test)
    (consequent conditional-consequent)
    (alternate conditional-alternate))

  (define-record-type <call>
    #:parent <tree-il>
    (make-call src proc args)
    call?
    (proc call-proc)
    (args call-args))

  (define-record-type <primcall>
    #:parent <tree-il>
    (make-primcall src name args)
    primcall?
    (name primcall-name)
    (args primcall-args))

  (define-record-type <seq>
    #:parent <tree-il>
    (make-seq src head tail)
    seq?
    (head seq-head)
    (tail seq-tail))

  (define-record-type <lambda>
    #:parent <tree-il>
    (make-lambda src meta body)
    lambda?
    (meta lambda-meta)
    (body lambda-body))

  (define-record-type <lambda-case>
    #:parent <tree-il>
    (make-lambda-case src req opt rest kw inits gensyms body alternate)
    lambda-case?
    (req lambda-case-req)
    (opt lambda-case-opt)
    (rest lambda-case-rest)
    (kw lambda-case-kw)
    (inits lambda-case-inits)
    (gensyms lambda-case-gensyms)
    (body lambda-case-body)
    (alternate lambda-case-alternate))

  (define-record-type <let>
    #:parent <tree-il>
    (make-let src names gensyms vals body)
    let?
    (names let-names)
    (gensyms let-gensyms)
    (vals let-vals)
    (body let-body))

  (define-record-type <letrec>
    #:parent <tree-il>
    (make-letrec src in-order? names gensyms vals body)
    letrec?
    (in-order? letrec-in-order?)
    (names letrec-names)
    (gensyms letrec-gensyms)
    (vals letrec-vals)
    (body letrec-body))

  (define-record-type <fix>
    #:parent <tree-il>
    (make-fix src names gensyms vals body)
    fix?
    (names fix-names)
    (gensyms fix-gensyms)
    (vals fix-vals)
    (body fix-body))

  (define-record-type <let-values>
    #:parent <tree-il>
    (make-let-values src exp body)
    let-values?
    (exp let-values-exp)
    (body let-values-body))

  (define-record-type <prompt>
    #:parent <tree-il>
    (make-prompt src escape-only? tag body handler)
    prompt?
    (escape-only? prompt-escape-only?)
    (tag prompt-tag)
    (body prompt-body)
    (handler prompt-handler))

  (define-record-type <abort>
    #:parent <tree-il>
    (make-abort src tag args tail)
    abort?
    (tag abort-tag)
    (args abort-args)
    (tail abort-tail)))
