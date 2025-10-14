;;; Finalization
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
;;; JavaScript FinalizationRegistry emulation using guardians.
;;;
;;; Code:

(define-module (hoot finalization)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (make-finalization-registry
            finalization-registry?
            finalization-registry-register!
            finalization-registry-unregister!
            poll-finalization-registry!))

(define-record-type <finalization-registry>
  (%make-finalization-registry proc guardian cells unregister)
  finalization-registry?
  (proc finalization-registry-proc)
  (guardian finalization-registry-guardian)
  (cells finalization-registry-cells)
  (unregister finalization-registry-unregister))

(define-record-type <finalization-cell>
  (make-finalization-cell held-value unregister-tokens registered?)
  finalization-cell?
  (held-value finalization-cell-held-value)
  (unregister-tokens finalization-cell-unregister-tokens
                     set-finalization-cell-unregister-tokens!)
  (registered? finalization-cell-registered?
               set-finalization-cell-registered?!))

(define (make-finalization-registry proc)
  (%make-finalization-registry proc
                               (make-guardian)
                               (make-hash-table)
                               (make-hash-table)))

(define* (finalization-registry-register! registry obj held-value
                                          #:optional unregister-token)
  (when (eq? obj held-value)
    (error "held value cannot be the same as target" obj held-value))
  (match registry
    (($ <finalization-registry> _ guardian cells unregister)
     (let* ((addr (object-address obj))
            (orig-cells (hashq-ref cells addr '()))
            (orig-unregister-cells (hashq-ref unregister unregister-token '())))
       (let lp ((cells* orig-cells))
         (match cells*
           ;; No existing cell for the held value, so add a new cell.
           (()
            (let ((cell (make-finalization-cell held-value
                                                (if unregister-token
                                                    (list unregister-token)
                                                    '())
                                                #t)))
              (hashv-set! cells addr (cons cell orig-cells))
              (when unregister-token
                (hashq-set! unregister unregister-token
                            (cons cell orig-unregister-cells)))
              (guardian obj)))
           (((and cell ($ <finalization-cell> val tokens)) . rest)
            ;; If there is already a cell for the held value then we
            ;; don't need to add a new one.
            (if (eq? val held-value)
                ;; Add the unregister token to the set of tokens for
                ;; this cell.
                (when (and unregister-token
                           (not (memq unregister-token tokens)))
                  (set-finalization-cell-unregister-tokens!
                   cell (cons unregister-token tokens))
                  (hashq-set! unregister unregister-token
                              (cons cell orig-unregister-cells)))
                (lp rest)))))))))

(define (finalization-registry-unregister! registry unregister-token)
  (match registry
    (($ <finalization-registry> _ _ _ unregister)
     (match (hashq-ref unregister unregister-token)
       (#f #f)
       (tokens
        (for-each (lambda (cell)
                    (set-finalization-cell-registered?! cell #f))
                  tokens)
        (hashq-remove! unregister unregister-token)
        #t)))))

(define (poll-finalization-registry! registry)
  (match registry
    (($ <finalization-registry> proc guardian cells unregister-tokens)
     (define (finalize cell)
       (define (cleanup-token token)
         (match (hashq-ref unregister-tokens token)
           (#f (values))
           (cells
            (match (delq cell cells)
              (()
               (hashq-remove! unregister-tokens token))
              (cells
               (hashq-set! unregister-tokens token cells))))))
       (match cell
         (($ <finalization-cell> held tokens registered?)
          (when registered?
            (proc held))
          (for-each cleanup-token tokens))))
     (let lp ()
       (match (guardian)
         (#f (values))
         (obj
          (let ((addr (object-address obj)))
            (for-each finalize (hashv-ref cells addr '()))
            (hashv-remove! cells addr)
            (lp))))))))
