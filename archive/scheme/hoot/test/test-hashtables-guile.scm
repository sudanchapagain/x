;;; Copyright (C) 2025 Igalia, S.L.
;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;; Copyright (C) 2023, 2024 Robin Templeton
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
;;; Hashtable tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-hashtables-guile")

;; Guile legacy API
(with-imports ((guile))
  (test-call "42"
             (lambda ()
               (let ((table (make-hash-table)))
                 (hashq-set! table 'foo 42)
                 (hashq-ref table 'foo))))

  (test-call "#f"
             (lambda ()
               (let ((table (make-hash-table)))
                 (hash-set! table "foo" 42)
                 (hash-remove! table "foo")
                 (hash-ref table "foo"))))

  (test-call "42"
             (lambda ()
               (let ((table (make-weak-key-hash-table)))
                 (hashq-set! table 'foo 42)
                 (hashq-ref table 'foo))))

  (test-call "((baz . 3) (bar . 2) (foo . 1))"
             (lambda ()
               (let ((table (make-hash-table)))
                 (hashq-set! table 'foo 1)
                 (hashq-set! table 'bar 2)
                 (hashq-set! table 'baz 3)
                 (hash-map->list cons table))))

  (test-call "3"
             (lambda ()
               (let ((table (make-hash-table)))
                 (hash-set! table "foo" 1)
                 (hash-set! table "bar" 2)
                 (hash-set! table "baz" 3)
                 (hash-count (lambda (key val) #t) table))))

  ;; clear, fold, and for-each on an empty table should no-op because
  ;; we don't yet know the concrete table type.
  (test-call "#t"
             (lambda ()
               (let ((table (make-hash-table)))
                 (hash-clear! table)
                 #t)))
  (test-call "0"
             (lambda ()
               (let ((table (make-hash-table)))
                 (hash-fold (lambda (key val sum)
                              (+ sum val))
                            0 table))))
  (test-call "0"
             (lambda ()
               (let ((count 0)
                     (table (make-hash-table)))
                 (hash-for-each (lambda (key val)
                                  (set! count (1+ count)))
                                table)
                 count))))

(test-end* "test-hashtables-guile")
