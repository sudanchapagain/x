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

(test-begin "test-hash")

(with-additional-imports ((hoot hashtables)
                          (only (hoot numbers) most-positive-fixnum))
  ;; Hashing numbers
  (test-call "6" hashq 42 37)

  ;; Hashing pairs and lists.
  (test-call "4" hash '(a . b) 389)
  (test-call "216" hash '(a b) 389)
  ;; Deeply nested list.
  (test-call "261" hash '(a (b (c (d (e (f (g (h (i))))))))) 389)
  ;; Due to the effort limit, long lists with equal heads hash to the
  ;; same value.
  (test-call "#t"
             (lambda ()
               (let ((a '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
                            20 21 22 23 24 25 26 27 28 29 30 31))
                     (b '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
                            20 21 22 23 24 25 26 27 28 29 30 31 32 33 34)))
                 (= (hash a most-positive-fixnum)
                    (hash b most-positive-fixnum)))))
  ;; Circular list!
  (test-call "261" hash
             (let ((x (list 'a 'b 'c)))
               (set-cdr! (cdr (cdr x)) x)
               x)
             389)

  ;; Hash composition should not be commutative.
  (test-call "#f"
             (lambda ()
               (= (hash '(a . b) most-positive-fixnum)
                  (hash '(b . a) most-positive-fixnum))))

  ;; Hashing vectors of different length.
  (test-call "161"  hash #() 389)
  (test-call "93" hash #(1 2 3) 389)

  ;; Hashing bytevectors of different length.
  (test-call "150"  hash #vu8() 389)
  (test-call "268" hash #vu8(1) 389)
  (test-call "193" hash #vu8(1 2) 389)
  (test-call "145" hash #vu8(1 2 3) 389)
  (test-call "276"  hash #vu8(1 2 3 4) 389)

  ;; Hashing bitvectors of different length.
  (test-call "114" hash #* 389)
  (test-call "207" hash #*1010 389)
  (test-call "350" hash #*01010 389)

  ;; Empty bytevector should have different hash than empty bitvector.
  (test-call "#f" (lambda () (= (hash #vu8() 389) (hash #* 389))))

  ;; Hashing records.
  (test-call "303" hash
             (let ()
               (define-record-type q (make-q a) q? (a q-a))
               (make-q 42))
             389))

(test-end* "test-hash")
