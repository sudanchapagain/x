;;; SRFI-14: Character sets
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
;;; Character sets.
;;;
;;; Code:

(define-module (srfi srfi-14)
  #:pure
  #:use-module ((hoot errors) #:select (check-type))
  #:use-module (hoot match)
  #:use-module ((hoot lists) #:select (fold sort))
  #:use-module ((hoot numbers) #:select (1+))
  #:use-module ((hoot syntax) #:select (case-lambda define*))
  #:use-module ((hoot vectors) #:select (vector-binary-search))
  #:use-module (scheme base)
  #:export (char-set
            char-set?
            char-set-contains?
            char-set-union
            char-set->list
            char-set->string
            list->char-set
            string->char-set

            char-set:lower-case
            char-set:upper-case
            char-set:title-case
            char-set:letter
            char-set:digit
            char-set:hex-digit
            char-set:letter+digit
            char-set:graphic
            char-set:printing
            char-set:whitespace
            char-set:iso-control
            char-set:punctuation
            char-set:symbol
            char-set:blank
            char-set:ascii
            char-set:empty
            char-set:full))

;; FIXME: This is a very poor and incomplete implementation of
;; character sets.  This was written to support the bare minimum
;; needed to get Guile's (web uri) module to compile.
;;
;; What we really need is a port of Guile's srfi-14.c that uses
;; character ranges.
(define-record-type <char-set>
  (make-char-set chars)
  char-set?
  (chars char-set-chars))

(define empty-char-set (make-char-set #()))

(define* (list->char-set chars #:optional base-cs)
  (let ((chars (if base-cs
                   (append chars (char-set-chars base-cs))
                   chars)))
    (make-char-set
     (list->vector
      (let lp ((chars (sort chars char<?)) (last #f))
        (match chars
          (() '())
          ((char . rest)
           (if (eqv? char last)
               (lp rest last)
               (cons char (lp rest char))))))))))

(define (string->char-set str)
  (list->char-set (string->list str)))

(define (range->char-set start end)
  (list->char-set
   (let lp ((i start))
     (if (< i end)
         (cons (integer->char i) (lp (1+ i)))
         '()))))

(define (char-set->list cs)
  (vector->list (char-set-chars cs)))

(define (char-set->string cs)
  (list->string (char-set->list cs)))

(define (char-set . chars)
  (for-each (lambda (char)
              (check-type char char? 'char-set))
            chars)
  (list->char-set chars))

(define char-set-union
  (case-lambda
    (() empty-char-set)
    ((char-set) char-set)
    (char-sets
     (list->char-set
      (fold (lambda (char-set chars)
              (append (vector->list (char-set-chars char-set)) chars))
            '() char-sets)))))

(define (char-compare a b)
  (- (char->integer a) (char->integer b)))

(define (char-set-contains? char-set char)
  (number? (vector-binary-search (char-set-chars char-set) char char-compare)))

;;;
;;; Built-in character sets
;;;

;; FIXME: ASCII ranges only for the moment.
(define char-set:empty (char-set))
(define char-set:lower-case
  (range->char-set (char->integer #\a) (1+ (char->integer #\z))))
(define char-set:upper-case
  (range->char-set (char->integer #\A) (1+ (char->integer #\Z))))
(define char-set:title-case char-set:empty)
(define char-set:letter
  (char-set-union char-set:lower-case char-set:upper-case))
(define char-set:digit     (string->char-set "0123456789"))
(define char-set:hex-digit (string->char-set "0123456789abcdefABCDEF"))
(define char-set:letter+digit
  (char-set-union char-set:letter char-set:digit))
(define char-set:punctuation (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))
(define char-set:symbol (string->char-set "$+<=>^`|~"))
(define char-set:graphic
  (char-set-union char-set:letter+digit char-set:punctuation char-set:symbol))
(define char-set:whitespace
  (list->char-set
   '(#\tab #\newline #\vtab #\page #\return #\space #\240)))
(define char-set:printing
  (char-set-union char-set:whitespace char-set:graphic))
(define char-set:iso-control
  (char-set-union (range->char-set 0 33) (char-set #\delete)))
(define char-set:blank
  (list->char-set '(#\tab #\space #\240)))
(define char-set:ascii (range->char-set 0 128))
(define char-set:full char-set:ascii)
