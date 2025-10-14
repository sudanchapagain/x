;;; R7RS (scheme chars) library
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
;;; R7RS (scheme chars) implementation
;;;
;;; Code:

(library (scheme char)
  (export char-alphabetic?
          char-ci<=?
          char-ci<?
          char-ci=?
          char-ci>=?
          char-ci>?
          char-downcase
          char-foldcase
          char-lower-case?
          char-numeric?
          char-upcase
          char-upper-case?
          char-whitespace?
          digit-value
          string-ci<=?
          string-ci<?
          string-ci=?
          string-ci>=?
          string-ci>?
          string-downcase
          string-foldcase
          string-upcase)
  (import (only (hoot syntax) include-from-path)
          (hoot inline-wasm)
          (scheme base)
          (hoot bitwise)
          (only (hoot numbers) 1+)
          (only (hoot char)
                char-downcase char-upcase
                string-downcase string-upcase
                char-alphabetic? char-lower-case? char-numeric?
                char-upper-case? char-whitespace?))

  (define (char-foldcase char)
    (if (or (eqv? char #\460) (eqv? char #\461))
        char
        (char-downcase (char-upcase char))))

  (define (digit-value char)
    ;; The table can be extracted with:
    ;; awk -F ';' '/ZERO;Nd/ {print "#x"$1}' UnicodeData.txt
    ;; Up to date with Unicode 15.1.0.
    (define *decimal-zeroes*
      '#(#x0030 #x0660 #x06F0 #x07C0 #x0966 #x09E6 #x0A66 #x0AE6 #x0B66
                #x0BE6 #x0C66 #x0CE6 #x0D66 #x0DE6 #x0E50 #x0ED0 #x0F20
                #x1040 #x1090 #x17E0 #x1810 #x1946 #x19D0 #x1A80 #x1A90
                #x1B50 #x1BB0 #x1C40 #x1C50 #xA620 #xA8D0 #xA900 #xA9D0
                #xA9F0 #xAA50 #xABF0 #xFF10 #x104A0 #x10D30 #x11066
                #x110F0 #x11136 #x111D0 #x112F0 #x11450 #x114D0 #x11650
                #x116C0 #x11730 #x118E0 #x11950 #x11C50 #x11D50 #x11DA0
                #x11F50 #x16A60 #x16AC0 #x16B50 #x1D7CE #x1D7D8 #x1D7E2
                #x1D7EC #x1D7F6 #x1E140 #x1E2F0 #x1E4F0 #x1E950 #x1FBF0))
    (let ((cp (char->integer char)))
      (if (<= 0 (- cp (char->integer #\0)) 9)
          ;; Fast case.
          (- cp (char->integer #\0))
          ;; Otherwise, a binary search.
          (let lp ((start 0) (end (vector-length *decimal-zeroes*)))
            (and (< start end)
                 (let* ((mid (ash (+ start end) -1))
                        (val (- cp (vector-ref *decimal-zeroes* mid))))
                   (cond
                    ((< val 0) (lp start mid))
                    ((< val 10) val)
                    (else (lp (1+ mid) end)))))))))

  (define (char-ci<? ch1 ch2 . ch*)
    (apply char<?
           (char-foldcase ch1) (char-foldcase ch2) (map char-foldcase ch*)))
  (define (char-ci<=? ch1 ch2 . ch*)
    (apply char<=?
           (char-foldcase ch1) (char-foldcase ch2) (map char-foldcase ch*)))
  (define (char-ci=? ch1 ch2 . ch*)
    (apply char=?
           (char-foldcase ch1) (char-foldcase ch2) (map char-foldcase ch*)))
  (define (char-ci>=? ch1 ch2 . ch*)
    (apply char>=?
           (char-foldcase ch1) (char-foldcase ch2) (map char-foldcase ch*)))
  (define (char-ci>? ch1 ch2 . ch*)
    (apply char>?
           (char-foldcase ch1) (char-foldcase ch2) (map char-foldcase ch*)))

  (define (string-foldcase str)
    (string-downcase (string-upcase str)))

  ;; FIXME: We could use Intl.Collator instead of manually folding case.
  (define (string-ci<?  . strs) (apply string<?  (map string-foldcase strs)))
  (define (string-ci<=? . strs) (apply string<=? (map string-foldcase strs)))
  (define (string-ci=?  . strs) (apply string=?  (map string-foldcase strs)))
  (define (string-ci>=? . strs) (apply string>=? (map string-foldcase strs)))
  (define (string-ci>?  . strs) (apply string>?  (map string-foldcase strs))))
