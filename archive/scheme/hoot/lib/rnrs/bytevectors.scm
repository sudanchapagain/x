;;; Bytevectors
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
;;; R6RS bytevectors.
;;;
;;; Code:

(define-module (rnrs bytevectors)
  #:use-module ((hoot bytevectors)
                #:hide (bytevector-copy
                        bytevector-copy!))
  #:use-module ((hoot bytevectors)
                #:prefix hoot:
                #:select (bytevector-copy
                          bytevector-copy!))
  #:use-module ((hoot strings) #:select (string->utf8 utf8->string))
  ;; Missing bindings:
  ;; bytevector=?
  ;; bytevector-fill!
  ;; uniform-array->bytevector
  ;; u8-list->bytevector
  ;; bytevector->sint-list
  ;; bytevector->uint-list
  ;; uint-list->bytevector
  ;; sint-list->bytevector
  ;; string->utf16
  ;; string->utf32
  ;; utf16->string
  ;; utf32->string
  #:export (bytevector-copy
            bytevector-copy!)
  #:re-export (endianness
               native-endianness
               make-bytevector
               bytevector?
               bytevector-length
               bytevector-u8-ref
               bytevector-u8-set!
               bytevector-s8-ref
               bytevector-s8-set!
               bytevector-u16-native-ref
               bytevector-u16-ref
               bytevector-u16-native-set!
               bytevector-s16-ref
               bytevector-s16-native-ref
               bytevector-s16-set!
               bytevector-s16-native-set!
               bytevector-u32-ref
               bytevector-u32-native-ref
               bytevector-u32-set!
               bytevector-u32-native-set!
               bytevector-s32-ref
               bytevector-s32-native-ref
               bytevector-s32-set!
               bytevector-s32-native-set!
               bytevector-u64-ref
               bytevector-u64-native-ref
               bytevector-u64-set!
               bytevector-u64-native-set!
               bytevector-s64-ref
               bytevector-s64-native-ref
               bytevector-s64-set!
               bytevector-s64-native-set!
               bytevector-uint-ref
               bytevector-uint-set!
               bytevector-sint-ref
               bytevector-sint-set!
               bytevector-ieee-single-ref
               bytevector-ieee-single-native-ref
               bytevector-ieee-single-set!
               bytevector-ieee-single-native-set!
               bytevector-ieee-double-ref
               bytevector-ieee-double-native-ref
               bytevector-ieee-double-set!
               bytevector-ieee-double-native-set!
               string->utf8
               utf8->string))

(define (bytevector-copy bv)
  (hoot:bytevector-copy bv))

(define (bytevector-copy! source source-start target target-start len)
  (hoot:bytevector-copy! target target-start source source-start (+ source-start len)))
