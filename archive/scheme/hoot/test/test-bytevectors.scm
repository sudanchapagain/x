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
;;; Bytevector tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-bytevectors")

(test-call "10" (lambda (bv) (bytevector-length bv))
           #vu8(0 1 2 3 4 5 6 7 8 9))

(with-additional-imports
 ((only (hoot bytevectors)
        bytevector-s8-ref bytevector-s8-set!))
 (test-call "8" (lambda (bv) (bytevector-u8-ref bv 8))
            #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
 (test-call "8" (lambda (bv) (bytevector-s8-ref bv 8))
            #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
 (test-call "-9" (lambda (bv) (bytevector-s8-ref bv 9))
            #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
 (test-call "247" (lambda (bv) (bytevector-u8-ref bv 9))
            #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7)))

(with-additional-imports
 ((only (hoot bytevectors)
        bytevector-u16-native-ref bytevector-u16-native-set!
        bytevector-s16-native-ref bytevector-s16-native-set!))
 (test-call "65280" (lambda (bv) (bytevector-u16-native-ref bv 0))
            #vu8(#x00 #xff #xff #x00))
 (test-call "65535" (lambda (bv) (bytevector-u16-native-ref bv 1))
            #vu8(#x00 #xff #xff #x00))
 (test-call "255" (lambda (bv) (bytevector-u16-native-ref bv 2))
            #vu8(#x00 #xff #xff #x00))
 (test-call "-256" (lambda (bv) (bytevector-s16-native-ref bv 0))
            #vu8(#x00 #xff #xff #x00))
 (test-call "-1" (lambda (bv) (bytevector-s16-native-ref bv 1))
            #vu8(#x00 #xff #xff #x00))
 (test-call "255" (lambda (bv) (bytevector-s16-native-ref bv 2))
            #vu8(#x00 #xff #xff #x00)))

(with-additional-imports
 ((only (hoot bytevectors)
        bytevector-u32-native-ref bytevector-u32-native-set!
        bytevector-s32-native-ref bytevector-s32-native-set!))

 (test-call "50463231" (lambda (bv) (bytevector-u32-native-ref bv 0))
            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
 (test-call "67305985" (lambda (bv) (bytevector-u32-native-ref bv 1))
            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
 (test-call "4278452994" (lambda (bv) (bytevector-u32-native-ref bv 2))
            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
 (test-call "50463231" (lambda (bv) (bytevector-s32-native-ref bv 0))
            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
 (test-call "67305985" (lambda (bv) (bytevector-s32-native-ref bv 1))
            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
 (test-call "-16514302" (lambda (bv) (bytevector-s32-native-ref bv 2))
            #vu8(#xff #x01 #x02 #x03 #x04 #xff)))

(with-additional-imports
 ((only (hoot bytevectors)
        bytevector-u64-native-ref bytevector-u64-native-set!
        bytevector-s64-native-ref bytevector-s64-native-set!))

 (test-call "511" (lambda (bv) (bytevector-u64-native-ref bv 0))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "1" (lambda (bv) (bytevector-u64-native-ref bv 1))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "72057594037927936"
            (lambda (bv) (bytevector-u64-native-ref bv 2))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "18374967954648334336"
            (lambda (bv) (bytevector-u64-native-ref bv 3))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "511" (lambda (bv) (bytevector-s64-native-ref bv 0))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "1" (lambda (bv) (bytevector-s64-native-ref bv 1))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "72057594037927936"
            (lambda (bv) (bytevector-s64-native-ref bv 2))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "-71776119061217280"
            (lambda (bv) (bytevector-s64-native-ref bv 3))
            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
 (test-call "-65025" (lambda (bv) (bytevector-s64-native-ref bv 0))
            #vu8(#xff 1 #xff #xff #xff #xff #xff #xff)))

(with-additional-imports
 ((only (hoot bytevectors)
        bytevector-ieee-single-native-ref
        bytevector-ieee-single-native-set!
        bytevector-ieee-double-native-ref
        bytevector-ieee-double-native-set!))
 (test-call "42.69" (lambda (bv)
                      (bytevector-ieee-double-native-ref bv 0))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "42.689998626708984"
            (lambda (bv)
              (bytevector-ieee-single-native-ref bv 0))
            #vu8(143 194 42 66))

 (test-call "85.38"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (+ f64 f64)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "43.69"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (+ f64 1.0)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "41.69"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (- f64 1.0)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "64.035"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (* f64 1.5)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "21.345"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (/ f64 2.0)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "-57.31"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (- f64 100.0)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "57.31"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (abs (- f64 100.0))))
            #vu8(184 30 133 235 81 88 69 64))
 (with-additional-imports ((scheme inexact))
   (test-call "6.5337584895678535"
              (lambda (bv)
                (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                  (sqrt (abs f64))))
              #vu8(184 30 133 235 81 88 69 64)))
 (test-call "42.0"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (floor f64)))
            #vu8(184 30 133 235 81 88 69 64))
 (test-call "43.0"
            (lambda (bv)
              (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                (ceiling f64)))
            #vu8(184 30 133 235 81 88 69 64))

 (with-additional-imports ((scheme inexact))
   (test-call "(-0.9614691168217643 0.2749129633138033 -3.497358237429792)"
              (lambda (bv)
                (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                  (list (sin f64)
                        (cos f64)
                        (tan f64))))
              #vu8(184 30 133 235 81 88 69 64))

   ;; Not testing fasin, facos for now because apparently Guile doesn't emit those!

   (test-call "(1.5473759202633208 0.7853981633974483)"
              (lambda (bv)
                (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
                  (list (atan f64)
                        (atan f64 f64))))
              #vu8(184 30 133 235 81 88 69 64))))

(with-additional-imports ((only (hoot bytevectors)
                                bytevector-uint-ref
                                bytevector-uint-set!
                                bytevector-sint-ref
                                bytevector-sint-set!
                                endianness))
  (test-call "15715755"
             (lambda (bv) (bytevector-uint-ref bv 0 (endianness little) 3))
             #vu8(171 205 239))
  (test-call "-1061461"
             (lambda (bv) (bytevector-sint-ref bv 0 (endianness little) 3))
             #vu8(171 205 239))
  (test-call "#vu8(239 205 171)"
             (lambda ()
               (let ((bv (make-bytevector 3)))
                 (bytevector-uint-set! bv 0 #xabcdef (endianness little) 3)
                 bv)))
  (test-call "#vu8(17 50 180)"
             (lambda ()
               (let ((bv (make-bytevector 3)))
                 (bytevector-sint-set! bv 0 #x-4bcdef (endianness little) 3)
                 bv))))

;; Big endian
(with-additional-imports ((only (hoot bytevectors)
                                bytevector-u16-ref
                                bytevector-u16-set!
                                bytevector-s16-ref
                                bytevector-s16-set!
                                bytevector-u32-ref
                                bytevector-u32-set!
                                bytevector-s32-ref
                                bytevector-s32-set!
                                bytevector-u64-ref
                                bytevector-u64-set!
                                bytevector-s64-ref
                                bytevector-s64-set!
                                bytevector-uint-ref
                                bytevector-uint-set!
                                bytevector-sint-ref
                                bytevector-sint-set!
                                bytevector-ieee-single-ref
                                bytevector-ieee-single-set!
                                bytevector-ieee-double-ref
                                bytevector-ieee-double-set!
                                endianness))
  (test-call "12345"
             (lambda (bv) (bytevector-u16-ref bv 0 (endianness big)))
             #vu8(48 57))
  (test-call "-12345"
             (lambda (bv) (bytevector-s16-ref bv 0 (endianness big)))
             #vu8(207 199))
  (test-call "1234567"
             (lambda (bv) (bytevector-u32-ref bv 0 (endianness big)))
             #vu8(0 18 214 135))
  (test-call "-1234567"
             (lambda (bv) (bytevector-s32-ref bv 0 (endianness big)))
             #vu8(255 237 41 121))
  (test-call "123456789123456789"
             (lambda (bv) (bytevector-u64-ref bv 0 (endianness big)))
             #vu8(1 182 155 75 172 208 95 21))
  (test-call "-123456789123456789"
             (lambda (bv) (bytevector-s64-ref bv 0 (endianness big)))
             #vu8(254 73 100 180 83 47 160 235))
  (test-call "11259375"
             (lambda (bv) (bytevector-uint-ref bv 0 (endianness big) 3))
             #vu8(171 205 239))
  (test-call "-5517841"
             (lambda (bv) (bytevector-sint-ref bv 0 (endianness big) 3))
             #vu8(171 205 239))
  (test-call "1.2999999523162842"
             (lambda (bv) (bytevector-ieee-single-ref bv 0 (endianness big)))
             #vu8(63 166 102 102))
  (test-call "1.3"
             (lambda (bv) (bytevector-ieee-double-ref bv 0 (endianness big)))
             #vu8(63 244 204 204 204 204 204 205))
  (test-call "#vu8(171 205)"
             (lambda ()
               (let ((bv (make-bytevector 2)))
                 (bytevector-u16-set! bv 0 #xabcd (endianness big))
                 bv)))
  (test-call "#vu8(180 51)"
             (lambda ()
               (let ((bv (make-bytevector 2)))
                 (bytevector-s16-set! bv 0 #x-4bcd (endianness big))
                 bv)))
  (test-call "#vu8(171 205 239 171)"
             (lambda ()
               (let ((bv (make-bytevector 4)))
                 (bytevector-u32-set! bv 0 #xabcdefab (endianness big))
                 bv)))
  (test-call "#vu8(180 50 16 85)"
             (lambda ()
               (let ((bv (make-bytevector 4)))
                 (bytevector-s32-set! bv 0 #x-4bcdefab (endianness big))
                 bv)))
  (test-call "#vu8(171 205 239 171 205 239 171 205)"
             (lambda ()
               (let ((bv (make-bytevector 8)))
                 (bytevector-u64-set! bv 0 #xabcdefabcdefabcd (endianness big))
                 bv)))
  (test-call "#vu8(180 50 16 84 50 16 84 51)"
             (lambda ()
               (let ((bv (make-bytevector 8)))
                 (bytevector-s64-set! bv 0 #x-4bcdefabcdefabcd (endianness big))
                 bv)))
  (test-call "#vu8(171 205 239)"
             (lambda ()
               (let ((bv (make-bytevector 3)))
                 (bytevector-uint-set! bv 0 #xabcdef (endianness big) 3)
                 bv)))
  (test-call "#vu8(180 50 17)"
             (lambda ()
               (let ((bv (make-bytevector 3)))
                 (bytevector-sint-set! bv 0 #x-4bcdef (endianness big) 3)
                 bv)))
  (test-call "#vu8(63 166 102 102)"
             (lambda ()
               (let ((bv (make-bytevector 4)))
                 (bytevector-ieee-single-set! bv 0 1.3 (endianness big))
                 bv)))
  (test-call "#vu8(63 244 204 204 204 204 204 205)"
             (lambda ()
               (let ((bv (make-bytevector 8)))
                 (bytevector-ieee-double-set! bv 0 1.3 (endianness big))
                 bv))))

(test-call "#vu8(0 0 0 0 0)"
           (lambda () (make-bytevector 5)))
(test-call "#vu8(42 42 42 42 42)"
           (lambda () (make-bytevector 5 42)))
(test-call "#vu8(1 2 3 4)"
           (lambda () (bytevector 1 2 3 4)))

(test-call "#t" (lambda (a b) (equal? a b)) #vu8() #vu8())
(test-call "#t" (lambda (a b) (equal? a b)) #vu8(1 2) #vu8(1 2))
(test-call "#f" (lambda (a b) (equal? a b)) #vu8() #vu8(1))
(test-call "#f" (lambda (a b) (equal? a b)) #vu8(1 2) #vu8(2 1))
(test-call "#f" (lambda (a b) (equal? a b)) #vu8(1 2 1) #vu8(1 2 3))

(test-end* "test-bytevectors")
