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
;;; String tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-strings")

(test-call "3" (lambda (str) (string-length str)) "fox")
(test-call "#\\f" (lambda (str) (string-ref str 0)) "fox")
(test-call "#\\x" (lambda (str) (string-ref str 2)) "fox")
(test-call "#t" (lambda (a b) (string=? a b)) "Even" "Even")
(test-call "#t" (lambda (a b) (string>? a b)) "Odd" "Even")
(test-call "#t" (lambda (a b) (string<? a b)) "Even" "Odd")

(test-call "\"abc\"" (lambda (a) (string-copy a)) "abc")

(test-call "\"IBM\""
           (lambda (str)
             (string-map (lambda (c)
                           (integer->char (+ 1 (char->integer c))))
                         str))
           "HAL")
;; Enable when string-map supports 2+ strings.
;; (test-call "\"StUdLyCaPs\""
;;            (lambda (a b)
;;              (string-map
;;               (lambda (c k)
;;                 ((if (eqv? k #\u) char-upcase char-downcase) c))
;;               a b))
;;            "studlycaps xxx"
;;            "ululululul")

(with-additional-imports
    ((only (guile) char-set:lower-case
           string-index string-rindex
           string-prefix? string-prefix-ci?
           string-reverse string-split
           string-suffix? string-suffix-ci?
           string-trim string-trim-right string-trim-both))

  (test-call "2" (lambda (str c) (string-index str c)) "fox" #\x)
  (test-call "2" (lambda (str c) (string-index str c)) "fox"
             (lambda (c) (char=? c #\x)))
  (test-call "0" (lambda (str) (string-index str char-set:lower-case))
             "fox")
  (test-call "2" (lambda (str c start) (string-index str c start)) "fox" #\x 1)
  (test-call "1" (lambda (str c start end) (string-index str c start end))
             "fox" #\o 0 2)

  (test-call "3" (lambda (str c) (string-rindex str c)) "foxx" #\x)
  (test-call "3" (lambda (str c) (string-rindex str c)) "foxx"
             (lambda (c) (char=? c #\x)))
  (test-call "3" (lambda (str c start) (string-rindex str c start))
             "foxx" #\x 1)
  (test-call "3" (lambda (str c start end) (string-rindex str c start end))
             "foxx" #\x 1 4)

  (test-call "#t" (lambda (str1 str2) (string-prefix? str1 str2)) "fo" "fox")
  (test-call "#t" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-prefix? str1 str2 start1 end1 start2 end2))
             "bird" "birdie" 1 3 1 4)
  (test-call "#f" (lambda (str1 str2) (string-prefix? str1 str2)) "Fo" "fox")
  (test-call "#f" (lambda (str1 str2) (string-prefix? str1 str2)) "of" "fox")
  (test-call "#f" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-prefix? str1 str2 start1 end1 start2 end2))
             "bird" "birdie" 1 3 2 4)

  (test-call "#t" (lambda (str1 str2) (string-prefix-ci? str1 str2)) "fO" "FoX")
  (test-call "#t" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-prefix-ci? str1 str2 start1 end1 start2 end2))
             "bIrD" "BiRdIe" 1 3 1 4)
  (test-call "#f" (lambda (str1 str2) (string-prefix-ci? str1 str2)) "oF" "FoX")
  (test-call "#f" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-prefix-ci? str1 str2 start1 end1 start2 end2))
             "bIrD" "BiRdIe" 1 3 2 4)

  (test-call "\"esrever\"" (lambda (str) (string-reverse str)) "reverse")
  (test-call "\"rerevse\"" (lambda (str start end) (string-reverse str start end))
             "reverse" 2 5)

  (test-call "(\"root\" \"x\" \"0\" \"0\" \"root\" \"/root\" \"/bin/bash\")"
             (lambda () (string-split "root:x:0:0:root:/root:/bin/bash" #\:)))
  (test-call "(\"\" \"\" \"\")"
             (lambda () (string-split "::" #\:)))
  (test-call "(\"\")"
             (lambda () (string-split "" #\:)))

  (test-call "#t" (lambda (str1 str2) (string-suffix? str1 str2)) "ox" "fox")
  (test-call "#t" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-suffix? str1 str2 start1 end1 start2 end2))
             "rdi" "birdie" 1 3 1 5)
  (test-call "#f" (lambda (str1 str2) (string-suffix? str1 str2)) "Ox" "fox")
  (test-call "#f" (lambda (str1 str2) (string-suffix? str1 str2)) "xo" "fox")
  (test-call "#f" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-suffix? str1 str2 start1 end1 start2 end2))
             "rdi" "birdie" 1 3 1 3)

  (test-call "#t" (lambda (str1 str2) (string-suffix-ci? str1 str2)) "Ox" "FoX")
  (test-call "#t" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-suffix-ci? str1 str2 start1 end1 start2 end2))
             "IrDi" "BiRdIe" 1 3 1 4)
  (test-call "#f" (lambda (str1 str2) (string-suffix-ci? str1 str2)) "oF" "FoX")
  (test-call "#f" (lambda (str1 str2 start1 end1 start2 end2)
                    (string-suffix-ci? str1 str2 start1 end1 start2 end2))
             "IrDi" "BiRdIe" 1 3 1 5)

  (test-call "\"\"" (lambda (str) (string-trim str)) "")
  (test-call "\"\"" (lambda (str) (string-trim str)) "  \t  ")
  (test-call "\"fox  \"" (lambda (str) (string-trim str)) "  fox  ")
  (test-call "\"  fox\"" (lambda (str) (string-trim-right str)) "  fox  ")
  (test-call "\"fox\"" (lambda (str) (string-trim-both str)) "  fox  "))

;; String mutation
(with-additional-imports
 ((only (hoot strings) mutable-string?))
 (test-call "#f" (lambda (a) (mutable-string? a)) "abc")
 (test-call "#t" (lambda () (mutable-string? (make-string 1))))
 (test-call "#t" (lambda () (mutable-string? (string-copy "abc"))))
 (test-call "#t" (lambda () (mutable-string? (string #\a #\b #\c)))))

(test-call "\"1@3\"" (lambda (a)
                       (let ((a (string-copy a)))
                         (string-set! a 1 #\@)
                         a))
           "123")
(test-call "\"a123e\"" (lambda (a)
                         (let ((b (string-copy "abcde")))
                           (string-copy! b 1 a 0 3)
                           b))
           "12345")
(test-call "\"a!!!!f\"" (lambda ()
                          (let ((a (string-copy "abcdef")))
                            (string-fill! a #\! 1 5)
                            a)))

(with-additional-imports ((scheme char))
  (test-equal "passing big strings to the host"
              (string-append "\"" (make-string 200001 #\W) "\"")
              (call-wasm (compile-main '(lambda (str) (string-upcase str)))
                         (compile-aux (make-string 200001 #\w)))))

(test-end* "test-strings")
