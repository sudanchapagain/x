;;; (hoot write) library
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
;;; R7RS (scheme write) implementation, plus number->string
;;;
;;; Code:

(library (hoot write)
  (export number->string
          display write write-shared write-simple)
  (import (hoot syntax)
          (hoot errors)
          (hoot external)
          (hoot inline-wasm)
          (hoot bitwise)
          (hoot bitvectors)
          (hoot bytevectors)
          (hoot char)
          (hoot keywords)
          (hoot procedures)
          (hoot strings)
          (hoot symbols)
          (hoot numbers)
          (hoot vectors)
          (hoot ports)
          (only (hoot records)
                record? write-record)
          (hoot syntax-objects)
          (hoot syntax-transformers)
          (hoot pairs)
          (hoot eq)
          (hoot match))

(define* (number->string n #:optional (radix 10))
  (cond
   ((exact-integer? n)
    (if (zero? n)
        "0"
        (let* ((mag (if (< n 0) (- n) n))
               (digits
                (case radix
                  ((2) (let lp ((mag mag) (out '()))
                         (if (zero? mag)
                             out
                             (lp (ash mag -1)
                                 (cons (integer->char
                                        (+ (char->integer #\0)
                                           (logand mag 1)))
                                       out)))))
                  ((8) (let lp ((mag mag) (out '()))
                         (if (zero? mag)
                             out
                             (lp (ash mag -3)
                                 (cons (integer->char
                                        (+ (char->integer #\0)
                                           (logand mag 7)))
                                       out)))))
                  ((10) (let lp ((mag mag) (out '()))
                          (if (zero? mag)
                              out
                              (lp (quotient mag 10)
                                  (cons (integer->char
                                         (+ (char->integer #\0)
                                            (remainder mag 10)))
                                        out)))))
                  ((16) (let lp ((mag mag) (out '()))
                          (if (zero? mag)
                              out
                              (lp (ash mag -4)
                                  (cons (integer->char
                                         (let ((digit (logand mag 15)))
                                           (+ (if (< digit 10)
                                                  (char->integer #\0)
                                                  (- (char->integer #\a) 10))
                                              digit)))
                                        out))))))))
          (list->string (if (negative? n) (cons #\- digits) digits)))))
   ((exact? n)
    (string-append (number->string (numerator n) radix)
                   "/"
                   (number->string (denominator n) radix)))
   ((real? n)
    (assert (eqv? radix 10) 'number->string)
    (%inline-wasm
     '(func (param $n f64)
            (result (ref eq))
            (struct.new $string
                        (i32.const 0)
                        (call $flonum->string (local.get $n))))
     n))
   (else
    (string-append (number->string (real-part n) radix)
                   "/"
                   (number->string (imag-part n) radix)
                   "i"))))

  (define* (%write-datum port x #:optional quote-strings?)
    (define (recur x) (%write-datum port x quote-strings?))
    (cond
     ((eq? x #f)         (write-string "#f" port))
     ((eq? x #t)         (write-string "#t" port))
     ((eq? x #nil)       (write-string "#nil" port))
     ((eq? x '())        (write-string "()" port))
     ((eq? x (if #f #f)) (write-string "#<unspecified>" port))
     ((eof-object? x)    (write-string "#<eof>" port))
     ((number? x)        (write-string (number->string x) port))
     ((char? x)
      (if quote-strings?
          (case x
            ((#\alarm)     (write-string "#\\alarm" port))
            ((#\backspace) (write-string "#\\backspace" port))
            ((#\delete)    (write-string "#\\delete" port))
            ((#\escape)    (write-string "#\\escape" port))
            ((#\newline)   (write-string "#\\newline" port))
            ((#\null)      (write-string "#\\null" port))
            ((#\return)    (write-string "#\\return" port))
            ((#\space)     (write-string "#\\space" port))
            ((#\tab)       (write-string "#\\tab" port))
            ((#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
              #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
              #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
              #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
              #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
              #\` #\~ #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\_ #\= #\+
              #\[ #\] #\{ #\} #\\ #\| #\; #\: #\' #\" #\< #\> #\, #\. #\/ #\?)
             (write-char #\# port)
             (write-char #\\ port)
             (write-char x port))
            (else
             (write-char #\# port)
             (write-char #\\ port)
             (write-char #\x port)
             (write-string (number->string (char->integer x) 16) port)))
          (write-char x port)))
     ((pair? x)
      (write-char #\( port)
      (recur (car x))
      (let lp ((tail (cdr x)))
        (cond
         ((null? tail)
          (write-char #\) port))
         ((pair? tail)
          (write-char #\space port)
          (recur (car tail))
          (lp (cdr tail)))
         (else
          (write-string " . " port)
          (recur tail)
          (write-char #\) port)))))
     ((string? x)
      (cond
       (quote-strings?
        (write-char #\" port)
        (string-for-each (lambda (ch)
                           (case ch
                             ((#\newline)
                              (write-char #\\ port)
                              (write-char #\n port))
                             ((#\\ #\")
                              (write-char #\\ port)
                              (write-char ch port))
                             (else
                              (write-char ch port))))
                         x)
        (write-char #\" port))
       (else
        (write-string x port))))
     ((symbol? x)
      (%write-datum port (symbol->string x) #f))
     ((vector? x)
      (write-char #\# port)
      (recur (vector->list x)))
     ((bytevector? x)
      (write-string "#vu8(" port)
      (let lp ((i 0))
        (when (< i (bytevector-length x))
          (unless (zero? i)
            (write-char #\space port))
          (write-string (number->string (bytevector-u8-ref x i)) port)
          (lp (1+ i))))
      (write-char #\) port))
     ((bitvector? x)
      (write-string "#*" port)
      (let lp ((i 0))
        (when (< i (bitvector-length x))
          (write-char (if (bitvector-ref x i) #\1 #\0) port)
          (lp (1+ i)))))
     ;; Test for records before procedures because applicable records
     ;; are considered procedures.
     ((record? x)
      (write-record x port write))
     ((procedure? x)
      (match (procedure-name x)
        (#f
         (write-string "#<procedure>" port))
        (name
         (write-string "#<procedure " port)
         (write name port)
         (write-char #\> port))))
     ((keyword? x)
      (write-string "#:" port)
      (write-string (symbol->string (keyword->symbol x)) port))
     ((port? x)
      (write-string "#<port>" port))
     ((syntax? x)
      (write-string "#<syntax" port)
      (let ((expr (syntax-expression x))
            (src (syntax-sourcev x)))
        (when src
          (let ((file (vector-ref src 0))
                (line (1+ (vector-ref src 1)))
                (col (vector-ref src 2)))
            (write-char #\: port)
            (write-string (or file "unknown file") port)
            (write-char #\: port)
            (write-string (number->string line) port)
            (write-char #\: port)
            (write-string (number->string col) port)))
        (write-char #\space port)
        (write expr port)
        (write-char #\> port)))
     ((syntax-transformer? x)
      (write-string "#<syntax-transformer " port)
      (write (syntax-transformer-type x) port)
      (write-char #\space port)
      (write (syntax-transformer-value x) port)
      (write-char #\> port))
     ((external? x)
      (write-string "#<external null?: " port)
      (write (external-null? x) port)
      (write-char #\> port))
     (else
      (recur "unhandled object :("))))

  (define* (display datum #:optional (port (current-output-port)))
    (%write-datum port datum #f))
  (define* (write datum #:optional (port (current-output-port)))
    (%write-datum port datum #t))

  (define* (write-shared datum #:optional (port (current-output-port)))
    (raise (make-unimplemented-error 'write-shared)))
  (define* (write-simple datum #:optional (port (current-output-port)))
    (write datum port))
  )
