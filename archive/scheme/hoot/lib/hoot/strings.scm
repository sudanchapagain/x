;;; Strings
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
;;; Strings.
;;;
;;; Code:

(library (hoot strings)
  (export string?
          mutable-string?
          string-length
          string-ref
          string-set!
          string
          make-string
          string-append
          string-copy
          substring
          string-copy!
          string-fill!
          string-for-each

          string-map
          string<?
          string<=?
          string=?
          string>=?
          string>?
          list->string
          string->list
          string-utf8-length
          string->utf8
          utf8->string)
  (import (only (hoot primitives)
                %string? %string-length %string-ref
                %utf8->string %string->utf8 %string-utf8-length
                guile:string->list guile:string-copy)
          (hoot apply)
          (hoot bitwise)
          (hoot bytevectors)
          (hoot char)
          (hoot cond-expand)
          (hoot eq)
          (hoot errors)
          (hoot inline-wasm)
          (hoot lists)
          (hoot match)
          (hoot numbers)
          (hoot pairs)
          (hoot syntax))

  ;; R7RS strings
  (define (string? x) (%string? x))

  (define (mutable-string? x)
    (%inline-wasm '(func (param $obj (ref eq))
                         (result (ref eq))
                         (if (ref eq)
                             (ref.test $mutable-string (local.get $obj))
                             (then (ref.i31 (i32.const 17)))
                             (else (ref.i31 (i32.const 1)))))
                  x))

  (define (string-length x) (%string-length x))

  (define (string-ref x i) (%string-ref x i))

  (define (%mutable-string-set-str! x x*)
    (check-type x mutable-string? '%mutable-string-set-str!)
    (check-type x* string? '%mutable-string-set-str!)
    (%inline-wasm '(func (param $s (ref $mutable-string))
                         (param $new-s (ref $string))
                         (struct.set $mutable-string
                                     $str
                                     (local.get $s)
                                     (struct.get $string
                                                 $str
                                                 (local.get $new-s))))
                  x x*)
    (if #f #f))

  (define (string-set! x i v)
    (check-type x mutable-string? 'string-set!)
    (check-range i 0 (1- (string-length x)) 'string-set!)
    (check-type v char? 'string-set!)
    (let ((x* (string-append (string-copy x 0 i)
                             (string v)
                             (string-copy x (1+ i) (string-length x)))))
      (%mutable-string-set-str! x x*)))

  (define (string . chars) (list->string chars))

  (define* (make-string n #:optional (init #\space))
    (check-type init char? 'make-string)
    (let lp ((n n) (chars '()))
      (if (zero? n)
          (list->string chars)
          (lp (1- n) (cons init chars)))))

  (define (string-append . strs)
    (utf8->string (bytevector-concatenate (map string->utf8 strs))))

  (define* (string-copy str #:optional (start 0) (end (string-length str)))
    (cond-expand
     (guile-vm
      (guile:string-copy str start end))
     (hoot
      (check-type str string? 'string-copy)
      (check-range start 0 (string-length str) 'string-copy)
      (check-range end start (string-length str) 'string-copy)
      (%inline-wasm
       '(func (param $str (ref string))
              (param $start i32)
              (param $end i32)
              (result (ref eq))
              (local $str_iter (ref stringview_iter))
              (local.set $str_iter (string.as_iter (local.get $str)))
              (drop
               (stringview_iter.advance (local.get $str_iter) (local.get $start)))
              (struct.new $mutable-string
                          (i32.const 0)
                          (stringview_iter.slice (local.get $str_iter)
                                                 (i32.sub (local.get $end)
                                                          (local.get $start)))))
       str start end))))

  (define (substring str start end)
    (string-copy str start end))

  (define* (string-copy! to at from #:optional (start 0) (end (string-length from)))
    (check-type to mutable-string? 'string-copy!)
    (check-range at 0 (string-length to) 'string-copy!)
    (check-type from string? 'string-copy!)
    (assert (<= (- end start) (- (string-length to) at)) 'string-copy!)
    (let ((to* (string-append (string-copy to 0 at)
                              (string-copy from start end)
                              (string-copy to (+ at (- end start))))))
      (%mutable-string-set-str! to to*)))

  (define* (string-fill! string fill
                         #:optional (start 0) (end (string-length string)))
    (check-type string mutable-string? 'string-fill!)
    (check-type fill char? 'string-fill!)
    (check-range start 0 (string-length string) 'string-fill!)
    (check-range end start (string-length string) 'string-fill!)
    (let ((string*
           (string-append (string-copy string 0 start)
                          (make-string (- end start) fill)
                          (string-copy string end (string-length string)))))
      (%mutable-string-set-str! string string*)))

  (define string-for-each
    (case-lambda
     ((f str) (for-each f (string->list str)))
     ((f str . strs)
      (apply for-each f (string->list str) (map string->list strs)))))

  ;; TODO: Support n strings, our 'map' doesn't support n lists yet.
  (define (string-map f str)
    (list->string (map f (string->list str))))

  (define (%string-compare a b)
    (if (eq? a b)
        0
        (cond-expand
         (guile-vm
          ;; Simple but allocation heavy implementation for use at
          ;; expansion time.
          (let lp ((a (string->list a))
                   (b (string->list b)))
            (match a
              ((ca . resta)
               (match b
                 ((cb . restb)
                  (let ((cmp (- (char->integer ca) (char->integer cb))))
                    (if (zero? cmp)
                        (lp resta restb)
                        cmp)))
                 (() 1)))
              (()
               (match b
                 (() 0)
                 (_ -1))))))
         (hoot
          (%inline-wasm
           '(func (param $a (ref string))
                  (param $b (ref string))
                  (result (ref eq))
                  (ref.i31 (i32.shl (string.compare (local.get $a) (local.get $b))
                                    (i32.const 1))))
           a b)))))

  (define (%string-compare* ordered? x y strs)
    (check-type x string? 'string-compare)
    (check-type y string? 'string-compare)
    (for-each (lambda (s) (check-type s string? 'string-compare)) strs)
    (define (pred a b) (ordered? (%string-compare a b) 0))
    (and (pred x y)
         (let lp ((y y) (strs strs))
           (match strs
             (() #t)
             ((z . strs) (and (pred y z) (lp z strs)))))))

  (define (string<?  x y . strs) (%string-compare* <  x y strs))

  (define (string<=? x y . strs) (%string-compare* <= x y strs))

  (define (string=?  x y . strs) (%string-compare* =  x y strs))

  (define (string>=? x y . strs) (%string-compare* >= x y strs))

  (define (string>?  x y . strs) (%string-compare* >  x y strs))

  (define (list->string chars)
    (define utf8-length
      (let lp ((len 0) (chars chars))
        (match chars
          (() len)
          ((ch . chars)
           (lp (+ len (let ((i (char->integer ch)))
                        (cond
                         ((<= i #x7f) 1)
                         ((<= i #x7ff) 2)
                         ((<= i #xffff) 3)
                         (else 4))))
               chars)))))
    (define bv (make-bytevector utf8-length 0))
    (let lp ((pos 0) (chars chars))
      (match chars
        (() (%utf8->string bv))
        ((ch . chars)
         (lp
          (+ pos
             (let ((i (char->integer ch)))
               (define (low-six i) (logand i #b111111))
               (define (put! offset byte)
                 (bytevector-u8-set! bv (+ pos offset) byte))
               (cond
                ((<= i #x7f)
                 (put! 0 i)
                 1)
                ((<= i #x7ff)
                 (put! 0 (logior #b11000000 (ash i -6)))
                 (put! 1 (logior #b10000000 (low-six i)))
                 2)
                ((<= i #xffff)
                 (put! 0 (logior #b11100000 (ash i -12)))
                 (put! 1 (logior #b10000000 (low-six (ash i -6))))
                 (put! 2 (logior #b10000000 (low-six i)))
                 3)
                (else
                 (put! 0 (logior #b11110000 (ash i -18)))
                 (put! 1 (logior #b10000000 (low-six (ash i -12))))
                 (put! 2 (logior #b10000000 (low-six (ash i -6))))
                 (put! 3 (logior #b10000000 (low-six i)))
                 4))))
          chars)))))

  (cond-expand
   (guile-vm
    (define string->list guile:string->list))
   (hoot
    (define* (string->list str #:optional (start 0) (end (string-length str)))
      (check-type str string? 'string->list)
      (check-range start 0 (string-length str) 'string->list)
      (check-range end start (string-length str) 'string->list)
      (%inline-wasm
       '(func (param $s (ref string)) (param $start i32) (param $end i32)
              (result (ref eq))
              (local $str_iter (ref stringview_iter))
              (local $s0 (ref eq))
              (local $i0 i32)
              (local.set $str_iter (string.as_iter (local.get $s)))
              (local.set $s0
                         (struct.new $mutable-pair
                                     (i32.const 0)
                                     (ref.i31 (i32.const 1))
                                     (ref.i31 (i32.const 13))))
              (local.set $i0
                         (i32.sub (local.get $end) (local.get $start)))
              (drop
               (stringview_iter.advance (local.get $str_iter) (local.get $start)))
              (ref.cast $mutable-pair (local.get $s0))
              (loop $lp
                    (if (local.get $i0)
                        (then
                         (ref.cast $mutable-pair (local.get $s0))
                         (local.tee
                          $s0
                          (struct.new $mutable-pair
                                      (i32.const 0)
                                      (ref.i31
                                       (i32.add
                                        (i32.shl (stringview_iter.next (local.get $str_iter))
                                                 (i32.const 2))
                                        (i32.const #b11)))
                                      (ref.i31 (i32.const 13))))
                         (struct.set $mutable-pair $cdr)
                         (local.set $i0 (i32.sub (local.get $i0) (i32.const 1)))
                         (br $lp))))
              (struct.get $mutable-pair $cdr))
       str start end))))

  (define (string-utf8-length str) (%string-utf8-length str))

  (define string->utf8
    (case-lambda
     ((str)           (%string->utf8 str))
     ((str start)     (%string->utf8
                       (if (zero? start)
                           str
                           (string-copy str start))))
     ((str start end) (%string->utf8
                       (if (and (zero? start) (eq? end (string-length str)))
                           str
                           (string-copy str start end))))))

  (define utf8->string
    (case-lambda
     ((bv)            (%utf8->string bv))
     ((bv start)      (%utf8->string
                       (if (zero? start)
                           bv
                           (bytevector-copy bv start))))
     ((bv start end)  (%utf8->string
                       (if (and (zero? start) (eq? end (bytevector-length bv)))
                           bv
                           (bytevector-copy bv start end)))))))
