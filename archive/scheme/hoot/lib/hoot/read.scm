;;; R7RS (scheme read) library
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
;;; R7RS (scheme read) implementation
;;;
;;; Code:

(library (hoot read)
  (export read read-syntax string->number)
  (import (hoot bitvectors)
          (hoot char)
          (hoot eq)
          (hoot errors)
          (hoot exceptions)
          (hoot keywords)
          (hoot lists)
          (hoot match)
          (hoot not)
          (hoot numbers)
          (hoot pairs)
          (hoot ports)
          (hoot strings)
          (hoot symbols)
          (hoot syntax)
          (hoot syntax-objects)
          (hoot values)
          (hoot vectors))

  (define* (string->number str #:optional (radix 10))
    (let ((port (open-input-string str)))
      (define (read-bin-digit)
        (case (peek-char port)
          ((#\0 #\1)
           (- (char->integer (read-char port)) (char->integer #\0)))
          (else #f)))
      (define (read-oct-digit)
        (case (peek-char port)
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
           (- (char->integer (read-char port)) (char->integer #\0)))
          (else #f)))
      (define (read-dec-digit)
        (case (peek-char port)
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (- (char->integer (read-char port)) (char->integer #\0)))
          (else #f)))
      (define (read-hex-digit)
        (case (peek-char port)
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (- (char->integer (read-char port)) (char->integer #\0)))
          ((#\a #\b #\c #\d #\e #\f)
           (+ 10 (- (char->integer (read-char port)) (char->integer #\a))))
          ((#\A #\B #\C #\D #\E #\F)
           (+ 10 (- (char->integer (read-char port)) (char->integer #\A))))
          (else #f)))
      (define (read-unsigned-int radix)
        (case radix
          ((2)
           (let ((x (read-bin-digit)))
             (and x
                  (let loop ((x x))
                    (let ((y (read-bin-digit)))
                      (if y (loop (+ (* x 2) y)) x))))))
          ((8)
           (let ((x (read-oct-digit)))
             (and x
                  (let loop ((x x))
                    (let ((y (read-oct-digit)))
                      (if y (loop (+ (* x 8) y)) x))))))
          ((10)
           (let ((x (read-dec-digit)))
             (and x
                  (let loop ((x x))
                    (let ((y (read-dec-digit)))
                      (if y (loop (+ (* x 10) y)) x))))))
          ((16)
           (let ((x (read-hex-digit)))
             (and x
                  (let loop ((x x))
                    (let ((y (read-hex-digit)))
                      (if y (loop (+ (* x 16) y)) x))))))))
      (define (read-sign)
        (let ((ch (peek-char port)))
          (cond
           ((eof-object? ch) #f)
           ((eqv? ch #\+)
            (read-char port)
            '+)
           ((eqv? ch #\-)
            (read-char port)
            '-)
           (else 'none))))
      (define (read-decimal n exactness)
        (case (peek-char port)
          ;; Decimal point
          ((#\.)
           (read-char port)
           (let ((ch (peek-char port)))
             ;; '0.' is a valid number, but '.' is not.  'n' being #f
             ;; signals the latter case.
             (if (eof-object? ch)
                 (and n (inexact n))
                 (let ((n (or n 0))
                       (x (read-dec-digit)))
                   (and x
                        (let loop ((i -2) (x (* x (expt 10 -1))))
                          (let ((y (read-dec-digit)))
                            (if y
                                (loop (- i 1) (+ x (* y (expt 10 i))))
                                (let ((z (+ n x)))
                                  (or (read-decimal z exactness)
                                      (if (eq? exactness 'exact)
                                          z
                                          (inexact z))))))))))))
          ;; Exponent
          ((#\e #\E)
           (read-char port)
           (let* ((sign (read-sign))
                  (x (read-unsigned-int 10)))
             (and x
                  (let ((y (* n (expt 10 (if (eq? sign '-) (- x) x)))))
                    (if (eq? exactness 'exact) y (inexact y))))))
          (else #f)))
      (define (read-unsigned radix exactness)
        (let ((ch (peek-char port)))
          (cond
           ((eof-object? ch) #f)
           ;; NaN
           ((or (eqv? ch #\n) (eqv? ch #\N))
            (read-char port)
            (case (read-char port)
              ((#\a #\A)
               (case (read-char port)
                 ((#\n #\N)
                  (case (read-char port)
                    ((#\.)
                     (case (read-char port)
                       ((#\0) +nan.0)
                       (else #f)))
                    (else #f)))
                 (else #f)))
              (else #f)))
           ;; Infinity
           ((or (eqv? ch #\i) (eqv? ch #\I))
            (read-char port)
            (let ((ch (peek-char port)))
              (cond
               ;; This might be a valid complex number, either '+i' or
               ;; '-i', so back up a char so the caller can check for
               ;; that case.
               ((eof-object? ch)
                (seek port -1 'cur)
                #f)
               ((or (eqv? ch #\n) (eqv? ch #\N))
                (read-char port)
                (case (read-char port)
                  ((#\f #\F)
                   (case (read-char port)
                     ((#\.)
                      (case (read-char port)
                        ((#\0) +inf.0)
                        (else #f)))
                     (else #f)))
                  (else #f)))
               (else #f))))
           ;; Decimal with no leading digits.
           ((eqv? ch #\.)
            (and (eqv? radix 10) (read-decimal #f exactness)))
           (else
            (let ((x (read-unsigned-int radix)))
              (and x
                   (case (peek-char port)
                     ;; Fraction
                     ((#\/)
                      (read-char port)
                      (let ((y (read-unsigned-int radix)))
                        (and y
                             (let ((z (/ x y)))
                               (if (eq? exactness 'inexact) (inexact z) z)))))
                     ;; Decimal point or exponent
                     ((#\. #\e #\E)
                      (and (eqv? radix 10) (read-decimal x exactness)))
                     (else
                      (if (eq? exactness 'inexact) (inexact x) x)))))))))
      (define (read-complex radix exactness)
        (let ((sign (read-sign)))
          (and sign
               (let ((x (read-unsigned radix exactness)))
                 (cond
                  ((or (and (not x) (eq? sign 'none))
                       ;; Infinities and NaNs need explicit sign.
                       (and x (or (infinite? x) (nan? x)) (eq? sign 'none)))
                   #f)
                  ;; +i and -i cases.
                  ((not x)
                   (let ((ch (read-char port)))
                     (and (or (eqv? ch #\i) (eqv? ch #\I))
                          (if (eq? sign '+) +i -i))))
                  ;; We've successfully read one real, now to check for
                  ;; a polar or imaginary part.
                  (else
                   (let ((x (if (eq? sign '-) (- x) x)))
                     (let ((ch (peek-char port)))
                       (cond
                        ((eof-object? ch) x)
                        ;; Complex number in polar form.
                        ((eqv? ch #\@)
                         (read-char port)
                         (let* ((sign (read-sign))
                                (y (read-unsigned radix exactness)))
                           (and y (make-polar x (if (eq? sign '-) (- y) y)))))
                        ;; Complex number in rectangular form.
                        ((or (eqv? ch #\+) (eqv? ch #\-))
                         (let ((sign (read-sign))
                               (y (or (read-unsigned radix exactness) 1.0)))
                           (case (read-char port)
                             ((#\i #\I)
                              (make-rectangular x (if (eq? sign '-) (- y) y)))
                             (else #f))))
                        (else #f))))))))))
      (define (read-number)
        ;; First, read the radix and exactness prefix.  These could be
        ;; specified in either order (like #x#e or #e#x), one could be
        ;; omitted (just #x or #e), or both could be omitted.  When
        ;; exactness is omitted, exactness becomes implicit.  For
        ;; example, '1.2' will produce an inexact value.
        (let loop ((radix* #f) (exactness #f))
          (let ((ch (peek-char port)))
            (cond
             ((eof-object? ch) #f)
             ((eqv? ch #\#)
              (read-char port)
              (let ((ch (read-char port)))
                (cond
                 ((and (or (eqv? ch #\b) (eqv? ch #\B)) (not radix*))
                  (loop 2 exactness))
                 ((and (or (eqv? ch #\o) (eqv? ch #\O)) (not radix*))
                  (loop 8 exactness))
                 ((and (or (eqv? ch #\d) (eqv? ch #\D)) (not radix*))
                  (loop 10 exactness))
                 ((and (or (eqv? ch #\x) (eqv? ch #\X)) (not radix*))
                  (loop 16 exactness))
                 ((and (or (eqv? ch #\e) (eqv? ch #\E)) (not exactness))
                  (loop radix* 'exact))
                 ((and (or (eqv? ch #\i) (eqv? ch #\I)) (not exactness))
                  (loop radix* 'inexact))
                 (else #f))))
             (else
              (read-complex (or radix* radix) exactness))))))
      (let ((x (read-number)))
        ;; Input should be completely consumed at this point.
        (and (eof-object? (peek-char port)) x))))

  (define (%read port annotate strip-annotation)
    (define fold-case? (%port-fold-case? port))
    (define (set-fold-case?! val)
      (set! fold-case? val)
      (%set-port-fold-case?! port val))

    (define (next) (read-char port))
    (define (peek) (peek-char port))
    ;; We are only ever interested in whether an object is a char or not.
    (define (eof-object? x) (not (char? x)))

    (define (input-error msg args)
      (raise
       (make-exception (make-lexical-violation)
                       (make-exception-with-origin "read")
                       (make-exception-with-message msg)
                       (make-exception-with-irritants args)
                       (make-i/o-filename-error (port-filename port))
                       (make-i/o-line-and-column-error (1+ (port-line port))
                                                       (1+ (port-column port))))))

    (define-syntax-rule (error msg arg ...)
      (let ((args (list arg ...)))
        (input-error msg args)))

    (define (read-semicolon-comment)
      (let ((ch (next)))
        (cond
         ((eof-object? ch) ch)
         ((eqv? ch #\newline) (next))
         (else (read-semicolon-comment)))))

    (define-syntax-rule (take-until first pred)
      (let ((p (open-output-string)))
        (write-char first p)
        (let lp ()
          (let ((ch (peek)))
            (if (or (eof-object? ch) (pred ch))
                (get-output-string p)
                (begin
                  (write-char ch p)
                  (next)
                  (lp)))))))
    (define-syntax-rule (take-while first pred)
      (take-until first (lambda (ch) (not (pred ch)))))

    (define (delimiter? ch)
      (case ch
        ((#\( #\) #\; #\" #\space #\return #\ff #\newline #\tab #\[ #\]) #t)
        (else #f)))

    (define (read-token ch)
      (take-until ch delimiter?))

    (define (read-mixed-case-symbol ch)
      (let ((str (read-token ch)))
        (string->symbol (if fold-case? (string-downcase str) str))))

    (define (read-parenthesized rdelim)
      (let lp ((ch (next-non-whitespace)))
        (when (eof-object? ch)
          (error "unexpected end of input while searching for: ~A"
                 rdelim))
        (cond
         ((eqv? ch rdelim) '())
         ((or (eqv? ch #\)) (eqv? ch #\]))
          (error "mismatched close paren: ~A" ch))
         (else
          (let ((expr (read-expr ch)))
            ;; Note that it is possible for scm_read_expression to
            ;; return `.', but not as part of a dotted pair: as in
            ;; #{.}#.  Indeed an example is here!
            (if (and (eqv? ch #\.) (eq? (strip-annotation expr) '#{.}#))
                (let* ((tail (read-subexpression "tail of improper list"))
                       (close (next-non-whitespace)))
                  (unless (eqv? close rdelim)
                    (error "missing close paren: ~A" close))
                  tail)
                (cons expr (lp (next-non-whitespace)))))))))

    (define (hex-digit ch)
      (case ch
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (- (char->integer ch) (char->integer #\0)))
        ((#\a #\b #\c #\d #\e #\f)
         (+ 10 (- (char->integer ch) (char->integer #\a))))
        ((#\A #\B #\C #\D #\E #\F)
         (+ 10 (- (char->integer ch) (char->integer #\A))))
        (else #f)))

    (define (read-r6rs-hex-escape)
      (let ((ch (next)))
        (cond
         ((hex-digit ch) =>
          (lambda (res)
            (let lp ((res res))
              (let ((ch (next)))
                (cond
                 ((hex-digit ch) => (lambda (digit) (lp (+ (* 16 res) digit))))
                 ((eqv? ch #\;) (integer->char res))
                 ((eof-object? ch)
                  (error "unexpected end of input in character escape sequence"))
                 (else
                  (error "invalid character in escape sequence: ~S" ch)))))))
         ((eof-object? ch)
          (error "unexpected end of input in character escape sequence"))
         (else
          (error "invalid character in escape sequence: ~S" ch)))))

    (define (read-fixed-hex-escape len)
      (let lp ((len len) (res 0))
        (if (zero? len)
            (integer->char res)
            (let ((ch (next)))
              (cond
               ((hex-digit ch) =>
                (lambda (digit)
                  (lp (1- len) (+ (* res 16) digit))))
               ((eof-object? ch)
                (error "unexpected end of input in character escape sequence"))
               (else
                (error "invalid character in escape sequence: ~S" ch)))))))

    (define (char-intraline-whitespace? ch)
      ;; True for tab and for codepoints whose general category is Zs.
      (case ch
        ((#\tab #\space
          #\240 #\13200
          #\20000 #\20001 #\20002 #\20003 #\20004 #\20005
          #\20006 #\20007 #\20010 #\20011 #\20012
          #\20057
          #\20137
          #\30000) #t)
        (else #f)))

    (define (read-string rdelim)
      (let ((out (open-output-string)))
        (let lp ()
          (let ((ch (next)))
            (cond
             ((eof-object? ch)
              (error "unexpected end of input while reading string"))
             ((eqv? ch rdelim)
              (get-output-string out))
             ((eqv? ch #\\)
              (let ((ch (next)))
                (when (eof-object? ch)
                  (error "unexpected end of input while reading string"))
                (cond
                 ((eqv? ch #\newline)
                  ;; Skip intraline whitespace before continuing.
                  (let skip ()
                    (let ((ch (peek)))
                      (when (and (not (eof-object? ch))
                                 (char-intraline-whitespace? ch))
                        (next)
                        (skip))))
                  (lp))
                 ((eqv? ch rdelim)
                  (write-char rdelim out)
                  (lp))
                 (else
                  (write-char
                   (case ch
                     ;; Accept "\(" for use at the beginning of
                     ;; lines in multiline strings to avoid
                     ;; confusing emacs lisp modes.
                     ((#\| #\\ #\() ch)
                     ((#\0) #\nul)
                     ((#\f) #\ff)
                     ((#\n) #\newline)
                     ((#\r) #\return)
                     ((#\t) #\tab)
                     ((#\a) #\alarm)
                     ((#\v) #\vtab)
                     ((#\b) #\backspace)
                     ;; When faced with the choice between Guile's old
                     ;; two-char \xHH escapes and R6RS \xHHH...;
                     ;; escapes, prefer R6RS; \xHH is of limited
                     ;; utility.
                     ((#\x) (read-r6rs-hex-escape))
                     ((#\u) (read-fixed-hex-escape 4))
                     ((#\U) (read-fixed-hex-escape 6))
                     (else
                      (error "invalid character in escape sequence: ~S" ch)))
                   out)
                  (lp)))))
             (else
              (write-char ch out)
              (lp)))))))

    (define (read-character)
      (let ((ch (next)))
        (cond
         ((eof-object? ch)
          (error "unexpected end of input after #\\"))
         ((delimiter? ch)
          ch)
         (else
          (let* ((tok (read-token ch))
                 (len (string-length tok)))
            (define dotted-circle #\x25cc)
            (define r5rs-charnames
              '(("space" . #\x20) ("newline" . #\x0a)))
            (define r6rs-charnames
              '(("nul" . #\x00) ("alarm" . #\x07) ("backspace" . #\x08)
                ("tab" . #\x09) ("linefeed" . #\x0a) ("vtab" . #\x0b)
                ("page" . #\x0c) ("return" . #\x0d) ("esc" . #\x1b)
                ("delete" . #\x7f)))
            (define r7rs-charnames
              '(("escape" . #\x1b)))
            (define C0-control-charnames
              '(("nul" . #\x00) ("soh" . #\x01) ("stx" . #\x02)
                ("etx" . #\x03) ("eot" . #\x04) ("enq" . #\x05)
                ("ack" . #\x06) ("bel" . #\x07) ("bs"  . #\x08)
                ("ht"  . #\x09) ("lf"  . #\x0a) ("vt"  . #\x0b)
                ("ff"  . #\x0c) ("cr"  . #\x0d) ("so"  . #\x0e)
                ("si"  . #\x0f) ("dle" . #\x10) ("dc1" . #\x11)
                ("dc2" . #\x12) ("dc3" . #\x13) ("dc4" . #\x14)
                ("nak" . #\x15) ("syn" . #\x16) ("etb" . #\x17)
                ("can" . #\x18) ("em"  . #\x19) ("sub" . #\x1a)
                ("esc" . #\x1b) ("fs"  . #\x1c) ("gs"  . #\x1d)
                ("rs"  . #\x1e) ("us"  . #\x1f) ("sp"  . #\x20)
                ("del" . #\x7f)))
            (define alt-charnames
              '(("null" . #\x0) ("nl" . #\x0a) ("np" . #\x0c)))
            ;; Although R6RS and R7RS charnames specified as being
            ;; case-sensitive, Guile matches them case-insensitively, like
            ;; other char names.
            (define (named-char tok alist)
              (let ((tok (string-downcase tok)))
                (let lp ((alist alist))
                  (match alist
                    (() #f)
                    (((name . ch) . alist)
                     (if (string=? name tok) ch (lp alist)))))))
            (cond
             ((= len 1) ch)
             ((and (= len 2) (eqv? (string-ref tok 1) dotted-circle))
              ;; Ignore dotted circles, which may be used to keep
              ;; combining characters from combining with the backslash in
              ;; #\charname.
              ch)
             ((and (<= (char->integer #\0) (char->integer ch) (char->integer #\7))
                   (string->number tok 8))
              ;; Specifying a codepoint as an octal value.
              => integer->char)
             ((and (eqv? ch #\x) (> len 1)
                   (string->number (string-copy tok 1) 16))
              ;; Specifying a codepoint as an hexadecimal value.  Skip
              ;; initial "x".
              => integer->char)
             ((named-char tok r5rs-charnames))
             ((named-char tok r6rs-charnames))
             ((named-char tok r7rs-charnames))
             ((named-char tok C0-control-charnames))
             ((named-char tok alt-charnames))
             (else
              (error "unknown character name ~a" tok))))))))

    (define (read-vector)
      (list->vector (map strip-annotation (read-parenthesized #\)))))

    (define (read-bytevector)
      (define (expect ch)
        (unless (eqv? (next) ch)
          (error "invalid bytevector prefix" ch)))
      (expect #\u)
      (expect #\8)
      (expect #\()
      (let ((p (open-output-bytevector)))
        (for-each (lambda (datum) (write-u8 (strip-annotation datum) p))
                  (read-parenthesized #\)))
        (get-output-bytevector p)))

    ;; FIXME: We should require a terminating delimiter.
    (define (read-bitvector)
      (let lp ((bits '()) (len 0))
        (let ((ch (peek)))
          (case ch
            ((#\0) (next) (lp bits (1+ len)))
            ((#\1) (next) (lp (cons len bits) (1+ len)))
            (else
             (let ((bv (make-bitvector len #f)))
               (for-each (lambda (bit) (bitvector-set-bit! bv bit)) bits)
               bv))))))

    (define (read-true)
      (match (peek)
        ((or (? eof-object?) (? delimiter?))
         #t)
        (_ (match (read-token #\t)
             ((? (lambda (tok) (string=? (string-downcase tok) "true"))) #t)
             (tok (error "unexpected input when reading #true" tok))))))
    (define (read-false)
      (match (peek)
        ((or (? eof-object?) (? delimiter?))
         #f)
        (_ (match (string-downcase (read-token #\f))
             ((? (lambda (tok) (string=? (string-downcase tok) "false"))) #f)
             (tok (error "unexpected input when reading #false" tok))))))

    (define (read-keyword)
      (let ((expr (strip-annotation (read-subexpression "keyword"))))
        (unless (symbol? expr)
          (error "keyword prefix #: not followed by a symbol: ~a" expr))
        (symbol->keyword expr)))

    (define (read-number-and-radix ch)
      (let ((tok (string-append "#" (read-token ch))))
        (or (string->number tok)
            (error "unknown # object: ~S" tok))))

    (define (read-extended-symbol)
      (define (next-not-eof)
        (let ((ch (next)))
          (when (eof-object? ch)
            (error "end of input while reading symbol"))
          ch))
      (let ((out (open-output-string)))
        (let lp ((saw-brace? #f))
          (let lp/inner ((ch (next-not-eof))
                         (saw-brace? saw-brace?))
            (cond
             (saw-brace?
              (unless (eqv? ch #\#)
                ;; Don't eat CH, see
                ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49623>.
                (write-char #\} out)
                (lp/inner ch #f)))
             ((eqv? ch #\})
              (lp #t))
             ((eqv? ch #\\)
              ;; \xH+; => R6RS hex escape
              ;; \C => C otherwise, for any C
              (let* ((ch (next-not-eof))
                     (ch (if (eqv? ch #\x)
                             (read-r6rs-hex-escape)
                             ch)))
                (write-char ch out)
                (lp #f)))
             (else
              (write-char ch out)
              (lp #f)))))
        (string->symbol (get-output-string out))))

    (define (read-nil)
      ;; Have already read "#\n" -- now read "il".
      (match (read-mixed-case-symbol #\n)
        ('nil #nil)
        (id (error "unexpected input while reading #nil: ~a" id))))

    (define (read-sharp)
      (let* ((ch (next)))
        (cond
         ((eof-object? ch)
          (error "unexpected end of input after #"))
         (else
          (case ch
            ((#\\) (read-character))
            ((#\() (read-vector))
            ((#\v) (read-bytevector))
            ((#\*) (read-bitvector))
            ((#\f #\F) (read-false))
            ((#\t #\T) (read-true))
            ((#\:) (read-keyword))
            ((#\i #\e #\b #\B #\o #\O #\d #\D #\x #\X #\I #\E)
             (read-number-and-radix ch))
            ((#\{) (read-extended-symbol))
            ((#\') (list 'syntax (read-subexpression "syntax expression")))
            ((#\`) (list 'quasisyntax
                         (read-subexpression "quasisyntax expression")))
            ((#\,)
             (if (eqv? #\@ (peek))
                 (begin
                   (next)
                   (list 'unsyntax-splicing
                         (read-subexpression "unsyntax-splicing expression")))
                 (list 'unsyntax (read-subexpression "unsyntax expression"))))
            ((#\n) (read-nil))
            (else
             (error "Unknown # object: ~S" (string #\# ch))))))))

    (define (read-number ch)
      (let ((str (read-token ch)))
        (or (string->number str)
            (string->symbol (if fold-case? (string-downcase str) str)))))

    (define (read-expr* ch)
      (case ch
        ((#\[) (read-parenthesized #\]))
        ((#\() (read-parenthesized #\)))
        ((#\") (read-string ch))
        ((#\|) (string->symbol (read-string ch)))
        ((#\') (list 'quote (read-subexpression "quoted expression")))
        ((#\`) (list 'quasiquote (read-subexpression "quasiquoted expression")))
        ((#\,) (cond
                ((eqv? #\@ (peek))
                 (next)
                 (list 'unquote-splicing (read-subexpression "subexpression of ,@")))
                (else
                 (list 'unquote (read-subexpression "unquoted expression")))))
        ;; FIXME: read-sharp should recur if we read a comment
        ((#\#) (read-sharp))
        ((#\)) (error "unexpected \")\""))
        ((#\]) (error "unexpected \"]\""))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\.) (read-number ch))
        (else (read-mixed-case-symbol ch))))

    (define (read-expr ch)
      (annotate (port-line port)
                (port-column port)
                (read-expr* ch)))

    (define (read-directive)
      (define (directive-char? ch)
        (and (char? ch)
             (or (eqv? ch #\-)
                 (char-alphabetic? ch)
                 (char-numeric? ch))))
      (let ((ch (peek)))
        (cond
         ((directive-char? ch)
          (next)
          (string->symbol (take-while ch directive-char?)))
         (else
          #f))))

    (define (skip-scsh-comment)
      (let lp ((ch (next)))
        (cond
         ((eof-object? ch)
          (error "unterminated `#! ... !#' comment"))
         ((eqv? ch #\!)
          (let ((ch (next)))
            (if (eqv? ch #\#)
                (next)
                (lp ch))))
         (else
          (lp (next))))))

    (define (process-shebang)
      ;; After having read #!, we complete either with #!r6rs,
      ;; #!fold-case, #!no-fold-case, or a SCSH block comment terminated
      ;; by !#.
      (match (read-directive)
        ('fold-case
         (set-fold-case?! #t)
         (next))
        ((or 'no-fold-case 'r6rs)
         (set-fold-case?! #f)
         (next))
        (_
         (skip-scsh-comment))))

    (define (skip-eol-comment)
      (let ((ch (next)))
        (cond
         ((eof-object? ch) ch)
         ((eq? ch #\newline) (next))
         (else (skip-eol-comment)))))

    ;; Unlike SCSH-style block comments, SRFI-30/R6RS block comments may be
    ;; nested.
    (define (skip-r6rs-block-comment)
      ;; We have read #|, now looking for |#.
      (let ((ch (next)))
        (when (eof-object? ch)
          (error "unterminated `#| ... |#' comment"))
        (cond
         ((and (eqv? ch #\|) (eqv? (peek) #\#))
          ;; Done.
          (next)
          (values))
         ((and (eqv? ch #\#) (eqv? (peek) #\|))
          ;; A nested comment.
          (next)
          (skip-r6rs-block-comment)
          (skip-r6rs-block-comment))
         (else
          (skip-r6rs-block-comment)))))

    (define (read-subexpression what)
      (let ((ch (next-non-whitespace)))
        (when (eof-object? ch)
          (error (string-append "unexpected end of input while reading " what)))
        (read-expr ch)))

    (define (next-non-whitespace)
      (let lp ((ch (next)))
        (case ch
          ((#\;)
           (lp (skip-eol-comment)))
          ((#\#)
           (case (peek)
             ((#\!)
              (next)
              (lp (process-shebang)))
             ((#\;)
              (next)
              (read-subexpression "#; comment")
              (next-non-whitespace))
             ((#\|)
              (next)
              (skip-r6rs-block-comment)
              (next-non-whitespace))
             (else ch)))
          ((#\space #\return #\ff #\newline #\tab)
           (next-non-whitespace))
          (else ch))))

    (let ((ch (next-non-whitespace)))
      (if (eof-object? ch)
          ch
          (read-expr ch))))

  (define* (read #:optional (port (current-input-port)))
    ;; For read-syntax, we'd define these annotate / strip functions
    ;; differently, to create syntax objects instead.
    (define (annotate line column datum) datum)
    (define (strip-annotation datum) datum)
    (%read port annotate strip-annotation))

  (define* (read-syntax #:optional (port (current-input-port)))
    (define filename (port-filename port))
    (define (annotate line column datum)
      ;; Usually when reading compound expressions consisting of multiple
      ;; syntax objects, like lists, the "leaves" of the expression are
      ;; annotated but the "root" isn't.  Like in (A . B), A and B will be
      ;; annotated but the pair won't.  Therefore the usually correct
      ;; thing to do is to just annotate the result.  However in the case
      ;; of reading ( . C), the result is the already annotated C, which
      ;; we don't want to re-annotate.  Therefore we avoid re-annotating
      ;; already annotated objects.
      (if (syntax? datum)
          datum
          (datum->syntax #f             ; No lexical context.
                         datum
                         #:source (vector filename line (1- column)))))
    (%read port annotate syntax->datum)))
