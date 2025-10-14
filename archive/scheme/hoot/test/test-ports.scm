;;; Copyright (C) 2023, 2024, 2025y Igalia, S.L.
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
;;; Port tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-ports")

(test-call "#vu8(100 120)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-u8 100 p)
               (write-u8 120 p)
               (get-output-bytevector p))))

(test-call "#vu8(100 120 130 140)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-bytevector #vu8(100 120) p)
               (write-bytevector #vu8(130 140) p)
               (get-output-bytevector p))))

(test-call "#vu8(104 101 108 108 111 44 32 119 111 114 108 100)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-string "hello, world" p)
               (get-output-bytevector p))))

(test-call "#(1 1 2 3 #<eof> #<eof> #<eof>)"
           (lambda ()
             (let* ((p (open-input-bytevector #vu8(1 2 3)))
                    (a (peek-u8 p))
                    (b (read-u8 p))
                    (c (read-u8 p))
                    (d (read-u8 p))
                    (e (read-u8 p))
                    (f (peek-u8 p))
                    (g (read-u8 p)))
               (vector a b c d e f g))))

(test-call "#(#vu8() #vu8(1) #vu8(1 2) #vu8(1 2 3) #vu8(1 2 3))"
           (lambda ()
             (define (read-n n)
               (read-bytevector n (open-input-bytevector #vu8(1 2 3))))
             (vector (read-n 0)
                     (read-n 1)
                     (read-n 2)
                     (read-n 3)
                     (read-n 4))))

(test-call "#<eof>"
           (lambda ()
             (read-bytevector 1 (open-input-bytevector #vu8()))))

(test-call "#(#\\h #\\h #\\e #\\l #\\l #\\o #<eof> #<eof> #<eof>)"
           (lambda ()
             (let* ((p (open-input-bytevector #vu8(104 101 108 108 111)))
                    (a (peek-char p))
                    (b (read-char p))
                    (c (read-char p))
                    (d (read-char p))
                    (e (read-char p))
                    (f (read-char p))
                    (g (read-char p))
                    (h (peek-char p))
                    (i (read-char p)))
               (vector a b c d e f g h i))))

(test-call "#(\"\" \"h\" \"he\" \"hel\" \"hell\" \"hello\" \"hello\")"
           (lambda ()
             (define (read-n n)
               (read-string n (open-input-bytevector #vu8(104 101 108 108 111))))
             (vector (read-n 0)
                     (read-n 1)
                     (read-n 2)
                     (read-n 3)
                     (read-n 4)
                     (read-n 5)
                     (read-n 6))))

(with-additional-imports ((only (hoot numbers) 1+))
  (test-call "#(43 43 70 #(101 101 421) 70)"
             (lambda ()
               (let* ((p (make-parameter 42 1+))
                      (a (p))
                      (b (p 69))
                      (c (p))
                      (d (parameterize ((p 100))
                           (let* ((a (p))
                                  (b (p 420))
                                  (c (p)))
                             (vector a b c))))
                      (e (p)))
                 (vector a b c d e)))))

(test-call "#(\"foo\" \"bar\" \"baz\" \"asdfa\" #<eof> #<eof>)"
           (lambda ()
             (let* ((p (open-input-string "foo\nbar\r\nbaz\rasdfa"))
                             (a (read-line p))
                             (b (read-line p))
                             (c (read-line p))
                             (d (read-line p))
                             (e (read-line p))
                             (f (read-line p)))
                        (vector a b c d e f))))

(with-additional-imports ((only (hoot ports) port-line port-column)
                          (only (hoot syntax) syntax-case with-syntax
                                syntax generate-temporaries))
  (test-call "#(#((0 . 0) \"foo\" (1 . 0)) #((1 . 0) #\\b (1 . 1)) #((1 . 1) #\\a (1 . 2)) #((1 . 2) #\\r (1 . 3)) #((1 . 3) #\\return (1 . 0)) #((1 . 0) #\\newline (2 . 0)) #((2 . 0) \"baz\" (3 . 0)) #((3 . 0) \"as\" (3 . 2)) #((3 . 2) \"df\" (3 . 4)) #((3 . 4) \"a\" (3 . 5)))"
             (lambda ()
               (define p (open-input-string "foo\nbar\r\nbaz\rasdfa"))
               (define (pos)
                 (cons (port-line p) (port-column p)))
               (define-syntax <<
                 (lambda (stx)
                   (syntax-case stx ()
                     ((_ exp ...)
                      (with-syntax (((t ...) (generate-temporaries #'(exp ...))))
                        #'(let* ((t (let* ((before (pos))
                                           (val exp)
                                           (after (pos)))
                                      (vector before val after)))
                                 ...)
                            (vector t ...)))))))
               (<< (read-line p)
                   (read-char p)
                   (read-char p)
                   (read-char p)
                   (read-char p)
                   (read-char p)
                   (read-line p)
                   (read-string 2 p)
                   (read-string 2 p)
                   (read-line p)))))

;; Apologies for the wall of text, but this tests that input that
;; exceeds the default buffer size (1024) comes through correctly.
(test-call "\"This paper would not have happened if Sussman had not been forced to\\nthink about lambda calculus by having to teach 6.031, not would it\\nhave happened had not Steele been forced to understand PLASMA by\\nmorbid curiosity.\\n\\nThis work developed out of an initial attempt to understand the\\nactorness of actors.  Steele thought he understood it, but couldn't\\nexplain it; Sussamn suggested the experimental approach of actually\\nbuilding an \\\"ACTORS interpreter\\\".  This interpreter attempted to\\nintermix the user of actors and LISP lambda expressions in a clean\\nmanner.  When it was completed, we discovered that the \\\"actors\\\" and\\nthe lambda expressions were identical in implementation.  Once we had\\ndiscovered this, all the rest fell into place, and it was only natural\\nto begin thinking about actors in terms of lambda calculus.  The\\noriginal interpreter was call-by-name for various reasons having to do\\nwith 6.031; we subsequently experimentally discovered how call-by-name\\nscrews iteration, and rewrote it to use call-by-value.  Note well that\\nwe did not bring forth a clean implementation in one brilliant flash\\nof understanding; we used an experimental and highly empirical\\napproach to bootstrap our knowledge.\""
           (lambda ()
             (let ((p (open-input-string
                       "This paper would not have happened if Sussman had not been forced to
think about lambda calculus by having to teach 6.031, not would it
have happened had not Steele been forced to understand PLASMA by
morbid curiosity.

This work developed out of an initial attempt to understand the
actorness of actors.  Steele thought he understood it, but couldn't
explain it; Sussamn suggested the experimental approach of actually
building an \"ACTORS interpreter\".  This interpreter attempted to
intermix the user of actors and LISP lambda expressions in a clean
manner.  When it was completed, we discovered that the \"actors\" and
the lambda expressions were identical in implementation.  Once we had
discovered this, all the rest fell into place, and it was only natural
to begin thinking about actors in terms of lambda calculus.  The
original interpreter was call-by-name for various reasons having to do
with 6.031; we subsequently experimentally discovered how call-by-name
screws iteration, and rewrote it to use call-by-value.  Note well that
we did not bring forth a clean implementation in one brilliant flash
of understanding; we used an experimental and highly empirical
approach to bootstrap our knowledge.")))
               (list->string
                (let lp ((char (read-char p)))
                  (if (eof-object? char)
                      '()
                      (cons char (lp (read-char p)))))))))

(test-call "#f"
           (lambda (str)
             (let ((port (open-input-string str)))
               (call-with-port port read-char)
               (input-port-open? port)))
           "foo")

;; We cannot test file ports against d8 because it lacks a sufficient
;; filesystem API.
(define input-fixture
 (in-vicinity (getenv "HOOT_TEST_DATA_DIR") "fixtures/hello"))

(parameterize ((use-d8? #f))
  (with-additional-imports ((ice-9 match)
                            (scheme read)
                            (scheme file))
    (test-call
     "(hello and welcome back to scheme)"
     (lambda ()
       (call-with-input-file ,input-fixture
         (lambda (port)
           (let loop ()
             (match (read port)
               ((? eof-object?) '())
               (x (cons x (loop))))))))))

  (with-additional-imports
      ((only (hoot ports) seek)
       (scheme read)
       (scheme file))
    (test-call
     "welcome"
     (lambda ()
       (call-with-input-file ,input-fixture
         (lambda (port)
           (seek port 10 'cur)
           (read port))))))

  ;; Not guaranteed to be a unique name, but 'mkstemp' opens a port
  ;; which we don't want since we need Hoot to open the port.
  (let ((tmp "/tmp/tmp-hoot-port-test"))
    (define-syntax-rule (test-output-file expected expr)
      (unwind-protect
       (lambda ()
         (test-call expected expr))
       (lambda ()
         (false-if-exception
          (delete-file tmp)))))

    (with-additional-imports ((scheme file))
      (test-output-file
       "#t"
       (lambda ()
         (call-with-output-file ,tmp (lambda (port) #t))
         (file-exists? ,tmp)))

      (test-output-file
       "deleted"
       (lambda ()
         (call-with-output-file ,tmp (lambda (port) #t))
         (and (file-exists? ,tmp)
              (begin
                (delete-file ,tmp)
                (file-exists? ,tmp)
                'deleted))))

      (with-additional-imports ((scheme write)
                                (scheme read))
        (test-output-file
         "HELLO"
         (lambda ()
           (call-with-output-file ,tmp
             (lambda (port)
               (write 'HELLO port)))
           (call-with-input-file ,tmp read)))))))

(test-end* "test-ports")
