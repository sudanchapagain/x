;;; Ports
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
;;; Ports.
;;;
;;; Code:

(library (hoot ports)
  (export %port-fold-case?
          %set-port-fold-case?!

          make-port
          port-filename
          port-line
          port-column
          port-encoding
          port-conversion-strategy
          set-port-encoding!
          set-port-conversion-strategy!
          (rename %port-read-buffering port-read-buffering)
          get-output-bytevector
          open-output-bytevector
          open-input-bytevector

          open-input-string
          open-output-string
          get-output-string

          ;; R7RS ports
          eof-object?
          eof-object
          port?
          input-port?
          output-port?
          binary-port?
          textual-port?
          port-open?
          input-port-open?
          output-port-open?
          close-input-port
          close-output-port
          close-port
          call-with-port
          seek
          flush-input-port
          flush-output-port
          u8-ready?
          peek-u8
          read-u8
          read-bytevector
          read-bytevector!
          char-ready?
          peek-char
          read-char
          read-string
          read-line
          write-u8
          write-bytevector
          write-char
          newline
          write-string

          standard-input-port
          standard-output-port
          standard-error-port

          current-input-port
          current-output-port
          current-error-port)
  (import (only (hoot primitives)
                %eof-object? guile:make-void-port
                %the-eof-object)
          (hoot apply)
          (hoot bitwise)
          (hoot boxes)
          (hoot bytevectors)
          (hoot char)
          (hoot cond-expand)
          (hoot eq)
          (hoot errors)
          (hoot inline-wasm)
          (hoot lists)
          (hoot match)
          (hoot not)
          (hoot numbers)
          (hoot pairs)
          (hoot parameters)
          (hoot procedures)
          (hoot strings)
          (hoot syntax)
          (hoot values)
          (hoot vectors))

  (define-syntax define-low-level-port-constructor-and-accessors
    (lambda (stx)
      (define (iota n)
        (let lp ((i 0))
          (if (= i n)
              '()
              (cons i (lp (1+ i))))))
      (syntax-case stx ()
        ((_ constructor pred (field wasm-type . accessors) ...)
         (with-syntax (((idx ...) (iota (length #'(field ...)))))
           #'(begin
               (define-low-level-port-constructor constructor
                 (field wasm-type) ...)
               (define-low-level-port-predicate pred field ...)
               (define-low-level-port-accessors field idx . accessors)
               ...))))))
  (define-syntax-rule (define-low-level-port-constructor constructor
                        (field wasm-type) ...)
    (define (constructor field ...)
      (cond-expand
       (guile-vm
        ;; Use a tagged vector to represent ports at expansion time,
        ;; since (hoot records) depends upon this module.
        (vector '<port> field ...))
       (hoot
        (%inline-wasm
         '(func (param field wasm-type) ... (result (ref eq))
                (struct.new $port (i32.const 0) (local.get field) ...))
         field ...)))))
  (define-syntax-rule (define-low-level-port-predicate pred field ...)
    (define (pred obj)
      (cond-expand
       (guile-vm
        (and (vector? obj)
             (eq? (vector-length obj) (1+ (length '(field ...))))
             (eq? (vector-ref obj 0) '<port>)))
       (hoot
        (%inline-wasm
         '(func (param $obj (ref eq))
                (result (ref eq))
                (if (ref eq)
                    (ref.test $port (local.get $obj))
                    (then (ref.i31 (i32.const 17)))
                    (else (ref.i31 (i32.const 1)))))
         obj)))))
  (define-syntax define-low-level-port-accessors
    (syntax-rules ()
      ((_ field idx getter)
       (define-low-level-port-getter getter field idx))
      ((_ field idx getter setter)
       (begin
         (define-low-level-port-getter getter field idx)
         (define-low-level-port-setter setter field idx)))))
  (define-syntax-rule (define-low-level-port-getter getter field idx)
    (define (getter port)
      (cond-expand
       (guile-vm
        (vector-ref port (1+ idx)))
       (hoot
        (%inline-wasm
         '(func (param $port (ref $port)) (result (ref eq))
                (struct.get $port field (local.get $port)))
         port)))))
  (define-syntax-rule (define-low-level-port-setter setter field idx)
    (define (setter port val)
      (cond-expand
       (guile-vm
        (vector-set! port (1+ idx) val))
       (hoot
        (%inline-wasm
         '(func (param $port (ref $port)) (param $val (ref eq))
                (struct.set $port field (local.get $port) (local.get $val)))
         port val)))))
  (define-low-level-port-constructor-and-accessors
    %make-port
    port?
    ($open? (ref eq) %port-open? %set-port-open?!)
    ($read (ref eq) %port-read)
    ($write (ref eq) %port-write)
    ($input-waiting? (ref eq) %port-input-waiting?)
    ($seek (ref eq) %port-seek)
    ($close (ref eq) %port-close)
    ($truncate (ref eq) %port-truncate)
    ($repr (ref $string) %port-repr)
    ($filename (ref eq) port-filename %set-port-filename!)
    ($position (ref $mutable-pair) %port-position)
    ($read-buf (ref eq) %port-read-buffer %set-port-read-buffer!)
    ($write-buf (ref eq) %port-write-buffer %set-port-write-buffer!)
    ($read-buffering (ref eq) %port-read-buffering %set-port-read-buffering!)
    ($r/w-random-access? (ref eq) %port-r/w-random-access?)
    ($fold-case? (ref eq) %port-fold-case? %set-port-fold-case?!)
    ($private-data (ref eq) %port-private-data))

  (define %default-buffer-size 1024)

  ;; FIXME: suspendability
  (define* (make-port #:key
                      read
                      write
                      input-waiting?
                      seek
                      close
                      truncate
                      repr
                      file-name
                      (read-buffer-size %default-buffer-size)
                      (write-buffer-size %default-buffer-size)
                      r/w-random-access?
                      fold-case?
                      private-data)
    (when file-name (check-type file-name string? 'make-port))
    (let ((read-buf (and read (vector (make-bytevector read-buffer-size 0) 0 0 #f)))
          (write-buf (and write (vector (make-bytevector write-buffer-size 0) 0 0))))
      (%make-port #t
                  read
                  write
                  input-waiting?
                  seek
                  close
                  truncate
                  repr
                  file-name
                  (cons 0 0)
                  read-buf
                  write-buf
                  read-buffer-size
                  r/w-random-access?
                  fold-case?
                  private-data)))

  (define (%set-port-buffer-cur! buf cur)           (vector-set! buf 1 cur))
  (define (%set-port-buffer-end! buf end)           (vector-set! buf 2 end))
  (define (%set-port-buffer-has-eof?! buf has-eof?) (vector-set! buf 3 has-eof?))

  (define (port-line port)
    (check-type port port? 'port-line)
    (car (%port-position port)))
  (define (port-column port)
    (check-type port port? 'port-column)
    (cdr (%port-position port)))

  ;; FIXME: These are stubs.
  (define (port-encoding port) "UTF-8")
  (define (port-conversion-strategy port) 'substitute)
  (define (set-port-encoding! port encoding) (values))
  (define (set-port-conversion-strategy! port strategy) (values))

  (define* (get-output-bytevector port #:optional (clear-buffer? #f))
    ;; FIXME: How to know it's a bytevector output port?
    (check-type port output-port? 'get-output-bytevector)
    (define accum (%port-private-data port))
    (flush-output-port port)
    (let ((flattened (bytevector-concatenate-reverse (box-ref accum))))
      (box-set! accum (if clear-buffer?
                          '()
                          (list flattened)))
      flattened))

  (define (open-output-bytevector)
    (define accum (make-box '()))
    (define pos #f)
    (define (appending?) (not pos))
    (define (bv-write bv start count)   ; write
      (unless (zero? count)
        (cond
         ((appending?)
          (box-set! accum
                    (cons (bytevector-copy bv start (+ start count))
                          (box-ref accum))))
         (else
          (let* ((dst (get-output-bytevector port))
                 (to-copy (min count (- (bytevector-length dst) pos))))
            (bytevector-copy! dst pos bv start to-copy)
            (cond
             ((< to-copy count)
              (box-set!
               accum
               (list (bytevector-copy bv (+ start to-copy) (- count to-copy))
                     dst))
              (set! pos #f))
             (else
              (set! pos (+ pos count))))))))
      count)
    (define (bv-seek offset whence)     ; seek
      (define len (bytevector-length (get-output-bytevector port)))
      (define base (match whence ('start 0) ('cur (or pos len)) ('end len)))
      (define dst (+ base offset))
      (check-range dst 0 len 'seek)
      (set! pos (if (= pos dst) #f dst))
      dst)

    (define port
      (make-port #:write bv-write
                 #:seek bv-seek
                 #:r/w-random-access? #t
                 #:repr "bytevector"
                 #:private-data accum))
    port)

  (define (open-input-bytevector src)
    (check-type src bytevector? 'open-input-bytevector)
    (define pos 0)
    (define (bv-read dst start count)
      (let* ((to-copy (min count (- (bytevector-length src) pos)))
             (end (+ pos to-copy)))
        (bytevector-copy! dst start src pos end)
        (set! pos end)
        to-copy))
    (define (bv-seek offset whence)     ; seek
      (define len (bytevector-length src))
      (define base (match whence ('start 0) ('cur pos) ('end len)))
      (define dst (+ base offset))
      (check-range dst 0 len 'seek)
      (set! pos dst)
      dst)
    ;; FIXME: Can we just provide `src` directly as the read buffer?
    (make-port #:read bv-read
               #:seek bv-seek
               #:r/w-random-access? #t
               #:repr "bytevector"))

  ;; FIXME: kwargs
  (define (make-soft-port repr %read-string %write-string input-waiting? close)
    (check-type repr string? 'make-soft-port)
    (define (make-reader read-string)
      (define buffer #f)
      (define buffer-pos 0)
      (lambda (bv start count)
        (unless (and buffer (< buffer-pos (bytevector-length buffer)))
          (let* ((str (%read-string)))
            (set! buffer (string->utf8 str))
            (set! buffer-pos 0)))

        (let* ((to-copy (min count (- (bytevector-length buffer) buffer-pos)))
               (next-pos (+ buffer-pos to-copy)))
          (bytevector-copy! bv start buffer buffer-pos next-pos)
          (if (= (bytevector-length buffer) next-pos)
              (set! buffer #f)
              (set! buffer-pos next-pos))
          to-copy)))

    (define (make-writer write-string)
      (lambda (bv start count)
        ;; FIXME: If the writer is binary, that could split a codepoint in
        ;; two, resulting in badness.  Shouldn't happen with textual
        ;; writers but it's worth noting.
        (%write-string (utf8->string bv start (+ start count)))
        count))

    (make-port #:read (and read-string (make-reader read-string))
               #:write (and write-string (make-writer write-string))
               #:input-waiting? input-waiting?
               #:repr repr))

  (define (open-input-string str)
    (open-input-bytevector (string->utf8 str)))

  (define (open-output-string) (open-output-bytevector))
  (define* (get-output-string p #:optional (clear-buffer? #f))
    (utf8->string (get-output-bytevector p clear-buffer?)))

  ;; R7RS ports
  (define (eof-object? x) (%eof-object? x))
  (define (eof-object)
    (define-syntax eof-object
      (lambda (stx) #`'#,%the-eof-object))
    (eof-object))

  (define (input-port? x) (and (port? x) (%port-read x) #t))
  (define (output-port? x) (and (port? x) (%port-write x) #t))
  (define (binary-port? x) (port? x))
  (define (textual-port? x) (port? x))
  (define (port-open? x)
    (check-type x port? 'port-open?)
    (%port-open? x))
  (define (input-port-open? x)
    (check-type x input-port? 'input-port-open?)
    (%port-open? x))
  (define (output-port-open? x)
    (check-type x output-port? 'output-port-open?)
    (%port-open? x))
  (define (close-input-port port)
    (check-type port input-port? 'close-input-port)
    ;; FIXME: Allow half-closing of socket-like ports.
    (close-port port))
  (define (close-output-port port)
    (check-type port output-port? 'close-output-port)
    ;; FIXME: Allow half-closing of socket-like ports.
    (close-port port))
  (define (close-port port)
    (check-type port port? 'close-port)
    (when (%port-open? port)
      (when (output-port? port) (flush-output-port port))
      (%set-port-open?! port #f)
      (match (%port-close port)
        (#f #f)
        (close (close))))
    (values))
  (define (call-with-port port proc)
    (check-type port port? 'call-with-port)
    (check-type proc procedure? 'call-with-port)
    (call-with-values (lambda () (proc port))
      (lambda vals
        (close-port port)
        (apply values vals))))

  (define (seek port offset whence)
    (check-type port port? 'seek)
    (check-type offset exact-integer? 'seek)
    (assert (case whence ((cur start end) #t) (else #f)) 'seek)
    (define (buffered-bytes buf)
      (define (port-buffer-cur buf) (vector-ref buf 1))
      (define (port-buffer-end buf) (vector-ref buf 2))
      (if (vector? buf)
          (- (port-buffer-end buf) (port-buffer-cur buf))
          0))
    (cond
     ((%port-seek port)
      => (lambda (%seek)
           (cond
            ((and (eq? whence 'cur) (zero? offset))
             ;; Query current position, adjust for buffering without
             ;; flush.
             (let ((pos (%seek offset whence))
                   (buf-in (buffered-bytes (%port-read-buffer port)))
                   (buf-out (buffered-bytes (%port-write-buffer port))))
               (+ pos (- buf-in) buf-out)))
            ((not (%port-r/w-random-access? port))
             (raise (make-not-seekable-error port 'seek)))
            (else
             (%flush-input port)
             (%flush-output port)
             (let ((pos (%seek offset whence)))
               (when (input-port? port)
                 (%set-port-buffer-has-eof?! (%port-read-buffer port) #f))
               pos)))))
     (else (raise (make-not-seekable-error port 'seek)))))

  (define (%write-bytes port bv start count)
    (let ((written ((%port-write port) bv start count)))
      (check-range written 0 count '%write-bytes)
      (when (< written count)
        (%write-bytes port bv (+ start written) (- count written)))))

  (define (%read-bytes port bv start count)
    (let ((read ((%port-read port) bv start count)))
      (check-range read 0 count '%read-bytes)
      read))

  (define (%flush-input port)
    ;; For buffered input+output ports that are random-access?, it's
    ;; likely that when switching from reading to writing that we will
    ;; have some bytes waiting to be read, and that the underlying
    ;; port-position is ahead.  This function discards buffered input and
    ;; seeks back from before the buffered input.
    (match (%port-read-buffer port)
      (#f (values))
      ((and buf #(bv cur end has-eof?))
       (when (< cur end)
         (%set-port-buffer-cur! buf 0)
         (%set-port-buffer-end! buf 0)
         (seek port (- cur end) 'cur))
       (values))))

  (define* (flush-input-port #:optional (port (current-output-port)))
    ;; For buffered input+output ports that are random-access?, it's
    ;; likely that when switching from reading to writing that we will
    ;; have some bytes waiting to be read, and that the underlying
    ;; port-position is ahead.  This function discards buffered input and
    ;; seeks back from before the buffered input.
    (check-type port input-port? 'flush-input-port)
    (%flush-input port))

  (define (%flush-output port)
    (match (%port-write-buffer port)
      (#f (values))
      ((and buf #(bv cur end))
       (when (< cur end)
         (%set-port-buffer-cur! buf 0)
         (%set-port-buffer-end! buf 0)
         (%write-bytes port bv cur (- end cur)))
       (values))))

  (define* (flush-output-port #:optional (port (current-output-port)))
    (check-type port output-port? 'flush-output-port)
    (%flush-output port))

  (define* (u8-ready? #:optional (port (current-input-port)))
    (check-type port port? 'u8-ready?)
    (match (%port-read-buffer port)
      (#f (raise (make-type-error port 'u8-ready? 'input-port?)))
      (#(bv cur end has-eof?)
       (or (< cur end)
           has-eof?
           (match (%port-input-waiting? port)
             (#f #t)
             (proc (proc)))))))

  (define (%fill-input port buf minimum-buffering)
    (match buf
      (#(bv cur end has-eof?)
       (let ((avail (- end cur)))
         (cond
          ((or has-eof?
               (<= minimum-buffering avail))
           (values buf avail))
          ((< (bytevector-length bv) minimum-buffering)
           (let* ((expanded (make-bytevector minimum-buffering 0))
                  (buf (vector expanded 0 (- end cur) #f)))
             (when (< cur end)
               (bytevector-copy! expanded 0 bv cur end))
             (%set-port-read-buffer! port buf)
             (%fill-input port buf minimum-buffering)))
          (else
           (when (< 0 cur)
             (%set-port-buffer-cur! buf 0))
           (cond
            ((not (zero? avail))
             ;; If there is buffered input, we know a random access port
             ;; has no buffered output.
             (bytevector-copy! bv 0 bv cur end))
            ((%port-r/w-random-access? port)
             (%flush-output port)))

           (let lp ((end avail))
             (let* ((must-read (- minimum-buffering end))
                    ;; precondition: read-buffering <= len(read-buffer)
                    ;; precondition: minimum-buffering <= len(read-buffer)
                    ;; precondition: end < minimum-buffering
                    (count (- (max (%port-read-buffering port)
                                   minimum-buffering)
                              end))
                    (read (%read-bytes port bv end count))
                    (end (+ end read)))
               (cond
                ((zero? read)
                 (%set-port-buffer-end! buf end)
                 (%set-port-buffer-has-eof?! buf #t)
                 (values buf end))
                ((< end minimum-buffering)
                 (lp end))
                (else
                 (%set-port-buffer-end! buf end)
                 (values buf end)))))))))))

  (define* (peek-u8 #:optional (port (current-input-port)))
    (check-type port port? 'peek-u8)
    (let lp ((buf (%port-read-buffer port)))
      (match buf
        (#f (raise (make-type-error port 'peek-u8 'input-port?)))
        (#(bv cur end has-eof?)
         (cond
          ((eq? cur end)
           (if has-eof?
               (eof-object)
               (call-with-values (lambda ()
                                   (%fill-input port buf 1))
                 (lambda (buf avail)
                   (if (zero? avail)
                       (eof-object)
                       (lp buf))))))
          (else
           (bytevector-u8-ref bv cur)))))))

  (define* (read-u8 #:optional (port (current-input-port)))
    (check-type port port? 'read-u8)
    (define (read-eof! buf)
      (%set-port-buffer-has-eof?! buf #f)
      (eof-object))
    (let lp ((buf (%port-read-buffer port)))
      (match buf
        (#f (raise (make-type-error port 'read-u8 'input-port?)))
        (#(bv cur end has-eof?)
         (cond
          ((eq? cur end)
           (if has-eof?
               (read-eof! buf)
               (call-with-values (lambda ()
                                   (%fill-input port buf 1))
                 (lambda (buf avail)
                   (if (zero? avail)
                       (read-eof! buf)
                       (lp buf))))))
          (else
           (%set-port-buffer-cur! buf (1+ cur))
           (bytevector-u8-ref bv cur)))))))

  (define* (read-bytevector k #:optional (port (current-input-port)))
    (check-range k 0 (1- (ash 1 29)) 'read-bytevector)
    (check-type port input-port? 'read-bytevector)
    (call-with-values (lambda ()
                        (%fill-input port (%port-read-buffer port) (max k 1)))
      (lambda (buf avail)
        (cond
         ((zero? avail)
          (%set-port-buffer-has-eof?! buf #f)
          (eof-object))
         (else
          (match buf
            (#(src cur end has-eof?)
             (let* ((cur* (min (+ cur k) end))
                    (bv (bytevector-copy src cur cur*)))
               (%set-port-buffer-cur! buf cur*)
               bv))))))))

  (define* (read-bytevector! dst #:optional (port (current-input-port))
                             (start 0) (end (bytevector-length dst)))
    (check-type dst bytevector? 'read-bytevector!)
    (check-range start 0 (bytevector-length dst) 'read-bytevector!)
    (check-range end start (bytevector-length dst) 'read-bytevector!)
    (check-type port input-port? 'read-bytevector!)
    (let ((count (- start end)))
      (call-with-values (lambda ()
                          (%fill-input port (%port-read-buffer port)
                                       (max count 1)))
        (lambda (buf avail)
          (cond
           ((zero? avail)
            (%set-port-buffer-has-eof?! buf #f)
            (eof-object))
           (else
            (match buf
              (#(src cur end has-eof?)
               (let* ((cur* (min (+ cur count) end))
                      (count (- cur* cur)))
                 (bytevector-copy! dst start src cur cur*)
                 (%set-port-buffer-cur! buf cur*)
                 count)))))))))

  (define* (char-ready? #:optional (port (current-input-port)))
    (u8-ready? port))

  (define* (peek-char #:optional (port (current-input-port)))
    (let ((a (peek-u8 port)))
      (cond
       ((eof-object? a) a)
       ((< a #b10000000) (integer->char a))
       (else
        ;; FIXME: This is a sloppy UTF-8 decoder.  Need to think more
        ;; about this.
        (let ((len (cond ((< a #b11100000) 2)
                         ((< a #b11110000) 3)
                         (else 4))))
          (call-with-values (lambda ()
                              (%fill-input port (%port-read-buffer port) len))
            (lambda (buf avail)
              (when (< avail len)
                (error "decoding error: partial utf-8 sequence"))
              (match buf
                (#(bv cur end has-eof?)
                 (integer->char
                  (cond-expand
                   (guile-vm
                    (raise (make-unimplemented-error 'peek-char)))
                   (hoot
                    (%inline-wasm
                     '(func (param $bv (ref $bytevector))
                            (param $cur i32)
                            (param $end i32)
                            (result i64)
                            (i64.extend_i32_s
                             (stringview_iter.next
                              (string.as_iter
                               (string.new_lossy_utf8_array
                                (struct.get $bytevector $vals (local.get $bv))
                                (local.get $cur)
                                (local.get $end))))))
                     bv cur (+ cur len))))))))))))))

  (define (scan-codepoint u8 line col)
    (cond
     ((< u8 #b10000000)
      (call-with-values
          (lambda ()
            (case (integer->char u8)
              ((#\alarm)     (values line col))
              ((#\backspace) (values line (if (> col 0) (1- col) col)))
              ((#\newline)   (values (1+ line) 0))
              ((#\return)    (values line 0))
              ((#\tab)       (values line (logand (+ col 8) (lognot 7))))
              (else          (values line (1+ col)))))
        (lambda (line col)
          (values 1 line col))))
     ((< u8 #b11100000) (values 2 line (1+ col)))
     ((< u8 #b11110000) (values 3 line (1+ col)))
     (else (values 4 line (1+ col)))))

  (define* (read-char #:optional (port (current-input-port)))
    (define (decode-wtf8/1 a)
      (integer->char a))
    (define (decode-wtf8/2 a b)
      (unless (and (eq? #b110 (ash a -5))
                   (eq? #b10 (ash b -6)))
        (error "decoding error: bad utf-8 sequence"))
      (let ((a (logand a #b11111))
            (b (logand b #b111111)))
        (integer->char (logior (ash a 6) b))))
    (define (decode-wtf8/3 a b c)
      (unless (and (eq? #b1110 (ash a -4))
                   (eq? #b10 (ash b -6))
                   (eq? #b10 (ash c -6)))
        (error "decoding error: bad utf-8 sequence"))
      (let ((a (logand a #b1111))
            (b (logand b #b111111))
            (c (logand c #b111111)))
        (integer->char (logior (ash a 12) (ash b 6) c))))
    (define (decode-wtf8/4 a b c d)
      (unless (and (eq? #b11110 (ash a -3))
                   (eq? #b10 (ash b -6))
                   (eq? #b10 (ash c -6))
                   (eq? #b10 (ash d -6)))
        (error "decoding error: bad utf-8 sequence"))
      (let ((a (logand a #b111))
            (b (logand b #b111111))
            (c (logand c #b111111))
            (d (logand d #b111111)))
        (integer->char (logior (ash a 18) (ash b 12) (ash c 6) d))))
    (match (peek-u8 port)
      ((? eof-object? a) a)
      (u8
       (match (%port-position port)
         ((and pos (line . col))
          (call-with-values (lambda ()
                              (scan-codepoint u8 line col))
            (lambda (len line col)
              (call-with-values
                  (lambda ()
                    (if (eq? len 1)
                        (values (%port-read-buffer port) 1)
                        (%fill-input port (%port-read-buffer port) len)))
                (lambda (buf avail)
                  (when (< avail len)
                    (error "decoding error: partial utf-8 sequence"))
                  (match buf
                    (#(bv cur end has-eof?)
                     (define char
                       (match len
                         ('1 (decode-wtf8/1 u8))
                         ('2 (decode-wtf8/2 u8 (bytevector-u8-ref bv (+ cur 1))))
                         ('3 (decode-wtf8/3 u8 (bytevector-u8-ref bv (+ cur 1))
                                            (bytevector-u8-ref bv (+ cur 2))))
                         (_ (decode-wtf8/4 u8 (bytevector-u8-ref bv (+ cur 1))
                                           (bytevector-u8-ref bv (+ cur 2))
                                           (bytevector-u8-ref bv (+ cur 3))))))
                     (%set-port-buffer-cur! buf (+ cur len))
                     (set-car! pos line)
                     (set-cdr! pos col)
                     char)))))))))))
  (define* (read-string k #:optional (port (current-input-port)))
    (check-type port input-port? 'read-string)
    (cond
     ;; Call peek-char to ensure we're at the start of some UTF-8.
     ((eof-object? (peek-char port)) (eof-object))
     (else
      (match (%port-read-buffer port)
        ((and buf #(bv cur end has-eof?))
         (match (%port-position port)
           ((and pos (line . col))
            (define (take-string count cur* line col)
              (define str (utf8->string bv cur cur*))
              (%set-port-buffer-cur! buf cur*)
              (set-car! pos line)
              (set-cdr! pos col)
              (let ((remaining (- k count)))
                (if (zero? remaining)
                    str
                    (match (read-string remaining port)
                      ((? eof-object?) str)
                      (tail (string-append str tail))))))

            ;; Count codepoints in buffer.
            (let count-codepoints ((count 0) (cur cur) (line line) (col col))
              (if (and (< cur end) (< count k))
                  (call-with-values
                      (lambda ()
                        (scan-codepoint (bytevector-u8-ref bv cur) line col))
                    (lambda (len line* col*)
                      (if (<= (+ cur len) end)
                          (count-codepoints (1+ count) (+ cur len) line* col*)
                          (take-string count cur line col))))
                  (take-string count cur line col))))))))))
  (define* (read-line #:optional (port (current-input-port)))
    (check-type port input-port? 'read-line)
    (define bytes '())
    (define (advance-column bv col)
      (define len (bytevector-length bv))
      (let lp ((idx 0) (col col))
        (cond
         ((< idx len)
          (call-with-values
              (lambda ()
                (scan-codepoint (bytevector-u8-ref bv idx) 0 col))
            (lambda (len line col)
              (lp (+ idx len) col))))
         (else col))))
    (define (finish newline?)
      (let* ((bv (bytevector-concatenate-reverse bytes))
             (str (utf8->string bv)))
        (match (%port-position port)
          ((and pos (line . col))
           (cond
            (newline?
             (set-car! pos (1+ line))
             (set-cdr! pos 0))
            (else
             (set-cdr! pos (advance-column bv col))))))
        str))
    (let read-some ((buf (%port-read-buffer port)))
      (match buf
        (#(bv cur end has-eof?)
         (define (accumulate-bytes! end)
           (set! bytes (cons (bytevector-copy bv cur end) bytes)))
         (let scan-for-newline ((pos cur))
           (cond
            ((< pos end)
             (let ((u8 (bytevector-u8-ref bv pos)))
               (cond
                ((or (eq? u8 (char->integer #\newline))
                     (eq? u8 (char->integer #\return)))
                 (accumulate-bytes! pos)
                 (%set-port-buffer-cur! buf (1+ pos))
                 (when (and (eq? u8 (char->integer #\return))
                            (eq? (peek-u8 port) (char->integer #\newline)))
                   (read-u8 port))
                 (finish #t))
                (else
                 (scan-for-newline (1+ pos))))))
            ((< cur pos)
             (accumulate-bytes! pos)
             (%set-port-buffer-cur! buf pos)
             (read-some (%fill-input port buf 1)))
            ((not has-eof?)
             (read-some (%fill-input port buf 1)))
            ((null? bytes)
             (%set-port-buffer-has-eof?! buf #f)
             (eof-object))
            (else
             (finish #f))))))))

  (define* (write-u8 u8 #:optional (port (current-output-port)))
    (check-type port port? 'write-u8)
    (match (%port-write-buffer port)
      (#f (raise (make-type-error port 'write-u8 'output-port?)))
      ((and buf #(dst cur end))
       (when (and (eq? cur end) (%port-r/w-random-access? port))
         (%flush-input port))
       (cond
        ((= end (bytevector-length dst))
         ;; Multiple threads racing; race to flush, then retry.
         (flush-output-port port)
         (write-u8 u8 port))
        (else
         (bytevector-u8-set! dst end u8)
         (let ((end (1+ end)))
           (%set-port-buffer-end! buf end)
           (when (= end (bytevector-length dst))
             (flush-output-port port))))))))

  (define* (write-bytevector bv #:optional (port (current-output-port))
                             (start 0) (end (bytevector-length bv)))
    (check-type port port? 'write-u8)
    (let ((count (- end start)))
      (match (%port-write-buffer port)
        (#f (raise (make-type-error port 'write-bytevector 'output-port?)))
        ((and buf #(dst cur end))
         (when (and (eq? cur end) (%port-r/w-random-access? port))
           (%flush-input port))
         (let ((size (bytevector-length dst))
               (buffered (- end cur)))
           (cond
            ((<= (+ end count) size)
             ;; Bytes fit in buffer: copy directly.
             (bytevector-copy! dst end bv start (+ start count))
             (let ((end (+ end count)))
               (%set-port-buffer-end! buf end)
               (when (= end size)
                 (flush-output-port port))))
            ((< count size)
             ;; Bytes fit in buffer, but we have to flush output first.
             (flush-output-port port)
             (bytevector-copy! dst 0 bv start (+ start count))
             (%set-port-buffer-cur! buf 0)
             (%set-port-buffer-end! buf count)
             (when (= count size)
               (flush-output-port port)))
            (else
             ;; Otherwise flush any buffered output, then make an
             ;; unbuffered write.
             (unless (zero? buffered) (flush-output-port port))
             (%write-bytes port bv start count))))))))

  (define* (write-char x #:optional (port (current-output-port)))
    ;; FIXME: update port position.
    (define (low-six i) (logand i #b111111))
    (let ((i (char->integer x)))
      (cond
       ((<= i #x7f)
        (write-u8 i port))
       ((<= i #x7ff)
        (write-bytevector
         (bytevector (logior #b11000000 (ash i -6))
                     (logior #b10000000 (low-six i)))
         port))
       ((<= i #xffff)
        (write-bytevector
         (bytevector (logior #b11100000 (ash i -12))
                     (logior #b10000000 (low-six (ash i -6)))
                     (logior #b10000000 (low-six i)))
         port))
       (else
        (write-bytevector
         (bytevector (logior #b11110000 (ash i -18))
                     (logior #b10000000 (low-six (ash i -12)))
                     (logior #b10000000 (low-six (ash i -6)))
                     (logior #b10000000 (low-six i)))
         port)))))

  (define* (newline #:optional (port (current-output-port)))
    (write-char #\newline port))

  (define* (write-string str #:optional (port (current-output-port)))
    ;; FIXME: Could avoid the double-copy and encode directly to buffer.
    (write-bytevector (string->utf8 str) port))

  (define (standard-input-port)
    (make-soft-port "stdin"
                    (lambda ()
                      (cond-expand
                       (guile-vm
                        (make-unimplemented-error 'standard-input-port))
                       (hoot
                        (%inline-wasm
                         '(func (result (ref eq))
                                (struct.new $string
                                            (i32.const 0)
                                            (call $read-stdin)))))))
                    #f #f #f))
  (define (standard-output-port)
    (make-soft-port "stdout"
                    #f
                    (lambda (str)
                      (cond-expand
                       (guile-vm
                        (make-unimplemented-error 'standard-output-port))
                       (hoot
                        (%inline-wasm
                         '(func (param $str (ref string))
                                (call $write-stdout (local.get $str)))
                         str))))
                    #f #f))
  (define (standard-error-port)
    (make-soft-port "stderr"
                    #f
                    (lambda (str)
                      (cond-expand
                       (guile-vm
                        (make-unimplemented-error 'standard-error-port))
                       (hoot
                        (%inline-wasm
                         '(func (param $str (ref string))
                                (call $write-stderr (local.get $str)))
                         str))))
                    #f #f))

  (cond-expand
   (guile-vm
    (define current-input-port
      (make-parameter (guile:make-void-port "r")))
    (define current-output-port
      (make-parameter (guile:make-void-port "w")))
    (define current-error-port
      (make-parameter (guile:make-void-port "w"))))
   (hoot-main
    (define current-input-port
      (make-parameter (standard-input-port)
                      (lambda (val)
                        (check-type val input-port? 'current-input-port)
                        val)))
    (define current-output-port
      (make-parameter (standard-output-port)
                      (lambda (val)
                        (check-type val output-port? 'current-output-port)
                        val)))
    (define current-error-port
      (make-parameter (standard-error-port)
                      (lambda (val)
                        (check-type val output-port? 'current-error-port)
                        val)))
    (%inline-wasm
     '(func (param $current-input-port (ref eq))
            (param $current-output-port (ref eq))
            (param $current-error-port (ref eq))
            (global.set $current-input-port (local.get $current-input-port))
            (global.set $current-output-port (local.get $current-output-port))
            (global.set $current-error-port (local.get $current-error-port)))
     current-input-port
     current-output-port
     current-error-port))
   (hoot-aux
    (define current-input-port
      (%inline-wasm
       '(func (result (ref eq)) (global.get $current-input-port))))
    (define current-output-port
      (%inline-wasm
       '(func (result (ref eq)) (global.get $current-output-port))))
    (define current-error-port
      (%inline-wasm
       '(func (result (ref eq)) (global.get $current-error-port)))))))
