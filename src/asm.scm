;;;============================================================================

;;; File: "asm.scm"

;;; Copyright (c) 1994-2022 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; This module implements a generic assembler.

;;;============================================================================

;; (asm-make-code-block start-pos endianness) starts a new empty code
;; block at address start-pos and returns it.  The parameter
;; endianness is a symbol (be or le) which indicates the byte ordering
;; to use for 16, 32 and 64 bit values.  After a call to
;; asm-make-code-block the code block is built by calling the
;; following procedures:
;;
;;  asm-8            adds an 8 bit integer to the code block
;;  asm-16-be        adds a 16 bit big-endian integer to the code block
;;  asm-16-le        adds a 16 bit little-endian integer to the code block
;;  asm-16           like asm-16-be or asm-16-le depending on endianness
;;  asm-32-be        adds a 32 bit big-endian integer to the code block
;;  asm-32-le        adds a 32 bit little-endian integer to the code block
;;  asm-32           like asm-32-be or asm-32-le depending on endianness
;;  asm-64-be        adds a 64 bit big-endian integer to the code block
;;  asm-64-le        adds a 64 bit little-endian integer to the code block
;;  asm-64           like asm-64-be or asm-64-le depending on endianness
;;  asm-int          adds an 8/16/32/64 bit int to the code block
;;  asm-int-be       adds an 8/16/32/64 bit big-endian int to the code block
;;  asm-int-le       adds an 8/16/32/64 bit little-endian int to the code block
;;  asm-string       adds a null terminated ISO8859-1 string to the code block
;;  asm-label        sets a label to the current position in the code block
;;  asm-align        adds enough padding bytes to force alignment
;;  asm-origin       adds enough padding bytes to move to a particular address
;;  asm-at-assembly  defers code production to assembly time
;;  asm-listing      adds textual information to the listing

(define (asm-code-block-size) 5)

(define (asm-code-block-start-pos cb)          (vector-ref cb 1))
(define (asm-code-block-start-pos-set! cb x)   (vector-set! cb 1 x))

(define (asm-code-block-endianness cb)         (vector-ref cb 2))
(define (asm-code-block-endianness-set! cb x)  (vector-set! cb 2 x))

(define (asm-code-block-stream cb)             (vector-ref cb 3))
(define (asm-code-block-stream-set! cb x)      (vector-set! cb 3 x))

(define (asm-code-block-lbl-counter cb)        (vector-ref cb 4))
(define (asm-code-block-lbl-counter-set! cb x) (vector-set! cb 4 x))

(define (asm-make-code-block start-pos endianness)
  (let ((cb (make-vector (asm-code-block-size))))
    (vector-set! cb 0 'code-block)
    (asm-init-code-block cb
                         start-pos
                         endianness)))

(define (asm-init-code-block cb start-pos endianness)
  (asm-code-block-start-pos-set!   cb start-pos)
  (asm-code-block-endianness-set!  cb endianness)
  (asm-code-block-stream-set!      cb (asm-make-stream))
  (asm-code-block-lbl-counter-set! cb 0)
  cb)

(define (asm-copy-code-block cb)
  (let ((copy (make-vector (asm-code-block-size))))
    (vector-set! copy 0 'code-block)
    (asm-code-block-start-pos-set!   copy (asm-code-block-start-pos   cb))
    (asm-code-block-endianness-set!  copy (asm-code-block-endianness  cb))
    (asm-code-block-stream-set!      copy (asm-code-block-stream      cb))
    (asm-code-block-lbl-counter-set! copy (asm-code-block-lbl-counter cb))
    copy))

;; (asm-8 cb n) adds the 8 bit signed or unsigned integer n to the
;; code block.

(define (asm-8 cb n)
  (asm-code-extend cb (asm-bits-0-to-7 n)))

;; (asm-16 cb n) adds the 16 bit signed or unsigned integer n to the
;; code block.

(define (asm-16 cb n)
  (if (eq? 'be (asm-code-block-endianness cb))
      (asm-16-be cb n)
      (asm-16-le cb n)))

(define (asm-16-be cb n)
  (asm-8 cb (asm-bits-8-and-up n))
  (asm-8 cb n))

(define (asm-16-le cb n)
  (asm-8 cb n)
  (asm-8 cb (asm-bits-8-and-up n)))

;; (asm-32 cb n) adds the 32 bit signed or unsigned integer n to the
;; code block.

(define (asm-32 cb n)
  (if (eq? 'be (asm-code-block-endianness cb))
      (asm-32-be cb n)
      (asm-32-le cb n)))

(define (asm-32-be cb n)
  (asm-16-be cb (asm-bits-16-and-up n))
  (asm-16-be cb n))

(define (asm-32-le cb n)
  (asm-16-le cb n)
  (asm-16-le cb (asm-bits-16-and-up n)))

;; (asm-64 cb n) adds the 64 bit signed or unsigned integer n to the
;; code block.

(define (asm-64 cb n)
  (if (eq? 'be (asm-code-block-endianness cb))
      (asm-64-be cb n)
      (asm-64-le cb n)))

(define (asm-64-be cb n)
  (asm-32-be cb (asm-bits-32-and-up n))
  (asm-32-be cb n))

(define (asm-64-le cb n)
  (asm-32-le cb n)
  (asm-32-le cb (asm-bits-32-and-up n)))

;; (asm-int cb n width) adds the signed or unsigned integer n of width
;; bits to the code block and returns the signed integer.

(define (asm-int cb n width)
  (if (eq? 'be (asm-code-block-endianness cb))
      (asm-int-be cb n width)
      (asm-int-le cb n width)))

(define (asm-int-be cb n width)
  (cond ((fx= width 8)
         (let ((x (asm-signed-lo8 n)))
           (asm-8 cb x)
           x))
        ((fx= width 16)
         (let ((x (asm-signed-lo16 n)))
           (asm-16-be cb x)
           x))
        ((fx= width 32)
         (let ((x (asm-signed-lo32 n)))
           (asm-32-be cb x)
           x))
        (else
         (let ((x (asm-signed-lo64 n)))
           (asm-64-be cb x)
           x))))

(define (asm-int-le cb n width)
  (cond ((fx= width 8)
         (let ((x (asm-signed-lo8 n)))
           (asm-8 cb x)
           x))
        ((fx= width 16)
         (let ((x (asm-signed-lo16 n)))
           (asm-16-le cb x)
           x))
        ((fx= width 32)
         (let ((x (asm-signed-lo32 n)))
           (asm-32-le cb x)
           x))
        (else
         (let ((x (asm-signed-lo64 n)))
           (asm-64-le cb x)
           x))))

;; (asm-string cb str) adds the null terminated ISO8859-1 string str
;; to the code block.

(define (asm-string cb str)
  (let* ((u8vect
          (list->u8vector (map char->integer (string->list str))))
         (len
          (u8vector-length u8vect)))
    (let loop ((i 0))
      (if (fx< i len)
          (begin
            (asm-8 cb (u8vector-ref u8vect i))
            (loop (fx+ i 1)))
          (asm-8 cb 0)))))

;; (asm-make-label cb id) creates a new label object.  A label can be
;; queried with asm-label-pos to obtain the label's position.  The
;; argument id gives a name (not necessarily unique) to the label and
;; is only needed for debugging purposes.

(define (asm-make-label cb id)
  (vector3 'label
           #f
           id))

(define (asm-make-label* cb)
  (let ((n (asm-code-block-lbl-counter cb)))
    (asm-code-block-lbl-counter-set! cb (fx+ n 1))
    (asm-make-label cb (string->symbol
                        (string-append "L" (number->string n))))))

;; (asm-label? x) tests if x is a label object.

(define (asm-label? x)
  (and (vector? x)
       (fx= (vector-length x) 3)
       (eq? (vector-ref x 0) 'label)))

;; (asm-label cb label-obj) sets the label to the current position in
;; the code block.

(define (asm-label cb label-obj)
  (if (vector-ref label-obj 1)
      (compiler-internal-error
       "asm-label, label multiply defined"
       (asm-label-id label-obj))
      (begin
        (vector-set! label-obj 1 0)
        (asm-code-extend cb label-obj))))

;; (asm-label-id label-obj) returns the identifier of the label object.

(define (asm-label-id label-obj)
  (vector-ref label-obj 2))

(define (asm-label-name label-obj)
  (let ((id (asm-label-id label-obj)))
    (cond ((string? id) id)
          ((symbol? id) (symbol->string id))
          ((number? id) (string-append "_" (number->string id)))
          (else
           (compiler-internal-error
            "asm-label-name, this type of label id is not supported"
            id)))))

;; (asm-label-pos label-obj) returns the position of the label
;; relative to the start of the code block (i.e. start-pos).
;; This procedure can only be called at assembly time (i.e.
;; within the call to asm-assemble) or after assembly time
;; for labels declared prior to assembly time with asm-label.
;; A label declared at assembly time can only be queried after
;; assembly time.  Moreover, at assembly time the position of a
;; label may vary from one call to the next due to the actions
;; of the assembler.

(define (asm-label-pos label-obj)
  (let ((pos (vector-ref label-obj 1)))
    (if pos
        pos
        (compiler-internal-error
         "asm-label-pos, undefined label"
         (asm-label-id label-obj)))))

;; (asm-align cb multiple offset fill) adds enough fill bytes to the
;; code block to force alignment to the next address congruent to
;; offset modulo multiple.

(define (asm-align cb multiple offset fill)
  (asm-at-assembly
   cb
   (list2
    (lambda (cb self)
      (fxmodulo (fx- multiple (fx- self offset)) multiple))
    (lambda (cb self)
      (let ((n (fxmodulo (fx- multiple (fx- self offset)) multiple)))
        (let loop ((i n))
          (if (fx> i 0)
              (begin
                (asm-8 cb fill)
                (loop (fx- i 1))))))))))

;; (asm-origin cb address fill) adds enough fill bytes to the code
;; block to move to the given address.

(define (asm-origin cb address fill)
  (asm-at-assembly
   cb
   (list2
    (lambda (self)
      (fx- address self))
    (lambda (self)
      (let ((len (fx- address self)))
        (if (fx< len 0)
            (compiler-internal-error "asm-origin, can't move back" '())
            (let loop ((n len))
              (if (fx> n 0)
                  (begin
                    (asm-8 cb fill)
                    (loop (fx- n 1)))))))))))

;; (asm-at-assembly cb procs) makes it possible to defer code
;; production to assembly time.  A useful application is to generate
;; position dependent and span dependent code sequences.  This
;; procedure must be passed a list of procedures of even length.  All
;; odd indexed procedures (including the first procedure) are called
;; "check" procedures.  The even indexed procedures are the
;; "production" procedures which, when called, produce a particular
;; code sequence.  A check procedure decides if, given the current
;; state of assembly (in particular the current positioning of the
;; labels), the code produced by the corresponding production
;; procedure is valid.  If the code is not valid, the check procedure
;; must return #f.  If the code is valid, the check procedure must
;; return the length of the code sequence in bytes.  The assembler
;; will try each check procedure in order until it finds one that does
;; not return #f (the last check procedure must never return #f).  For
;; convenience, the code block and current position in the code
;; sequence are passed as the two arguments of check and production
;; procedures.
;;
;; Here is a sample call of asm-at-assembly to produce the
;; shortest branch instruction to branch to label x for a
;; hypothetical processor:
;;
;;  (asm-at-assembly
;;
;;    cb
;;
;;    (list4
;;
;;     (lambda (cb self) ;; first check procedure
;;       (let ((dist (fx- (asm-label-pos x) self)))
;;         (if (and (fx>= dist -128) (fx<= dist 127)) ;; short branch possible?
;;             2
;;             #f)))
;;
;;     (lambda (cb self) ;; first production procedure
;;       (asm-8 cb #x34) ;; branch opcode for 8 bit displacement
;;       (asm-8 cb (fx- (asm-label-pos x) self)))
;;
;;     (lambda (cb self) 5) ;; second check procedure
;;
;;     (lambda (cb self) ;; second production procedure
;;       (asm-8 cb #x35) ;; branch opcode for 32 bit displacement
;;       (asm-32 cb (fx- (asm-label-pos x) self)))))

(define (asm-at-assembly cb procs)
  (asm-code-extend cb (vector3 'deferred procs 0)))

;; (asm-listing cb text) adds text to the right side of the listing.
;; The atoms in text will be output using the display procedure.  The
;; listing is generated by calling asm-display-listing.

(define (asm-listing cb text)
  (asm-code-extend cb (vector2 'listing text)))

;; (asm-separated-list lst sep) returns a list which, when displayed
;; in a listing, will display each element of the list lst separated
;; with the string sep.

(define (asm-separated-list lst sep)
  (if (pair? lst)
      (cons (car lst)
            (map (lambda (x) (cons sep x))
                 (cdr lst)))
      lst))

;; (asm-display-listing cb encoding?) produces a listing of the code block on
;; the current output port.  The listing contains the text inserted by
;; asm-listing and, when the parameter encoding? is #t, the bytes generated
;; are shown in hexadecimal on the left side of the listing.

(define (asm-display-listing cb encoding?)

  (define text-col 24)
  (define pos-width 6)
  (define byte-width 2)
  (define pos-radix 16)

  (define (output text)
    (cond ((null? text))
          ((pair? text)
           (output (car text))
           (output (cdr text)))
          (else
           (display text))))

  (define (print-digit n)
    (write-char (string-ref "0123456789abcdef" n)))

  (define (print-byte n)
    (print-digit (fxquotient n 16))
    (print-digit (fxmodulo n 16)))

  (define (print-pos n)

    (define (p n i)
      (if (fx> i 0)
          (if (fx< n 0)
              (begin
                (p n (fx- i 1))
                (output " "))
              (begin
                (p (fxquotient n pos-radix) (fx- i 1))
                (print-digit (fxmodulo n pos-radix))))))

    (output "#")
    (p n (- pos-width 1)))

  (let loop1 ((lst (cdr (asm-code-block-stream cb)))
              (pos (asm-code-block-start-pos cb))
              (col 0))
    (if (pair? lst)
        (let ((x (car lst)))
          (if (vector? x)
              (let ((kind (vector-ref x 0)))
                (cond ((eq? kind 'listing)
                       (if encoding?
                           (let ((col
                                  (if (fx= col 0)
                                      (begin
                                        (print-pos pos)
                                        pos-width)
                                      col)))
                             (let loop2 ((col col))
                               (if (fx< col text-col)
                                   (begin
                                     (output "\t")
                                     (loop2 (fx* 8 (fx+ (fxquotient col 8) 1))))))))
                       (output (vector-ref x 1))
                       (newline)
                       (loop1 (cdr lst) pos 0))
                      (else
                       ;;(loop1 (cdr lst) pos col)
                       (compiler-internal-error
                        "asm-display-listing, code stream not assembled"
                        '()))))
              (cond ((not encoding?)
                     (loop1 (cdr lst) (fx+ pos 1) col))
                    ((or (fx= col 0) (fx>= col (fx- text-col byte-width)))
                     (if (not (fx= col 0)) (newline))
                     (print-pos pos)
                     (display " ")
                     (print-byte x)
                     (loop1 (cdr lst) (fx+ pos 1) (fx+ (fx+ pos-width 1) byte-width)))
                    (else
                     (print-byte x)
                     (loop1 (cdr lst) (fx+ pos 1) (fx+ col byte-width))))))
        (newline))))

;; (asm-assemble cb) assembles the code block and returns the number
;; of bytes in the code block.  After assembly, the label objects will
;; be set to their final position and the alignment bytes and the
;; deferred code will have been produced.  It is possible to extend
;; the code block after assembly.  However, if any of the procedures
;; "asm-label", "asm-align", "asm-origin", and "asm-at-assembly" are
;; called, the code block will have to be assembled once more.

(define (asm-assemble cb)

  (define (pass1)

    ;; construct fixup list and make first label assignment

    (let loop ((curr (cdr (asm-code-block-stream cb)))
               (fixup-lst '())
               (span 0)
               (pos (asm-code-block-start-pos cb)))
      (if (pair? curr)
          (let ((x (car curr)))
            (if (vector? x)
                (let ((kind (vector-ref x 0)))
                  (cond ((eq? kind 'label)
                         ;; make first approximation of label's position
                         (vector-set! x 1 pos)
                         (loop (cdr curr)
                               (cons (cons span curr) fixup-lst)
                               0
                               pos))
                        ((eq? kind 'deferred)
                         (loop (cdr curr)
                               (cons (cons span curr) fixup-lst)
                               0
                               pos))
                        (else
                         (loop (cdr curr)
                               fixup-lst
                               span
                               pos))))
                (loop (cdr curr)
                      fixup-lst
                      (fx+ span 1)
                      (fx+ pos 1))))
          (reverse fixup-lst))))

  (let ((fixup-lst (pass1)))

    (let loop1 ()

      ;; determine size of deferred code given current label positions

      (let loop2 ((lst fixup-lst)
                  (pos (asm-code-block-start-pos cb))
                  (changed? #f))
        (if (pair? lst)
            (let* ((fixup (car lst))
                   (pos (fx+ pos (car fixup)))
                   (curr (cdr fixup))
                   (x (car curr)))
              (if (eq? (vector-ref x 0) 'label)
                  ;; label
                  (loop2 (cdr lst) pos changed?)
                  ;; deferred
                  (let ((old-size (vector-ref x 2)))
                    (let loop3 ()
                      (let* ((check (car (vector-ref x 1)))
                             (new-size (check cb pos)))
                        (if new-size
                            (if (fx= old-size new-size)
                                (loop2 (cdr lst) (fx+ pos old-size) changed?)
                                (begin
                                  ;; set the new size of the deferred code
                                  (vector-set! x 2 new-size)
                                  ;; position must advance according to old size
                                  (loop2 (cdr lst) (fx+ pos old-size) #t)))
                            (begin
                              ;; discard current check/production procedures
                              (vector-set! x 1 (cddr (vector-ref x 1)))
                              (loop3))))))))

            ;; determine label positions given new size of deferred code

            (let loop4 ((lst fixup-lst)
                        (pos (asm-code-block-start-pos cb))
                        (changed? changed?))
              (if (pair? lst)
                  (let* ((fixup (car lst))
                         (pos (fx+ pos (car fixup)))
                         (curr (cdr fixup))
                         (x (car curr)))
                    (if (eq? (vector-ref x 0) 'label)
                        ;; label
                        (if (fx= (vector-ref x 1) pos)
                            (loop4 (cdr lst) pos changed?)
                            (begin
                              (vector-set! x 1 pos)
                              (loop4 (cdr lst) pos #t)))
                        ;; deferred
                        (let ((new-size (vector-ref x 2)))
                          (loop4 (cdr lst) (fx+ pos new-size) changed?))))

                  ;; repeat if one or more labels changed position

                  (if changed?
                      (loop1)))))))

    ;; generate deferred code by calling production procedures

    (let loop5 ((prev (asm-code-block-stream cb))
                (curr (cdr (asm-code-block-stream cb)))
                (pos (asm-code-block-start-pos cb)))
      (if (pair? curr)
          (let ((x (car curr))
                (next (cdr curr)))
            (if (vector? x)
                (let ((kind (vector-ref x 0)))
                  (cond ((eq? kind 'label)
                         (let ((final-pos (vector-ref x 1)))
                           (if final-pos
                               (if (not (fx= pos final-pos))
                                   (compiler-internal-error
                                    "asm-assemble, inconsistency detected"
                                    '()))
                               (vector-set! x 1 pos))
                           ;; remove label
                           (set-cdr! prev next)
                           (loop5 prev next pos)))
                        ((eq? kind 'deferred)
                         (let ((temp (asm-code-block-stream cb)))
                           (asm-code-block-stream-set! cb (asm-make-stream))
                           (let ((production (cadr (vector-ref x 1))))
                             (production cb pos))
                           (let ((tail (car (asm-code-block-stream cb))))
                             (set-cdr! tail next))
                           (let ((head (cdr (asm-code-block-stream cb))))
                             (set-cdr! prev head)
                             (asm-code-block-stream-set! cb temp)
                             (loop5 prev head pos))))
                        (else
                         (loop5 curr next pos))))
                (loop5 curr next (fx+ pos 1))))
          (begin
            (set-car! (asm-code-block-stream cb) prev)
            (fx- pos (asm-code-block-start-pos cb)))))))

;; (asm-assemble-to-file cb filename) assembles the code block and
;; writes it to a file.

'
(define (asm-assemble-to-file cb filename)
  (asm-assemble cb)
  (with-output-to-file filename
    (lambda ()
      (let loop ((lst (cdr (asm-code-block-stream cb))))
        (if (pair? lst)
            (let ((x (car lst)))
              (if (vector? x)
                  (let ((kind (vector-ref x 0)))
                    (if (not (eq? kind 'listing))
                        (compiler-internal-error
                         "asm-write-code, code stream not assembled"
                         '()))
                    (loop (cdr lst)))
                  (begin
                    (write-char (integer->char x))
                    (loop (cdr lst))))))))))

;; (asm-assemble-to-u8vector cb) assembles the code block and converts
;; it to a u8vector containing the sequence of bytes.

(define (asm-assemble-to-u8vector cb)
  (let* ((len (asm-assemble cb))
         (u8v (make-u8vector len)))
    (let loop ((lst (cdr (asm-code-block-stream cb)))
               (pos 0))
        (if (pair? lst)
            (let ((x (car lst)))
              (if (vector? x)
                  (let ((kind (vector-ref x 0)))
                    (if (not (eq? kind 'listing))
                        (compiler-internal-error
                         "asm-write-code, code stream not assembled"
                         '()))
                    (loop (cdr lst) pos))
                  (begin
                    (u8vector-set! u8v pos x)
                    (loop (cdr lst) (fx+ pos 1)))))
            u8v))))

;; (asm-write-hex-file cb filename) writes the code in a file using
;; the intel hex file format.

'
(define (asm-write-hex-file cb filename)
  (with-output-to-file filename
    (lambda ()

      (define (print-digit n)
        (display (string-ref "0123456789ABCDEF" n)))

      (define (print-byte n)
        (print-digit (fxquotient n 16))
        (print-digit (fxmodulo n 16)))

      (define (print-line type addr bytes)
        (let ((n (length bytes))
              (addr-hi (fxquotient addr 256))
              (addr-lo (fxmodulo addr 256)))
          (display ":")
          (print-byte n)
          (print-byte addr-hi)
          (print-byte addr-lo)
          (print-byte type)
          (for-each print-byte bytes)
          (let ((sum
                 (fxmodulo (fx- (apply fx+ n addr-hi addr-lo type bytes)) 256)))
            (print-byte sum)
            (newline))))

      (let loop ((lst (cdr (asm-code-block-stream cb)))
                 (pos (asm-code-block-start-pos cb))
                 (rev-bytes '()))
        (if (pair? lst)
            (let ((x (car lst)))
              (if (vector? x)
                  (let ((kind (vector-ref x 0)))
                    (if (not (eq? kind 'listing))
                        (compiler-internal-error
                         "asm-write-hex-file, code stream not assembled"
                         '()))
                    (loop (cdr lst)
                          pos
                          rev-bytes))
                  (let ((new-pos
                         (fx+ pos 1))
                        (new-rev-bytes
                         (cons x
                               (if (fx= (fxmodulo pos 16) 0)
                                   (begin
                                     (print-line 0
                                                 (fx- pos (length rev-bytes))
                                                 (reverse rev-bytes))
                                     '())
                                   rev-bytes))))
                    (loop (cdr lst)
                          new-pos
                          new-rev-bytes))))
            (begin
              (if (pair? rev-bytes)
                  (print-line 0
                              (fx- pos (length rev-bytes))
                              (reverse rev-bytes)))
              (print-line 1 0 '())
              pos))))))

;; Utilities.

(define (asm-make-stream) ;; create an empty stream
  (let ((x (cons '() '())))
    (set-car! x x)
    x))

(define (asm-code-extend cb item) ;; add an item at the end of current code stream
  (let* ((stream (asm-code-block-stream cb))
         (tail (car stream))
         (cell (cons item '())))
    (set-cdr! tail cell)
    (set-car! stream cell)))

;; Note that the following code may operate on bignums.

(define (asm-signed8? n) ;; is n a signed 8 bit integer?
  (and (<= #x-80 n) (<= n #x7f)))

(define (asm-signed16? n) ;; is n a signed 16 bit integer?
  (and (<= #x-8000 n) (<= n #x7fff)))

(define (asm-signed32? n) ;; is n a signed 32 bit integer?
  (and (<= #x-80000000 n) (<= n #x7fffffff)))

(define (asm-signed-lo8 n) ;; return low 8 bits of n as a signed integer
  (- (modulo (+ n #x80) #x100)
     #x80))

(define (asm-unsigned-lo8 n) ;; return low 8 bits of n as an unsigned integer
  (modulo n #x100))

(define (asm-signed-lo16 n) ;; return low 16 bits of n as a signed integer
  (- (modulo (+ n #x8000) #x10000)
     #x8000))

(define (asm-unsigned-lo16 n) ;; return low 16 bits of n as an unsigned integer
  (modulo n #x10000))

(define (asm-signed-lo32 n) ;; return low 32 bits of n as a signed integer
  (let ((two**32 (* #x10000 #x10000))
        (two**31 (* #x8000 #x10000)))
    (- (modulo (+ n two**31) two**32)
       two**31)))

(define (asm-unsigned-lo32 n) ;; return low 32 bits of n as an unsigned integer
  (let ((two**32 (* #x10000 #x10000)))
    (modulo n two**32)))

(define (asm-signed-lo64 n) ;; return low 64 bits of n as a signed integer
;;  (- (modulo (+ n #x8000000000000000) #x10000000000000000)
;;     #x8000000000000000))
  n) ;; kludge to avoid needing bignums

(define (asm-unsigned-lo64 n) ;; return low 64 bits of n as an unsigned integer
;;  (modulo n #x10000000000000000))
  n) ;; kludge to avoid needing bignums

(define (asm-signed-lo n width) ;; return low "width" bits of n as a signed integer
  (cond ((eqv? 8 width)  (asm-signed-lo8 n))
        ((eqv? 16 width) (asm-signed-lo16 n))
        ((eqv? 32 width) (asm-signed-lo32 n))
        ((eqv? 64 width) (asm-signed-lo64 n))
        (else            (compiler-internal-error "unsupported width" width))))

(define (asm-unsigned-lo n width) ;; return low "width" bits of n
  (cond ((eqv? 8 width)  (asm-unsigned-lo8 n))
        ((eqv? 16 width) (asm-unsigned-lo16 n))
        ((eqv? 32 width) (asm-unsigned-lo32 n))
        ((eqv? 64 width) (asm-unsigned-lo64 n))
        (else            (compiler-internal-error "unsupported width" width))))

(define (asm-bits-0-to-7 n) ;; return bits 0 to 7 of a signed integer
  (modulo n #x100))

(define (asm-bits-8-and-up n) ;; return bits 8 and up of a signed integer
  (if (>= n 0)
      (quotient n #x100)
      (- (quotient (+ n 1) #x100) 1)))

(define (asm-bits-16-and-up n) ;; return bits 16 and up of a signed integer
  (if (>= n 0)
      (quotient n #x10000)
      (- (quotient (+ n 1) #x10000) 1)))

(define (asm-bits-32-and-up n) ;; return bits 32 and up of a signed integer
  (let ((two**32 (* #x10000 #x10000)))
    (if (>= n 0)
        (quotient n two**32)
        (- (quotient (+ n 1) two**32) 1))))

;;;============================================================================
