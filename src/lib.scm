;;; SScheme (Small-Scheme) library.

;;;----------------------------------------------------------------------------

;; Low-level operations supported by the micro-Scheme VM (here
;; implemented on top of standard Scheme).

(define (clump? o) (vector? o))
(define (clump field1 field2 field3) (vector field1 field2 field3))
(define (field1 o) (vector-ref o 0))
(define (field2 o) (vector-ref o 1))
(define (field3 o) (vector-ref o 2))
(define (field1-set! o x) (vector-set! o 0 x))
(define (field2-set! o x) (vector-set! o 1 x))
(define (field3-set! o x) (vector-set! o 2 x))

(define eq? eq?)

(define getchar (let ((rc read-char))
                  (lambda ()
                    (let ((c (rc)))
                      (if (eof-object? c)
                          -1
                          (char->integer c))))))

(define putchar (let ((wc write-char))
                  (lambda (c)
                    (wc (integer->char c)))))

(define < <)
(define = =)

(define + +)
(define - -)
(define * *)
(define quotient quotient)

(define halt exit)

;;;----------------------------------------------------------------------------

;; Implementation of SScheme types using the micro-Scheme operations.

(define false (clump 0 0 0)) ;; #f
(define (false? o) (eq? o false))

(define true (clump 0 0 0)) ;; #t
(define (true? o) (eq? o true))

(define null (clump 0 0 0)) ;; ()
(define (null? o) (eq? o null))

(define pair-type (clump 0 0 0))
(define (pair? o) (and (clump? o) (eq? pair-type (field1 o))))
(define (cons car cdr) (clump pair-type car cdr))
(define (car pair) (field2 pair))
(define (cdr pair) (field3 pair))

(define string-type (clump 0 0 0))
(define (string? o) (and (clump? o) (eq? string-type (field1 o))))
(define (list->string lst) (clump string-type lst 0))
(define (string->list str) (field2 str))

(define symbol-type (clump 0 0 0))
(define (symbol? o) (and (clump? o) (eq? symbol-type (field1 o))))
(define (string->symbol str) (clump symbol-type str 0))
(define (symbol->string sym) (field2 sym))
(define (global-var-ref sym) (field3 sym))
(define (global-var-set! sym x) (field3-set! sym x))

(define (string->number str)
  (string->number-aux (string->list str) 0))

(define (string->number-aux lst n)
  (if (pair? lst)
      (let ((c (car lst)))
        (if (and (< 47 c) (< c 58))
            (string->number-aux (cdr lst) (+ (* 10 n) (- c 48)))
            false))
      n))

;;;----------------------------------------------------------------------------

;; Character I/O (characters are represented with integers).

(define buf -2)

(define (read-char)
  (let ((c buf))
    (if (= c -2)
        (getchar)
        (begin (set! buf -2) c))))

(define (peek-char)
  (let ((c buf))
    (if (= c -2)
        (begin (set! buf (getchar)) buf)
        c)))

;;;----------------------------------------------------------------------------

;; The read procedure.

(define (read)
  (let ((c (peek-char-non-whitespace)))
    (cond ((< c 0)
           c)
          ((= c 40) ;; #\(
           (read-char) ;; skip "("
           (read-list))
          ((= c 35) ;; #\#
           (read-char) ;; skip "#"
           (read-sharp))
          (else
           (read-char) ;; skip first char
           (let ((s (list->string (cons c (read-symbol)))))
             (let ((n (string->number s)))
               (if (false? n)
                   (string->symbol s)
                   n)))))))

(define (read-list)
  (let ((c (peek-char-non-whitespace)))
    (if (= c 41) ;; #\)
        (begin
          (read-char) ;; skip ")"
          null)
        (let ((first (read)))
          (let ((rest (read-list)))
            (cons first rest))))))

(define (read-sharp)
  (let ((c (peek-char-non-whitespace)))
    (cond ((= c 102) ;; #\f
           (read-char) ;; skip "f"
           false)
          ((= c 116) ;; #\t
           (read-char) ;; skip "t"
           true)
          ((= c 40) ;; #\(
           (read-char) ;; skip "("
           (list->vector (read-list)))
          (else
           (halt))))) ;; TODO: error?

(define (read-symbol)
  (let ((c (peek-char)))
    (if (or (= c 40) ;; #\(
            (= c 41) ;; #\)
            (< c 33)) ;; whitespace or eof?
        null
        (begin
          (read-char)
          (cons c (read-symbol))))))

(define (peek-char-non-whitespace)
  (let ((c (peek-char)))
    (if (or (< c 0)   ;; eof?
            (< 32 c)) ;; above #\space ?
        c
        (begin
          (read-char)
          (peek-char-non-whitespace)))))

;;;----------------------------------------------------------------------------

;; The write procedure.

(define (write o)
  (cond ((false? o)
         (putchar 35)   ;; #\#
         (putchar 102)) ;; #\f
        ((true? o)
         (putchar 35)   ;; #\#
         (putchar 116)) ;; #\t
        ((null? o)
         (putchar 40)  ;; #\(
         (putchar 41)) ;; #\)
        ((pair? o)
         (putchar 40)  ;; #\(
         (write (car o))
         (write-list (cdr o))
         (putchar 41))  ;; #\)
        ((symbol? o)
         (write-chars (string->list (symbol->string o))))
        (else
         ;; must be a number
         (write-number o))))

(define (write-list lst)
  (if (null? lst)
      #f
      (begin
        (putchar 32) ;; #\space
        (if (pair? lst)
            (begin
              (write (car lst))
              (write-list (cdr lst)))
            (begin ;; probably overkill to implement dotted pairs
              (putchar 46) ;; #\.
              (putchar 32) ;; #\space
              (write lst))))))

(define (write-chars lst)
  (if (null? lst)
      #f
      (begin
        (putchar (car lst))
        (write-chars (cdr lst)))))

(define (write-number n)
  ;; only supports nonnegative integers
  (let ((q (quotient n 10)))
    (if (< 0 q) (write-number q))
    (putchar (+ 48 (- n (* q 10)))))) ;; 0..9

;;;----------------------------------------------------------------------------

;; try it...

(write (read))

;;;----------------------------------------------------------------------------
