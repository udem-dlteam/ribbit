(cond-expand
  ((host js)

    (define-primitive
      (##stdin)
      (use node-fs)
      "() => push(0),")

    (define-primitive
      (##stdout)
      (use node-fs)
      "() => push(1),")

   (define-primitive 
     (##get-fd-input-file filename)
     (use node-fs scm2str)
     "prim1(filename => fs.openSync(scm2str(filename), 'r')),"
     )

   (define-primitive
     (##get-fd-output-file filename)
     (use node-fs scm2str)
     "prim1(filename => fs.openSync(scm2str(filename), 'w')),"
     )

   (define-primitive
     (##read-char port)
     (use node-fs)
     "prim1(port => {
     let buf=Buffer.alloc(1); 
     let ch=fs.readSync(port[0], buf) === 0 ? NIL : buf[0]; 
     return ch;
     }),"
   )

  (define-primitive
    (##write-char ch port)
    (use node-fs)
    "prim2((port, ch) => fs.writeSync(port[0], String.fromCodePoint(ch), null, 'utf8')),"
    )

  (define-primitive
    (close-input-port port)
    (use node-fs)
    "prim1(port => { if (port[1][2] === TRUE) {
    fs.closeSync(port[0]);
    port[1][2] = FALSE;
    }}),"
  )

   (define-primitive
     (close-output-port port)
     (use node-fs)
     "prim1(port => { if (port[1] === TRUE) {
     fs.closeSync(port[0]);
     port[1] = FALSE;
     }}),"
   ))

  ((host c)

   (define-primitive
     (##stdin)
     (use stdio)
     "{
     FILE* file = fdopen(0, \"r\");
     push2((long) file | 1, PAIR_TAG);
     break;
     }")

   (define-primitive
     (##stdout)
     (use stdio)
     "{
     FILE* file = fdopen(1, \"w\");
     push2((long) file | 1, PAIR_TAG);
     break;
     }")

   (define-primitive 
     (##get-fd-input-file filename)
     (use stdio scm2str)
     "{
     PRIM1();
     char* filename = scm2str(x);
     FILE* file = fopen(filename, \"r\");
     if (file == NULL) perror(\"Couldn't open the file\\n\");
     push2((long) file | 1, PAIR_TAG);
     free((void*) filename);
     break;
     }"
     )

   (define-primitive
     (##get-fd-output-file filename)
     (use stdio scm2str)
     "{
     PRIM1();
     char* filename = scm2str(x);
     FILE* file = fopen(filename, \"w\");
     push2((long) file | 1, PAIR_TAG);
     free((void *) filename);
     break;
     }"
     )

   (define-primitive
     (##read-char port)
     (use stdio)
     "{
     PRIM1();
     FILE* file = (FILE*) ((long) CAR(x) ^ 1);
     char buffer[1];
     int bytes_read = fread(buffer, 1, 1, file);
     if (!bytes_read) push2(NIL, PAIR_TAG);
     else push2(TAG_NUM(buffer[0]), PAIR_TAG);
     break;
     }"
     )

   (define-primitive
     (##write-char ch port)
     (use stdio)
     "{
     PRIM2();
     FILE* file = (FILE*) ((long) CAR(y) ^ 1);
     char buffer[1] = {(char) NUM(x)};
     int success = fwrite(buffer, 1, 1, file);
     if (success != 1) {
        perror(\"WHAT\");
     }
     fflush(file);
     push2(TRUE, PAIR_TAG);
     break;
     }"
     )

   (define-primitive
     (close-input-port port)
     (use stdio)
     "{
     PRIM1();
     if (TAG(CDR(x)) == TRUE) {
        FILE* file = (FILE*) ((long) CAR(x) ^ 1);
        fclose(file);
        TAG(CDR(x)) = FALSE;
     }
     push2(TRUE, PAIR_TAG);
     break;
     }"
     )

   (define-primitive
     (close-output-port port)
     (use stdio)
     "{
     PRIM1();
     if (CDR(x) == TRUE) {
        FILE* file = (FILE*) ((long) CAR(x) ^ 1);
        fclose(file);
        CDR(x) = FALSE;
     }
     push2(TRUE, PAIR_TAG);
     break;
     }"
     )
   ))


;; ---------------------- EOF & TYPES ---------------------- ;;

(define input-port-type 8)
(define output-port-type 9)

(define ##eof (rib 0 0 5))

(define (eof-object? obj)
  (eqv? obj ##eof))

(define stdin-port
  (rib (##stdin) (rib 0 '() #t) input-port-type)) ;; stdin

(define stdout-port
  (rib (##stdout) #t output-port-type))  ;; stdout


;; ---------------------- INPUT ---------------------- ;;

(define (##get-last-char port)
  (field1 (field1 port)))

(define (##set-last-char port ch)
  (field1-set! (field1 port) ch))

(define (input-port-close? port)
  (eqv? (field2 (field1 port)) #f))

(define (open-input-file filename)
  ;; (file_descriptor, (cursor, last_char, is_open), input_file_type)
  (rib (##get-fd-input-file filename) (rib 0 '() #t) input-port-type))

(define (input-port? port)
  (eqv? (field2 port) input-port-type))

(define (current-input-port)
  stdin-port)

(define (call-with-input-file filename proc)
  (let* ((port (open-input-file filename))
         (result (proc port)))
    (close-input-port port)
    result))

(define (read-char (port (current-input-port))) 
  (if (input-port-close? port)
    (error "Cannot read from a closed port"))
  (if (eqv? (##get-last-char port) '())
    (let ((ch (##read-char port)))
      (if (eqv? ch '()) ##eof ch))
    (let ((ch (##get-last-char port)))
      (##set-last-char port '())
      ch)))

(define (peek-char (port (current-input-port)))
  (if (input-port-close? port)
    (error "Cannot read from a closed port"))
  (if (eqv? (##get-last-char port) '())
    (let* ((ch (##read-char port)) (ch (if (eqv? ch '()) ##eof ch)))
      (##set-last-char port ch)
      ch)
    (##get-last-char port)))


;; ---------------------- READ ---------------------- ;;

(define (read (port (current-input-port)))
  (if (input-port-close? port)
    (error "Cannot read from a closed port"))

  (let ((c (peek-char-non-whitespace port)))
    (cond ((< c 0)
           c)
          ((eqv? c 40) ;; #\(
           (read-char port) ;; skip "("
           (read-list port))
          ((eqv? c 35) ;; #\#
           (read-char port) ;; skip "#"
           (let ((c (peek-char port)))
             (cond ((eqv? c 102) ;; #\f
                    (read-char port) ;; skip "f"
                    #f)
                   ((eqv? c 116) ;; #\t
                    (read-char port) ;; skip "t"
                    #t)
                   (else ;; assume it is #\(
                    (list->vector (read port))))))
          ((eqv? c 39) ;; #\'
           (read-char port) ;; skip "'"
           (rib 'quote (rib (read port) '() 0) 0))
;;          ((eqv? c 34) ;; #\"
;;           (read-char) ;; skip """
;;           (list->string (read-chars '())))
          (else
           (read-char port) ;; skip first char
           (let ((s (list->string (rib c (read-symbol port) 0))))
             (let ((n (string->number s)))
               (or n
                   (string->symbol s))))))))

(define (read-list port)
  (let ((c (peek-char-non-whitespace port)))
    (if (eqv? c 41) ;; #\)
        (begin
          (read-char port) ;; skip ")"
          '())
        (let ((first (read port)))
          (rib first (read-list port) 0)))))

(define (read-symbol port)
  (let ((c (peek-char port)))
    (if (or (eqv? c 40) ;; #\(
            (eqv? c 41) ;; #\)
            (eof-object? c)
            (< c 33)) ;; whitespace
        '()
        (begin
          (read-char port)
          (rib c (read-symbol port) 0)))))

;;(define (read-chars lst)
;;  (let ((c (read-char)))
;;    (cond ((eof-object? c)
;;           '())
;;          ((eqv? c 34) ;; #\"
;;           (reverse lst))
;;          ((eqv? c 92) ;; #\\
;;           #; ;; no support for \n in strings
;;           (read-chars (cons (read-char) lst))
;;           ;#; ;; support for \n in strings
;;           (let ((c2 (read-char)))
;;             (read-chars (cons (if (eqv? c2 110) 10 c2) lst))))
;;          (else
;;           (read-chars (cons c lst))))))

(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (eof-object? c) ;; eof?
        -1
        (if (< 32 c) ;; above #\space ?
            (if (eqv? c 59) ;; #\;
                (skip-comment port)
                c)
            (begin
              (read-char port)
              (peek-char-non-whitespace port))))))

(define (skip-comment port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        (if (eqv? c 10) ;; #\newline
            (peek-char-non-whitespace port)
            (skip-comment port)))))


;; ---------------------- OUTPUT ---------------------- ;;

(define (open-output-file filename)
  ;; (file_descriptor, is_open, write_file_type)
  (rib (##get-fd-output-file filename) #t output-port-type))

(define (output-port? port)
  (eqv? (field2 port) output-port-type))

(define (current-output-port)
  stdout-port)

(define (output-port-close? port)
  (eqv? (field1 port) #f))

(define (call-with-output-file filename proc)
  (let* ((port (open-output-file filename))
         (result (proc port)))
    (close-output-port port)
    result))

(define (write-char ch (port (current-output-port)))
  (##write-char ch port))

(define (newline (port (current-output-port)))
  (write-char 10 port))

(define (write o (port (current-output-port)))
  (cond ((eqv? (field2 o) 3) ;; string?
         (write-char 34 port)
         (write-chars (field0 o) port)
         (write-char 34 port))
        (else
         (display o port))))

(define (display o (port (current-output-port)))
  (cond ((eqv? o #f)
         (write-char 35 port)
         (write-char 102 port)) ;; #f
        ((eqv? o #t)
         (write-char 35 port)
         (write-char 116 port)) ;; #t
        ((eof-object? o)
         (display "#!eof" port))
        ((eqv? o '())
         (write-char 40 port)
         (write-char 41 port)) ;; ()
        ((eqv? (rib? o) #f)
         (display (number->string o) port))
        ((eqv? (field2 o) 0) ;; pair?
         (write-char 40 port)  ;; #\(
         (write (field0 o) port) ;; car
         (write-list (field1 o) port) ;; cdr
         (write-char 41 port)) ;; #\)
        ((eqv? (field2 o) 2) ;; symbol?
         (display (field1 o) port)) ;; name
        ((eqv? (field2 o) 3) ;; string?
         (write-chars (field0 o) port)) ;; chars
;;        ((vector? o)
;;         (write-char 35) ;; #\#
;;         (write (vector->list o)))
        ((eqv? (field2 o) 1) ;; procedure?
         (write-char 35 port)
         (write-char 112 port)) ;; #p
        (else
         ;; must be a number
         (display (number->string o) port))))

(define (write-list lst port)
  (if (eqv? (field0 lst) 0) ;; pair?
      (begin
        (write-char 32 port) ;; #\space
        (if (eqv? (field2 lst) 0) ;; pair?
            (begin
              (write (field0 lst) port) ;; car
              (write-list (field1 lst) port))  ;; cdr
            #f)) ;; writing dotted pairs is not supported
      #f))

(define (write-chars lst port)
  (if (eqv? (field2 lst) 0) ;; pair?
      (let ((c (field0 lst))) ;; car
        (write-char c port)
        (write-chars (field1 lst) port)) ;; cdr
      #f))



;; ---------------------- LOAD ---------------------- ;;
(define (load filename)
  )


;; ---------------------- UTIL ---------------------- ;;

(define string->list field0)
(define (list->string lst) (rib lst (length lst) 3)) ;; string
(define (list->vector lst) (rib lst (length lst) vector-type)) 

(define (number->string x)
  (list->string
   (if (< x 0)
       (rib 45 (number->string-aux (- 0 x) '()) 0) ;; cons
       (number->string-aux x '()))))

(define (number->string-aux x tail)
  (let ((q (quotient x 10)))
    (let ((d (+ 48 (- x (* q 10)))))
      (let ((t (rib d tail 0))) ;; cons
        (if (< 0 q)
            (number->string-aux q t)
            t)))))

(define (string->number str)
  (let ((lst (string->list str)))
    (if (eqv? lst '())
        #f
        (if (eqv? (field0 lst) 45) ;; car
            (string->number-aux (field1 lst)) ;; cdr
            (let ((n (string->number-aux lst)))
              (and n (- 0 n)))))))

(define (string->number-aux lst)
  (if (eqv? lst '())
      #f
      (string->number-aux2 lst 0)))

(define (string->number-aux2 lst n)
  (if (and (rib? lst) (eqv? (field2 lst) 0)) ;; pair?
    (let ((c (field0 lst))) ;; car
        (and (< 47 c)
             (< c 58)
             (string->number-aux2 (field1 lst) ;; cdr
                                  (- (* 10 n) (- c 48)))))
      n))

(define (length lst)
  (if (eqv? lst '())
    0
    (+ 1 (length (field1 lst)))))



(define symtbl (field1 rib)) ;; get symbol table

(define (string->uninterned-symbol str) (rib #f str 2)) ;; 2 is symbol-type

(define (string->symbol str)
  (string->symbol-aux str symtbl))

(define (string->symbol-aux str syms)
  (if (and (rib? syms) (eqv? (field2 syms) 0)) ;; pair?
    (let ((sym (field0 syms)))
      (if (equal? (field1 sym) str)
        sym
        (string->symbol-aux str (field1 syms))))
    (let ((sym (string->uninterned-symbol str)))
      (set! symtbl (rib sym symtbl 0))
      sym)))


(define (equal? x y)
  (or (eqv? x y)
      (and (rib? x)
           (if (eqv? (field2 x) 5) ;; 5 is singleton-type
             #f
             (and (rib? y)
                  (equal? (field2 x) (field2 y))
                  (equal? (field1 x) (field1 y))
                  (equal? (field0 x) (field0 y)))))))
