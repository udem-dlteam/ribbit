(##include-once "./types.scm")

(cond-expand
  ((host js)

   (define-primitive
     (##stdin)
     (use js/node/fs)
     "() => push(0),")

   (define-primitive
     (##stdout)
     (use js/node/fs)
     "() => push(1),")

   (define-primitive 
     (##get-fd-input-file filename)
     (use js/node/fs scm2str)
     "prim1(filename => fs.openSync(scm2str(filename), 'r')),")

   (define-primitive
     (##get-fd-output-file filename)
     (use js/node/fs scm2str)
     "prim1(filename => fs.openSync(scm2str(filename), 'w')),")

   (define-primitive
     (##read-char fd)
     (use js/node/fs)
     "prim1(fd => {
     let buf=Buffer.alloc(1); 
     let ch=fs.readSync(fd, buf) === 0 ? NIL : buf[0]; 
     return ch;
     }),")

   (define-primitive
     (##write-char ch fd)
     (use js/node/fs)
     "prim2((fd, ch) => fs.writeSync(fd, String.fromCodePoint(ch), null, 'utf8')),")

   (define-primitive
     (##close-input-port fd)
     (use js/node/fs)
     "prim1(fd => fs.closeSync(fd)),")

   (define ##close-output-port ##close-input-port))
   
  ((host c)

   (define-primitive
     (##stdin)
     (use c/stdio)
     "{
     FILE* file = fdopen(0, \"r\");
     push2((long) file | 1, PAIR_TAG);
     break;
     }")

   (define-primitive
     (##stdout)
     (use c/stdio)
     "{
     FILE* file = fdopen(1, \"w\");
     push2((long) file | 1, PAIR_TAG);
     break;
     }")

   (define-primitive 
     (##get-fd-input-file filename)
     (use c/stdio scm2str)
     "{
     PRIM1();
     char* filename = scm2str(x);
     FILE* file = fopen(filename, \"r\");
     if (file == NULL) perror(\"Couldn't open the file\\n\");
     push2((long) file | 1, PAIR_TAG);
     free((void*) filename);
     break;
     }")

   (define-primitive
     (##get-fd-output-file filename)
     (use c/stdio scm2str)
     "{
     PRIM1();
     char* filename = scm2str(x);
     FILE* file = fopen(filename, \"w\");
     push2((long) file | 1, PAIR_TAG);
     free((void *) filename);
     break;
     }")

   (define-primitive
     (##read-char fd)
     (use c/stdio)
     "{
     PRIM1();
     FILE* file = (FILE*) ((long) x ^ 1);
     char buffer[1];
     int bytes_read = fread(buffer, 1, 1, file);
     if (!bytes_read) push2(NIL, PAIR_TAG);
     else push2(TAG_NUM(buffer[0]), PAIR_TAG);
     break;
     }")

   (define-primitive
     (##write-char ch fd)
     (use c/stdio)
     "{
     PRIM2();
     FILE* file = (FILE*) ((long) y ^ 1);
     char buffer[1] = {(char) NUM(x)};
     int success = fwrite(buffer, 1, 1, file);
     if (success != 1) {
     perror(\"WHAT\");
     }
     fflush(file);
     push2(TRUE, PAIR_TAG);
     break;
     }")

   (define-primitive
     (##close-input-port fd)
     (use c/stdio)
     "{
     PRIM1();
     FILE* file = (FILE*) ((long) x ^ 1);
     fclose(file);
     break;
     }")

   (define ##close-output-port ##close-input-port))

  ((host hs)

   (define-feature hs/io-handle (hs/foreign-type "    | RibHandle !Handle"))

   (define-primitive
     (##stdin)
     (use hs/io-handle)
     " , push . RibForeign $ RibHandle stdin")

   (define-primitive
     (##stdout)
     (use hs/io-handle)
     " , push . RibForeign $ RibHandle stdout")

   (define-primitive 
     (##get-fd-input-file filename)
     (use hs/io-handle scm2str)
     " , prim1 $ \\filename -> scm2str filename >>= (\\x -> openFile x ReadMode) >>= (pure . RibForeign . RibHandle)")

   (define-primitive
     (##get-fd-output-file filename)
     (use hs/io-handle scm2str)
     " , prim1 $ \\filename -> scm2str filename >>= (\\x -> openFile x WriteMode) >>= (pure . RibForeign . RibHandle)")

   (define-primitive
     (##read-char fd)
     (use hs/io-handle)
     " , prim1 $ \\(RibForeign (RibHandle handle)) -> hIsEOF handle >>= \\eof -> if eof then return ribNil else hGetChar handle >>= (pure . RibInt . ord)")

   (define-primitive
     (##write-char ch fd)
     (use hs/io-handle)
     " , prim2 $ \\(RibInt ch) (RibForeign (RibHandle handle)) -> hPutChar handle (chr ch) >> pure ribTrue")

   (define-primitive
     (##close-input-port fd)
     (use hs/io-handle)
     " , prim1 $ \\(RibForeign (RibHandle handle)) -> hClose handle >> pure ribTrue")

   (define ##close-output-port ##close-input-port)))

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

(define (open-input-file filename)
  ;; (file_descriptor, (cursor, last_char, is_open), input_file_type)
  (rib (##get-fd-input-file filename) (rib 0 '() #t) input-port-type))

(define (close-input-port port)
  (if (field2 (field1 port))
    (begin 
      (field2-set! (field1 port) #f)
      (##close-input-port (field0 port)))))

(define (input-port? port)
  (eqv? (field2 port) input-port-type))
(define (##get-last-char port)
  (field1 (field1 port)))

(define (##set-last-char port ch)
  (field1-set! (field1 port) ch))

(define (input-port-close? port)
  (eqv? (field2 (field1 port)) #f))

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
    (let ((ch (##read-char (field0 port))))
      (if (eqv? ch '()) ##eof (integer->char ch)))
    (let ((ch (##get-last-char port)))
      (##set-last-char port '())
      (integer->char ch))))

(define (peek-char (port (current-input-port)))
  (if (input-port-close? port)
    (error "Cannot read from a closed port"))
  (if (eqv? (##get-last-char port) '())
    (let* ((ch (##read-char (field0 port))) (ch (if (eqv? ch '()) ##eof ch)))
      (##set-last-char port ch)
      (integer->char ch))
    (integer->char (##get-last-char port))))


;; ---------------------- READ ---------------------- ;;

(define (read (port (current-input-port)))
  (if (input-port-close? port)
    (error "Cannot read from a closed port"))

  (let ((c (peek-char-non-whitespace port)))
    (cond ((eof-object? c)
           c)
          ((eqv? c #\() ;; #\(
           (read-char port) ;; skip "("
           (read-list port))
          ((eqv? c #\#) ;; #\#
           (read-char port) ;; skip "#"
           (let ((c (peek-char port)))
             (cond ((eqv? c #\f) ;; #\f
                    (read-char port) ;; skip "f"
                    #f)
                   ((eqv? c #\t) ;; #\t
                    (read-char port) ;; skip "t"
                    #t)
                   ((eqv? c #\\) ;; #\\
                    (read-char port) ;; skip "\\"
                    (read-char port))
                   (else ;; assume it is #\(
                     (list->vector (read port))))))
          ((eqv? c #\') ;; #\'
           (read-char port) ;; skip "'"
           (rib 'quote (rib (read port) '() 0) 0))
          ;; ((eqv? c 34) ;; #\"
          ;;  (read-char) ;; skip """
          ;;  (list->string (read-chars '())))
          (else
            (read-char port) ;; skip first char
            (let ((s (list->string (rib c (read-symbol port) 0))))
              (let ((n (string->number s)))
                (or n
                    (string->symbol s))))))))

(define (read-list port)
  (let ((c (peek-char-non-whitespace port)))
    (if (eqv? c #\)) ;; #\)
        (begin
          (read-char port) ;; skip ")"
          '())
        (let ((first (read port)))
          (rib first (read-list port) 0)))))

(define (read-symbol port)
  (let ((c (peek-char port)))
    (if (or (eqv? c #\() ;; #\(
            (eqv? c #\)) ;; #\)
            (eof-object? c)
            (< (field0 c) 33)) ;; whitespace
        '()
        (begin
          (read-char port)
          (rib (field0 c) (read-symbol port) 0)))))

(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (eof-object? c) ;; eof?
        c
        (if (< 32 (field0 c)) ;; above #\space ?
            (if (eqv? c #\;) ;; #\;
                (skip-comment port)
                c)
            (begin
              (read-char port)
              (peek-char-non-whitespace port))))))

(define (skip-comment port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        (if (eqv? c #\newline) ;; #\newline
            (peek-char-non-whitespace port)
            (skip-comment port)))))


;; ---------------------- OUTPUT ---------------------- ;;

(define (open-output-file filename)
  ;; (file_descriptor, is_open, write_file_type)
  (rib (##get-fd-output-file filename) #t output-port-type))

(define (close-output-port port)
  (if (field1 port)
    (begin 
      (field1-set! port #f)
      (##close-output-port (field0 port)))))

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
  (##write-char (char->integer ch) (field0 port)))

(define (newline (port (current-output-port)))
  (write-char #\newline port))

(define (write o (port (current-output-port)))
  (cond ((string? o)
         (write-char #\" port)
         (write-chars (field0 o) port)
         (write-char #\" port))
        (else
         (display o port))))

(define (display o (port (current-output-port)))
  (cond ((eqv? o #f)
         (write-char #\# port)
         (write-char #\f port)) ;; #f
        ((eqv? o #t)
         (write-char #\# port)
         (write-char #\t port)) ;; #t
        ((eof-object? o)
         (display "#!eof" port))
        ((null? o)
         (write-char #\( port)
         (write-char #\) port)) ;; ()
        ((integer? o)
         (display (number->string o) port))
        ((char? o)
         (write-char o port))
        ((pair? o)
         (write-char #\( port)  ;; #\(
         (write (field0 o) port) ;; car
         (write-list (field1 o) port) ;; cdr
         (write-char #\) port)) ;; #\)
        ((symbol? o)
         (display (field1 o) port)) ;; name
        ((string? o)
         (write-chars (field0 o) port)) ;; chars
        ((vector? o)
         (write-char #\#) ;; #\#
         (write-char #\()
         (let ((l (vector->list o)))
           (write (car l) port)
           (write-list (cdr l) port))
         (write-char #\)))
        ((procedure? o)
         (write-char #\# port)
         (write-char #\p port)) ;; #p
        (else
         (error "Object not printable"))))

(define (write-list lst port)
  (cond 
    ((pair? lst)
     (write-char #\space port) ;; #\space
     (if (pair? lst)
       (begin
         (write (field0 lst) port) ;; car
         (write-list (field1 lst) port))  ;; cdr
       #f))

    ((null? lst) #f)

    (else
      (write-char #\space port) ;; #\space
      (write-char #\. port) ;; #\.
      (write-char #\space port) ;; #\space
      (write lst port))))


(define (write-chars lst port)
  (if (pair? lst)
      (let ((c (field0 lst))) ;; car
        (write-char (integer->char c) port)
        (write-chars (field1 lst) port)) ;; cdr
      #f))

