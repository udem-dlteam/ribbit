(cond-expand
  ((host js)

   (define-primitive 
     (open-input-file filename)
     (use io scm2str)
     "prim1(filename => make_input_port(node_fs.openSync(scm2str(filename), 'r'))),"
     )

   (define-primitive
     (open-output-file filename)
     (use io scm2str)
     "prim1(filename => make_output_port(node_fs.openSync(scm2str(filename), 'w'))),"
     )

   (define-primitive
     (input-port? port)
     (use io bool2scm)
     "prim1(port => bool2scm(port[2] === 8)),"
     )

   (define-primitive
     (output-port? port)
     (use io bool2scm)
     "prim1(port => bool2scm(port[2] === 9)),"
     )

   (define-primitive
     (##read-char port)
     (use io)
     "prim1(port => {
        let buf=Buffer.alloc(1); 
        let ch = node_fs.readSync(port[0], buf, {position: port[1][0]++}) === 0 ? EOF : buf[0]; 
        port[1][1] = ch; 
        return ch;
      }),"
   )

   (define-primitive
     (##write-char ch port)
     (use io scm2str)
     "prim2((port, ch) => node_fs.writeSync(port[0], scm2str(ch), {encoding: 'utf8'})),"
   )

   (define-primitive
     (current-input-port)
     (use io)
     "() => make_input_port(0),"
     )

   (define-primitive
     (current-output-port)
     (use io)
     "() => make_output_port(1),"
     )

   (define-primitive
     (close-input-port port)
     (use io)
     "prim1(port => { if (port[1][2] === TRUE) {
        node_fs.closeSync(port[0]);
        port[1][2] = FALSE;
     }}),"
     )

   (define-primitive
     (close-output-port port)
     (use io)
     "prim1(port => { if (port[1] === TRUE) {
        node_fs.closeSync(port[0]);
        port[1] = FALSE;
     }}),"
     )

   (define-primitive
     (eof-object? obj)
     (use io bool2scm)
     "prim1(obj => bool2scm(obj === EOF)),"
     )
  ))

;; ---------------------- INPUT ---------------------- ;;

(define (input-port-close? port)
  (eqv? (field2 (field1 port)) #f))

(define (read-char (port (current-input-port)))
  (if (input-port-close? port)
    (error "Cannot read from a closed port")
    (##read-char port))
  )

#| (define (peek-char (port (current-input-port)))
  (if (eqv? (##get-last-char port) '())
    (let ((ch (##read-char port)))
      (field0-set! port (- (field0 port) 1))
      ch
      )
    (##get-last-char port)
    )) |#

(define (peek-char (port (current-input-port)))
  (field1 (field1 port)))

;; ---------------------- OUTPUT ---------------------- ;;

(define (write-char ch (port (current-output-port)))
  (##write-char ch port))

(define (newline (port (current-output-port)))
  (write-char "\n" port))

#| (define (write o)
  (cond ((string? o)
         (putchar 34)
         (write-chars (string->list o))
         (putchar 34))
        (else
         (display o))))

(define (display o)
  (cond ((not o)
         (putchar2 35 102)) ;; #f
        ((eqv? o #t)
         (putchar2 35 116)) ;; #t
        ((null? o)
         (putchar2 40 41)) ;; ()
        ((pair? o)
         (putchar 40)  ;; #\(
         (write (car o))
         (write-list (cdr o))
         (putchar 41)) ;; #\)
        ((symbol? o)
         (display (symbol->string o)))
        ((string? o)
         (write-chars (string->list o)))
;;        ((vector? o)
;;         (putchar 35) ;; #\#
;;         (write (vector->list o)))
        ((procedure? o)
         (putchar2 35 112)) ;; #p
        (else
         ;; must be a number
         (display (number->string o)))))

(define (write-list lst)
  (if (pair? lst)
      (begin
        (putchar 32) ;; #\space
        (if (pair? lst)
            (begin
              (write (car lst))
              (write-list (cdr lst)))
            #f)) ;; writing dotted pairs is not supported
      #f))

(define (write-chars lst escape?)
 (if (pair? lst)
     (let ((c (car lst)))
       (putchar
        (cond ((not escape?)
               c)
              ;#; ;; support for \n in strings
              ((eqv? c 10) ;; #\newline
               (putchar 92)
               110)
              ((or (eqv? c 34) ;; #\"
                   (eqv? c 92)) ;; #\\
               (putchar 92)
               c)
              (else
               c)))
       (write-chars (cdr lst) escape?))
     #f))

(define (write-chars lst)
  (if (pair? lst)
      (let ((c (car lst)))
        (putchar c)
        (write-chars (cdr lst)))
      #f))

(define (write-char c)
  (putchar c)) |#

