;; ########## Booleans && Equality (R4RS sections 6.1 and 6.2) ########## ;;

(define (not obj) (##eqv? obj (or)))

(define (notnot obj) (not (not obj)))

(define (inverse-result proc) (lambda args (not (apply proc args))))

(define (transformed-call f transform) (lambda args (apply f (map transform args))))

(define (equal? x y)
  (or (##eqv? x y)
      (and (##rib? x)
           (if (##eqv? (##field2 x) singleton-type)
               #f
               (and (##rib? y)
                    (equal? (##field2 x) (##field2 y))
                    (equal? (##field1 x) (##field1 y))
                    (equal? (##field0 x) (##field0 y)))))))



;; ########## Types (R4RS section 3.4 + others) ########## ;;

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)
(define char-type      6)

(define input-port-type 8)
(define output-port-type 9)

(define (instance? type) (lambda (o) (and (##rib? o) (##eqv? (##field2 o) type))))

(define pair? (instance? pair-type))
(define symbol? (instance? symbol-type))
(define string? (instance? string-type))
(define vector? (instance? vector-type))
(define procedure? (instance? procedure-type))
(define char? (instance? char-type))
(define (boolean? o) (or (##eqv? o #t) (##eqv? o #f)))

(define input-port? (instance? input-port-type))
(define output-port? (instance? output-port-type))
  

(define (eqv? o1 o2)
  (if (and (char? o1) (char? o2)) 
    (##eqv? (##field0 o1) (##field0 o2))
    (##eqv? o1 o2)))


(define (eq? o1 o2) (##eqv? o1 o2))

(define (null? obj) (##eqv? obj '()))

(define (integer? obj) (not (##rib? obj)))
(define number? integer?)
(define rational? integer?)
(define real? integer?)
(define complex? integer?)

(define (list? obj)

  (define (list?-aux fast slow)
    (if (pair? fast)
      (let ((fast (##field1 fast)))
        (cond ((##eqv? fast slow)
               #f)
              ((pair? fast)
               (list?-aux (##field1 fast) (##field1 slow)))
              (else
                (null? fast))))
      (null? fast)))

  (list?-aux obj obj))


(define (length lst)
  (if (pair? lst)
      (##+ 1 (length (##field1 lst)))
      0))

;; ----- CONVERSIONS ----- ;;

(define (char->integer x) (##field0 x))
(define (integer->char n) (##rib n 0 char-type))

(define (##list->string lst) (##rib lst (length lst) string-type))
(define (##string->list x) (##field0 x))

(define (##map proc lst)
  (if (pair? lst)
    (##cons (proc (##field0 lst)) (##map proc (##field1 lst)))
    '()))

(define (list->string lst) (##list->string (##map char->integer lst)))
(define (string->list s) (##map integer->char (##string->list s)))

(define (list->vector lst) (##rib lst (length lst) vector-type))
(define (vector->list x) (##field0 x))

(define (symbol->string x) (##field1 x))

(define (string->symbol str)

  (define (string->symbol-aux str syms)
    (if (pair? syms)
      (let ((sym (##field0 syms)))
        (if (equal? (##field1 sym) str)
          sym
          (string->symbol-aux str (##field1 syms))))
      (let ((sym (##rib #f (string-append str) symbol-type))) ;; string->uninterned-symbol
        (set! symtbl (##rib sym symtbl pair-type)) ;; cons
        sym)))

  (string->symbol-aux str symtbl))

(define (string->uninterned-symbol str) (##rib #f (string-append str) symbol-type))

(define symtbl (##field1 ##rib)) ;; get symbol table

(define (number->string x (radix 10))
  (define (number->string-aux x tail)
    (let ((q (##quotient x radix)))
      (let ((d (##- x (##* q radix))))
        (let ((t (##rib (if (##< 9 d) (##+ 55 d) (##+ 48 d)) tail pair-type))) ;; cons
          (if (##< 0 q)
            (number->string-aux q t)
            t)))))

  (let ((chars (if (##< x 0)
                 (##rib 45 (number->string-aux (##- 0 x) '()) pair-type) ;; cons
                 (number->string-aux x '()))))
    (##rib
      chars 
      (length chars)
      string-type)))


(define (string->number str (radix 10))

  (define (string->number-aux lst)
    (and (pair? lst)
         (string->number-aux2 lst 0)))

  (define (string->number-aux2 lst n)
    (if (pair? lst)
        (let* ((c (##field0 lst))
               (x (cond ((< 47 c 58) (##- c 48))  ;; 0-9
                        ((< 64 c 71) (##- c 55))  ;; A-F
                        ((< 96 c 103) (##- c 87)) ;; a-f
                        (else 99))))
          (and (##< x radix)
               (string->number-aux2
                (##field1 lst) ;; cdr
                (##- (##* radix n) x))))
        n))

  (let ((lst (##string->list str)))
    (and (pair? lst)
         (if (##eqv? (##field0 lst) 45)      ;; car
             (string->number-aux (##field1 lst)) ;; cdr
             (let ((n (string->number-aux (if (##eqv? (##field0 lst) 43) 
                                            (##field1 lst)
                                            lst))))
               (and n (##- 0 n)))))))



;; ########## Pairs and lists (R4RS section 6.3) ########## ;;

(cond-expand 
  ((host js)
   (define-primitive
     (##cons car cdr)
     "prim2((cdr, car) => [car,cdr,0]),"))
  (else 
    (define (##cons car cdr) (##rib car cdr pair-type))))

(define (cons car cdr) (##cons car cdr))
(define (car x) (##field0 x))
(define (cdr x) (##field1 x))
(define (set-car! x car) (##field0-set! x car))
(define (set-cdr! x cdr) (##field1-set! x cdr))

(define (cadr pair) (##field0 (##field1 pair)))
(define (cddr pair) (##field1 (##field1 pair)))
(define (caddr pair) (cadr (##field1 pair)))
(define (cadddr pair) (caddr (##field1 pair)))

(define (caar pair) (##field0 (##field0 pair)))
(define (cdar pair) (##field1 (##field0 pair)))

(define (caaar pair) (caar (##field0 pair)))
(define (caadr pair) (caar (##field1 pair)))
(define (cadar pair) (cadr (##field0 pair)))
(define (cdaar pair) (cdar (##field0 pair)))
(define (cdadr pair) (cdar (##field1 pair)))
(define (cddar pair) (cddr (##field0 pair)))
(define (cdddr pair) (cddr (##field1 pair)))

(define (caaaar pair) (caaar (##field0 pair)))
(define (caaadr pair) (caaar (##field1 pair)))
(define (caadar pair) (caadr (##field0 pair)))
(define (caaddr pair) (caadr (##field1 pair)))
(define (cadaar pair) (cadar (##field0 pair)))
(define (cadadr pair) (cadar (##field1 pair)))
(define (caddar pair) (caddr (##field0 pair)))
(define (cdaaar pair) (cdaar (##field0 pair)))
(define (cdaadr pair) (cdaar (##field1 pair)))
(define (cdadar pair) (cdadr (##field0 pair)))
(define (cdaddr pair) (cdadr (##field1 pair)))
(define (cddaar pair) (cddar (##field0 pair)))
(define (cddadr pair) (cddar (##field1 pair)))
(define (cdddar pair) (cdddr (##field0 pair)))
(define (cddddr pair) (cdddr (##field1 pair)))


(define (list . args) args)

(define (append . lsts)
  (if (pair? lsts)
    (let ((lst (##field0 lsts)))
      (if (pair? lst)
        (##cons (##field0 lst) (apply append (##cons (##field1 lst) (##field1 lsts))))
        (if (null? (##field1 lsts))
          lst
          (apply append (##field1 lsts)))))
    '()))

(define (reverse lst)
  (reverse-aux lst '()))

(define (reverse-aux lst result)
  (if (pair? lst)
    (reverse-aux (##field1 lst) (##cons (##field0 lst) result))
    result))

(define (list-ref lst i)
  (##field0 (list-tail lst i)))

(define (list-set! lst i x)
  (##field0-set! (list-tail lst i) x))

(define (list-tail lst i)
  (if (##< 0 i)
      (list-tail (##field1 lst) (##- i 1))
      lst))

(define (memv x lst)
  (if (pair? lst)
      (if (eqv? x (##field0 lst))
          lst
          (memv x (##field1 lst)))
      #f))

(define memq memv)


(define (member x lst)
  (if (pair? lst)
      (if (equal? x (##field0 lst))
          lst
          (member x (##field1 lst)))
      #f))

(define (assv x lst)
  (if (pair? lst)
      (let ((couple (##field0 lst)))
        (if (eqv? x (##field0 couple))
            couple
            (assv x (##field1 lst))))
      #f))

(define assq assv)

(define (assoc x lst)
  (if (pair? lst)
      (let ((couple (##field0 lst)))
        (if (equal? x (##field0 couple))
            couple
            (assoc x (##field1 lst))))
      #f))

(define (make-list k fill lst)
  (if (##< 0 k)
      (make-list (##- k 1) fill (##cons fill lst))
      lst))



;; ########## Case (R4RS section 4.2.1) ########## ;;

(define ##case-memv memv)



;; ########## Quasiquotes (R4RS section 4.2.6) ########## ;;

(define ##qq-list list)
(define ##qq-list->vector list->vector)
(define ##qq-cons cons)
(define ##qq-append append)



;; ########## Vectors (R4RS section 6.8) ########## ;; 

(define (vector-length x) (##field1 x))
(define (vector-ref vect i) (list-ref (##field0 vect) i))
(define (vector-set! vect i x) (list-set! (##field0 vect) i x))

(define (make-vector k) (list->vector (make-list k 0 '())))
(define (make-vector-fill k fill) (list->vector (make-list k fill '())))

(define (vector . args) (list->vector args))



;; ########## Characters (R4RS section 6.6) ########## ;;

(define char=? eqv?)
(define (char<? ch1 ch2) (##< (##field0 ch1) (##field0 ch2)))
(define (char>? ch1 ch2) (char<? ch2 ch1))
(define char<=? (inverse-result char>?))
(define char>=? (inverse-result char<?))

(define (char-upcase ch)
  (if (char-lower-case? ch)
      (integer->char (##- (##field0 ch) 32)) ; (- (##field0 #\A) (##field0 #\a))
      ch))

(define (char-downcase ch)
  (if (char-upper-case? ch)
      (integer->char (##+ (##field0 ch) 32)) ; (- (##field0 #\a) (##field0 #\A))
      ch))

(define char-ci=? (transformed-call char=? char-downcase))
(define char-ci<? (transformed-call char<? char-downcase))
(define char-ci>? (transformed-call char>? char-downcase))
(define char-ci<=? (inverse-result char-ci>?))
(define char-ci>=? (inverse-result char-ci<?))

(define (char-alphabetic? ch)
  (or (char-lower-case? ch)
      (char-upper-case? ch)))

(define (char-in-range start end) 
  (lambda (ch) (< start (##field0 ch) end)))

(define char-numeric? (char-in-range 47 58)) ;; #\0 #\9

(define (char-whitespace? ch) (##< (##field0 ch) 33)) ;; #\backspace #\tab #\newline #\vtab #\page #\return

(define (char-lower-case? ch) (char-in-range 96 123)) ;; #\a #\z

(define char-upper-case? (char-in-range 64 91)) ;; #\A #\Z



;; ########## Numbers (R4RS section 6.5) ########## ;;

(define (+ . args)
  (##fold (lambda (x y) (##+ x y)) 0 args))

(define (* . args)
  (##fold (lambda (x y) (##* x y)) 1 args))

(define (- x . y)
  (if (null? y)
    (##- 0 x)
    (##fold (lambda (x y) (##- x y)) x y)))

(define (/ x . y)
  (if (null? y)
    (##quotient 1 x)    ;; 1/x
    (##fold quotient x y)))

(define (quotient x y) (##quotient x y))

(define (exact? obj) #t)
(define (inexact? obj) #f)

(define (= x . rest)
  (##scan-until-false eqv? x #t rest))

(define (< x . rest) 
  (##scan-until-false (lambda (x y) (##< x y)) x #t rest))

(define (> x . rest) 
  (##scan-until-false (lambda (x y) (##< y x)) x #t rest))

(define (<= x . rest)
  (##scan-until-false (inverse-result >) x #t rest))

(define (>= x . rest) 
  (##scan-until-false (inverse-result <) x #t rest))

(define (zero? x) (##eqv? x 0))
(define (positive? x) (##< 0 x))
(define (negative? x) (##< x 0))
(define (even? x) (##eqv? x (##* 2 (##quotient x 2))))
(define odd? (inverse-result even?))

(define (max x . rest) 
  (##fold (lambda (curr best) (if (##< best curr) curr best)) x rest))

(define (min x . rest) 
  (##fold (lambda (curr best) (if (##< best curr) best curr)) x rest))

(define (abs x) (if (##< x 0) (##- 0 x) x))

(define (remainder x y)
  (##- x (##* y (##quotient x y))))

(define (modulo x y)
  (let ((r (remainder x y)))
    (if (##eqv? r 0)
      0
      (if (##eqv? (##< x 0) (##< y 0))
        r
        (##+ r y)))))

(define (gcd . args)
  (define (gcd-aux x y)
    (if (##eqv? x 0)
      y
      (gcd-aux (remainder y x) x)))

  (##fold (lambda (x y)
          (let ((ax (abs x)) (ay (abs y)))
            (if (##< ax ay)
              (gcd-aux ax ay)
              (gcd-aux ay ax))))
        0
        args))


(define (lcm . args)
  (define (lcm-aux x y)
    (if (##eqv? y 0)
      0
      (let ((ax (abs x)) (ay (abs y)))
        (##* (##quotient ax (gcd ax ay)) ay))))

  (##fold lcm-aux 1 args))


(define (denominator x) 1)

(define (floor x) (##id x))
(define numerator floor)
(define ceiling floor)
(define truncate floor)
(define round floor)



;; ########## Control (R4RS section 6.9) ########## ;;

(cond-expand
  ((host js)

   (define-primitive
     (##apply f args)
     "() => {
        let num_args = 0;
        let arg = pop();
        let f = pop();
        while (arg !== NIL) {
            push(arg[0]);
            arg=arg[1];
            num_args++;
        }
        push(num_args); // @@(feature arity-check)@@
        return f;
     }, "))

  ((host c)
   (define-primitive
     (##apply f args)
     "{
     PRIM2();
     int num_args = 0;
     obj arg = TAG_RIB(y);
     while (arg != NIL) {
        push2(CAR(arg), PAIR_TAG);
        arg = TAG_RIB(CDR(arg));
        num_args++;
     }
     push2(TAG_NUM(num_args), PAIR_TAG);
     return TAG_RIB(x);
     }"))

  ((host hs)
   (define-primitive
     (##apply f args)
     " ,  (do
            let numArgs = 0
            arg <- pop
            f <- pop
            let loop arg' numArgs' = do
              case arg' of
                ribNil -> do
                  push (head arg')
                  loop (tail arg') (numArgs' + 1)
                _ -> do
                  push numArgs' -- @@(feature arity-check)@@
                  return f)
        ")))

;; Control features (R4RS section 6.9).

;; (define (apply f arg1 . args) (##apply f (append (list arg1) args))))
(define (apply f args) (##apply f args))

(define (make-procedure code env) (##rib code env procedure-type))

(define (compose f g)
  (lambda args (f (apply g args))))

(define (map proc . lsts)
  (if (pair? (##field0 lsts))
    (##cons (apply proc (##map car lsts))
          (apply map (append (list proc) (##map cdr lsts))))
    '()))

(define (for-each proc . lsts)
  (if (pair? (##field0 lsts))
      (begin
        (apply proc (##map car lsts))
        (apply for-each (append (list proc) (##map cdr lsts))))
      #f))


;; First-class continuations.

(define (call/cc receiver)
  (let ((c (##field1 (##field1 (##close #f))))) ;; get call/cc continuation rib
    (receiver (lambda (r)
                (let ((c2 (##field1 (##field1 (##close #f)))))
                  (##field0-set! c2 (##field0 c)) ;; set "stack" field
                  (##field2-set! c2 (##field2 c)) ;; set "pc" field
                  r))))) ;; return to continuation

(define call-with-current-continuation call/cc)


;; ---------------------- UTILS NOT IN R4RS ---------------------- ;;

;; FIXME: Remove and change the procs that depend on it
(define (##fold func base lst)
  (if (pair? lst)
    (##fold func (func base (##field0 lst)) (##field1 lst))
    base))

(define (##scan-until-false func base state lst)
  (if (and (pair? lst) state)
    (##scan-until-false func (##field0 lst) (func base (##field0 lst)) (##field1 lst))
    state))

(define (partial f . args)
  (lambda other-args (apply f (append args other-args))))



;; ########## Strings (R4RS section 6.7) ########## ;;

(define (string-length x) (##field1 x))

(define (string-ref str i) (integer->char (vector-ref str i)))
(define (string-set! str i ch) (vector-set! str i (##field0 ch)))

(define (make-string k (ch #\space)) (list->string (make-list k ch '())))

(define (string . args) (list->string args))

(define string=? equal?)
(define (string<? str1 str2) (##< (string-cmp str1 str2) 0))
(define (string>? str1 str2) (##< 0 (string-cmp str1 str2)))

(define string<=? (inverse-result string>?))
(define string>=? (inverse-result string<?))

(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

(define string-ci=? (transformed-call equal? string-downcase))
(define string-ci<? (transformed-call string<? string-downcase))
(define string-ci>? (transformed-call string>? string-downcase))
(define string-ci<=? (inverse-result string-ci>?))
(define string-ci>=? (inverse-result string-ci<?))

(define (substring str start end)

  (define (substring-aux str start end tail)
    (if (##< start end)
      (let ((i (##- end 1)))
        (substring-aux str start i (##cons (string-ref str i) tail)))
      (list->string tail)))

  (substring-aux str start end '()))

(define (string-append . args)
  (list->string (apply append (map string->list args))))


;; ---------------------- UTILS NOT IN R4RS ---------------------- ;;

(define (string-cmp str1 str2)
  (define (string-cmp-aux lst1 lst2)
    (if (pair? lst1)
      (if (pair? lst2)
        (let ((c1 (##field0 lst1)) (c2 (##field0 lst2)))
          (if (##< c1 c2)
            -1
            (if (##< c2 c1)
              1
              (string-cmp-aux (##field1 lst1) (##field1 lst2)))))
        1)
      (if (pair? lst2)
        -1
        0)))
  (string-cmp-aux (##field0 str1) (##field0 str2)))


(define (string-at? str sub-str start end)
  (let ((sub-str-len (string-length sub-str))
        (str-len (string-length str)))
    (and (>= str-len sub-str-len)
         (string=? (substring str start end) sub-str))))

(define (string-suffix? suffix str)
  (string-at? str suffix (- (string-length str) (string-length suffix)) (string-length str)))

(define (string-prefix? prefix str)
  (string-at? str prefix 0 (string-length prefix)))

(define (string-contains str sub-str)
  (let ((str-len (string-length str))
        (sub-str-len (string-length sub-str)))
    (let loop ((i 0))
      (and (<= i (- str-len sub-str-len))
           (or (string-at? str sub-str i sub-str-len)
               (loop (+ 1 i)))))))

(define (string-find str sub-str)
  (let ((str-len (string-length str))
        (sub-str-len (string-length sub-str)))
    (let loop ((i 0))
      (and (<= i (- str-len sub-str-len))
           (if (string-at? str sub-str i sub-str-len)
             i
             (loop (+ 1 i)))))))

(define (string-concatenate list-str (sep ""))
  (if (null? list-str)
    ""
    (let loop ((final (##field0 list-str)) (s (##field1 list-str)))
      (if (null? s)
        final 
        (loop (string-append final sep (##field0 s)) (##field1 s))))))



;; ########## I/O (R4RS section 6.10) ########## ;;

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
     "prim1(filename => {try{return fs.openSync(scm2str(filename), 'r')}catch{return FALSE}}),")

   (define-primitive
     (##get-fd-output-file filename)
     (use js/node/fs scm2str)
     "prim1(filename => {try{return fs.openSync(scm2str(filename), 'w')}catch{return FALSE}}),")

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

   (define-feature 
     ##close-output-port
     (use ##close-input-port)))

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
     }"))

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
     " , prim1 $ \\(RibForeign (RibHandle handle)) -> hClose handle >> pure ribTrue")))


(define (##close-output-port port) (##close-input-port port))

;; ---------------------- EOF & TYPES ---------------------- ;;

(define ##eof (##rib 0 0 5))

(define (eof-object? obj)
  (##eqv? obj ##eof))

(define stdin-port
  (##rib (##stdin) (##rib 0 '() #t) input-port-type)) ;; stdin

(define stdout-port
  (##rib (##stdout) #t output-port-type))  ;; stdout


;; ---------------------- INPUT ---------------------- ;;

(define (open-input-file filename)
  ;; (file_descriptor, (cursor, last_char, is_open), input_file_type)
  (##rib (##get-fd-input-file filename) (##rib 0 '() #t) input-port-type))

(define (close-input-port port)
  (if (##field2 (##field1 port))
    (begin 
      (##field2-set! (##field1 port) #f)
      (##close-input-port (##field0 port)))))

(define (##get-last-char port)
  (##field1 (##field1 port)))

(define (##set-last-char port ch)
  (##field1-set! (##field1 port) ch))

(define (input-port-close? port)
  (not (##field2 (##field1 port))))

(define (current-input-port)
  stdin-port)

(define (call-with-input-file filename proc)
  (let* ((port (open-input-file filename))
         (result (proc port)))
    (close-input-port port)
    result))

(define (read-char (port (current-input-port))) 
  (if (input-port-close? port) (crash))
  (let ((last-ch (##get-last-char port)))
    (if (null? last-ch)

      (let ((ch (##read-char (##field0 port))))
        (if (null? ch) ##eof (integer->char ch)))

      (begin
        (##set-last-char port '())
        last-ch))))

(define (peek-char (port (current-input-port)))
  (let ((ch (read-char port)))
    (##set-last-char port ch)
    ch))

;; ---------------------- READ ---------------------- ;;

(define special-chars '((newline 10) 
                        (space 32) 
                        (tab 9)
                        (return 13)))

(define escapes '((10 110)   ;; \n -> n
                  (13 116)   ;; \t -> t
                  (92 92)    ;; \\ -> \
                  (34 34)))  ;; \" -> "

(define (read (port (current-input-port)))

  (let ((c (peek-char-non-whitespace port)))
    (cond ((eof-object? c) c)
          ((##eqv? c 40)            ;; #\(
           (read-char port)
           (read-list port))
          ((##eqv? c 35)            ;; #\#
           (read-char port) ;; skip "#"
           (let ((c (##field0 (peek-char port))))
             (cond ((##eqv? c 102)  ;; #\f
                    (read-char port) ;; skip "f"
                    #f)
                   ((##eqv? c 116)     ;; #\t
                    (read-char port) ;; skip "t"
                    #t)
                   ((##eqv? c 92)        ;; #\\
                    (read-char port) ;; skip "\\"
                    (let ((ch (peek-char port))
                          (str (read-symbol port '())))
                      (cond 
                        ((##eqv? (##field1 str) 0) (read-char port))
                        ((##eqv? (##field1 str) 1) ch)
                        (else (integer->char (cadr (assq (string->symbol str) special-chars)))))))
                   ((##eqv? c 40)  ;; #\(
                     (list->vector (read port)))
                   (else 
                     (string->symbol (read-symbol port (list #\#)))))))
          ((##eqv? c 39)      ;; #\'
           (read-char port) ;; skip "'"
           (list 'quote (read port)))
          ((##eqv? c 96)      ;; #\`
           (read-char port) ;; skip "`"
           (list 'quasiquote (read port)))
          ((##eqv? c 44)      ;; #\,
           (read-char port) ;; skip ","
           (let ((c (##field0 (peek-char port))))
             (if (##eqv? c 64)  ;; #\@
               (begin
                 (read-char port) ;; skip "@"
                 (list 'unquote-splicing (read port)))
               (list 'unquote (read port)))))
          ((##eqv? c 34)      ;; #\"
           (read-char port) ;; skip """
           (##list->string (read-chars '() port)))
          (else
            ;; (read-char port) ;; skip first char
            (let ((s (read-symbol port '())))
              (let ((n (string->number s)))
                (or n
                    (string->symbol s))))))))

(define (read-list port)
  (let ((c (peek-char-non-whitespace port)))
    (if (##eqv? c 41) ;; #\)
      (begin
        (read-char port) ;; skip ")"
        '())
      (let ((first (read port)))
        (if (and (symbol? first) (equal? (symbol->string first) "."))
          (let ((result (read port)))
            (read-char port)
            result)
          (##cons first (read-list port)))))))

(define (read-symbol port lst)
  (let ((c (##field0 (peek-char port))))
    (if (or (##eqv? c 40)  ;; #\(
            (##eqv? c 41)  ;; #\)
            (##< c 33))    ;; whitespace
      (list->string (reverse lst))
      (read-symbol port (##cons (char-downcase (read-char port)) lst)))))

(define (read-chars lst port)
  (let ((c (##field0 (read-char port))))
    (cond ((##eqv? c 0) '())   ;; eof
          ((##eqv? c 34) (reverse lst))  ;; #\"
          ((##eqv? c 92)                 ;; #\\
           (let ((c2 (##field0 (read-char port))))
             (read-chars
              (##rib (cond
                     ;#; ;; support for \n in strings
                     ((##eqv? c2 110) 10) ;; #\n
                     ;#; ;; support for \r in strings
                     ((##eqv? c2 114) 13) ;; #\r
                     ;#; ;; support for \t in strings
                     ((##eqv? c2 116) 9)  ;; #\t
                     (else          c2))
                    lst 0)
              port)))
          (else
           (read-chars (##cons c lst) port)))))

(define (peek-char-non-whitespace port)
  (let ((c (peek-char port)))
    (if (eof-object? c) ;; eof?
      c
      (if (char-whitespace? c) 
        (begin
          (read-char port)
          (peek-char-non-whitespace port))
        (if (##eqv? (##field0 c) 59) ;; #\;
          (skip-comment port)
          (##field0 c))))))  ;; returns the code point of the char

(define (skip-comment port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        (if (##eqv? (##field0 c) 10) ;; #\newline
            (peek-char-non-whitespace port)
            (skip-comment port)))))


;; ---------------------- OUTPUT ---------------------- ;;

(define (open-output-file filename)
  ;; (file_descriptor, is_open, write_file_type)
  (##rib (##get-fd-output-file filename) #t output-port-type))

(define (close-output-port port)
  (if (##field1 port)
    (begin
      (##field1-set! port #f)
      (##close-output-port (##field0 port)))))

(define (current-output-port) stdout-port)

(define (output-port-close? port)
  (not (##field1 port)))

(define (call-with-output-file filename proc)
  (let* ((port (open-output-file filename))
         (result (proc port)))
    (close-output-port port)
    result))

(define (write-char ch (port (current-output-port)))
  (##write-char (##field0 ch) (##field0 port)))

(define (newline (port (current-output-port)))
  (##write-char 10 (##field0 port)))  ;; #\newline

(define (write o (port (current-output-port)))
  (let ((port-val (##field0 port)))
    (cond ((string? o)
           (##write-char 34 port-val)     ;; #\"
           (write-chars (##field0 o) escapes port-val)
           (##write-char 34 port-val))    ;; #\"
          ((char? o)
           (##write-char 35 port-val)     ;; #\#
           (##write-char 92 port-val)     ;; #\\
           (let ((name (assv (##field0 o) (map reverse special-chars)))) 
             (if name
               (write (cadr name) port)
               (##write-char (##field0 o) port-val))))
          ((pair? o)
           (##write-char 40 port-val)  ;; #\(
           (write (##field0 o) port) ;; car
           (print-list (##field1 o) write port) ;; cdr
           (##write-char 41 port-val)) ;; #\)
          ((vector? o)
           (##write-char 35 port-val)  ;; #\#
           (##write-char 40 port-val)  ;; #\(
           (if (##< 0 (##field1 o))
             (let ((l (##field0 o)))   ;; vector->list
               (write (##field0 l) port)
               (print-list (##field1 l) write port)))
           (##write-char 41 port-val)) ;; #\)
          (else
            (display o port)))))

(define (display o (port (current-output-port)))
  (let ((port-val (##field0 port)))
    (cond ((not o)
           (##write-char 35 port-val)     ;; #\#
           (##write-char 102 port-val))   ;; #f

          ((##eqv? o #t)
           (##write-char 35 port-val)     ;; #\#
           (##write-char 116 port-val))   ;; #t

          ((eof-object? o)
           (##write-char 35 port-val)     ;; #\#
           (##write-char 101 port-val))   ;; #e

          ((null? o)
           (##write-char 40 port-val)  ;; #\(
           (##write-char 41 port-val)) ;; #\)

          ((integer? o)
           (display (number->string o) port))

          ((input-port? o)
           (display (vector (##field0 o) (##field2 (##field1 o)) (##field2 o))))

          ((output-port? o)
           (display (vector (##field0 o) (##field1 o) (##field2 o))))

          ((char? o)
           (##write-char (##field0 o) port-val))

          ((pair? o)
           (##write-char 40 port-val)  ;; #\(
           (display (##field0 o) port) ;; car
           (print-list (##field1 o) display port) ;; cdr
           (##write-char 41 port-val)) ;; #\)

          ((symbol? o)
           (write-chars (##field0 (##field1 o)) '() port-val))

          ((string? o)
           (write-chars (##field0 o) '() port-val)) ;; chars

          ((vector? o)
           (##write-char 35 port-val)  ;; #\#
           (##write-char 40 port-val)  ;; #\(
           (if (##< 0 (##field1 o))
             (let ((l (##field0 o)))   ;; vector->list
               (display (##field0 l) port)
               (print-list (##field1 l) display port)))
           (##write-char 41 port-val)) ;; #\)

          ((procedure? o)
           (##write-char 35 port-val)  ;; #\#
           (##write-char 112 port-val)) ;; #p

          (else
            (crash)))))

(define (print-list lst mode port)
  (cond 
    ((pair? lst)
     (##write-char 32 (##field0 port)) ;; #\space
         (mode (##field0 lst) port) ;; car
         (print-list (##field1 lst) mode port))  ;; cdr

    ((null? lst) #f)

    (else
      (let ((port-val (##field0 port)))
        (##write-char 32 port-val) ;; #\space
        (##write-char 46 port-val) ;; #\.
        (##write-char 32 port-val)) ;; #\space
      (mode lst port))))

(define (write-chars lst escapes port-val)
  (if (pair? lst)
    (let ((escape (assq (##field0 lst) escapes)))
      (if escape
        (begin
          (##write-char 92 port-val)
          (##write-char (cadr escape) port-val))
        (##write-char (##field0 lst) port-val))
      (write-chars (##field1 lst) escapes port-val))))


;; ---------------------- UTILS NOT IN R4RS ---------------------- ;;

(define (pp arg (port (current-output-port)))
  (write arg port)
  (newline port))

(define (read-all (port (current-input-port)))
  (let ((x (read port)))
    (if (eof-object? x)
        '()
        (##cons x (read-all port)))))

(define (read-chars-until predicate (port (current-input-port)))
  (let read-chars-aux ((c (read-char port)) (result '()))
    (cond 
      ((predicate c) (list->string (reverse result)))
      ((eof-object? c) #f)
      (else (read-chars-aux (read-char port) (##cons c result))))))

(define (read-str-until predicate (port (current-input-port)))
  (let read-str-aux ((c (read-char port)) (result ""))
    (cond 
      ((predicate result) result)
      ((eof-object? c) #f)
      (else (read-str-aux (read-char port) (string-append result (string c)))))))

(define (read-line (port (current-input-port)) (sep #\newline))
  (read-chars-until (lambda (c) (or (eof-object? c) (eqv? c sep))) port))

(define (read-lines-until predicate (port (current-input-port)))
  (let loop ((line (read-line port)) (lines '()))
    (if (predicate line)
      (reverse (##cons line lines))
      (loop (read-line port) (##cons line lines)))))

(define (string-from-file filename)
  (call-with-input-file filename (lambda (port) (read-str-until eof-object? port))))

(define (file-exists? filename) (notnot (##get-fd-input-file filename)))


(define (error . msg)
  (for-each display msg)
  (newline)
  (##exit 1))

(define (crash)
  (error "(._.')"))



;; ########## Compiler from Ribbit Scheme to RVM code ########## ;;

(cond-expand
  ((host js)
   (define-primitive
     (welcome-msg)
     "() => (console.log(`
              ____________________
             |                    |
             | Welcome to Ribbit! |
             |                    |
    λ        | - Rib the Frog     |
  @...@  --- |____________________|
 (-----)
( >___< )
^^ ~~~ ^^`), true),")))

(define jump/call-op 0)
(define set-op       1)
(define get-op       2)
(define const-op     3)
(define if-op        4)

(define (add-nb-args nb tail)
  (##rib const-op
   nb
   tail))

(define (improper-length lst)
  (if (pair? lst)
    (##+ 1 (improper-length (##field1 lst)))
    0))

(define (improper-list->list lst tail)
  (if (pair? lst)
    (improper-list->list (##field1 lst) (##cons (##field0 lst) tail))
    (reverse (##cons lst tail))))

(define (last-item lst)
  (if (pair? lst)
    (last-item (##field1 lst))
    lst))

(define (comp cte expr cont)
  (cond ((symbol? expr)
         (##rib get-op (lookup expr cte 0) cont))

        ((pair? expr)
         (let ((first (##field0 expr)))
           (cond ((##eqv? first 'quote)
                  (##rib const-op (cadr expr) cont))

                 ((##eqv? first 'quasiquote)
                  (comp cte
                        (let parse ((x (cadr expr)) (depth 1)) ;; expand-qq
                          (cond 
                            ((not (pair? x))
                             (if (vector? x)
                               (##qq-list '##qq-list->vector (parse (##field0 x) depth))
                               (expand-constant x)))
                            ((##eqv? (##field0 x) 'unquote)
                             (if (##eqv? depth 1)
                               (cadr x)
                               (##qq-list '##qq-cons (expand-constant 'unquote) (parse (##field1 x) (##- depth 1)))))
                            ((and (pair? (##field0 x)) (##eqv? (caar x) 'unquote-splicing))
                             (if (##eqv? depth 1)
                               (##qq-list '##qq-append (cadar x) (parse (##field1 x) depth))
                               (##qq-list '##qq-cons (##qq-list '##qq-cons (expand-constant 'unquote-splicing) (parse (cdar x) (##- depth 1))) (parse (##field1 x) depth))))
                            ((##eqv? (##field0 x) 'quasiquote)
                             (##qq-list '##qq-cons (expand-constant 'quasiquote) (parse (##field1 x) (##+ depth 1))))
                            (else
                              (##qq-list '##qq-cons (parse (##field0 x) depth) (parse (##field1 x) depth)))))
                        cont))

                 ((or (##eqv? first 'set!) (##eqv? first 'define))
                  (let ((pattern (cadr expr)))
                    (if (pair? pattern)
                      (comp cte
                            (##cons 'lambda (##cons (##field1 pattern) (cddr expr)))
                            (gen-assign (lookup (##field0 pattern) cte 1)
                                        cont))
                      (comp cte
                            (caddr expr)
                            (gen-assign (lookup pattern cte 1)
                                        cont)))))

                 ((##eqv? first 'if)
                  (comp cte
                        (cadr expr)
                        (##rib if-op
                             (comp cte (caddr expr) cont)
                             (comp cte (cadddr expr) cont))))

                 ((##eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (variadic (or (symbol? params) (not (null? (last-item params)))))
                         (nb-params (if variadic (##+ 1 (##* 2 (improper-length params))) (##* 2 (length params))))
                         (params 
                           (if variadic
                             (improper-list->list params '())
                             params)))
                    (##rib const-op
                     (make-procedure
                       (##rib nb-params
                        0
                        (comp-begin (extend params
                                            (##cons #f
                                                  (##cons #f
                                                        cte)))
                                    (cddr expr)
                                    tail))
                       '())
                     (if (null? cte)
                       cont
                       (if-feature
                         prim-no-arity
                         (gen-call '##close cont)
                         (add-nb-args
                           1
                           (gen-call '##close cont)))))))

                 ;#; ;; support for begin special form
                 ((##eqv? first 'begin)
                  (comp-begin cte (##field1 expr) cont))

                 ;#; ;; support for let special form
                 ((##eqv? first 'let)
                  (let ((bindings (cadr expr)))
                    (comp-bind cte
                               (map car bindings)
                               (map cadr bindings)
                               (cddr expr)
                               cte
                               cont)))

                 ;#; ;; support for single armed let special form
                 ((##eqv? first 'letrec)
                  (let ((bindings (cadr expr)))
                    (comp cte
                          (##cons 'let
                                (##cons (map (lambda (binding)
                                             (list (##field0 binding) #f))
                                           bindings)
                                      (append (map (lambda (binding)
                                                     (list 'set! (##field0 binding) (cadr binding)))
                                                   bindings)
                                              (cddr expr))))
                          cont)))

                 ;#; ;; support for and special form
                 ((##eqv? first 'and)
                  (comp cte
                        (if (pair? (##field1 expr))
                          (let ((second (cadr expr)))
                            (if (pair? (cddr expr))
                              (build-if second
                                        (##cons 'and (cddr expr))
                                        #f)
                              second))
                          #t)
                        cont))

                 ;#; ;; support for or special form
                 ((##eqv? first 'or)
                  (comp cte
                        (if (pair? (##field1 expr))
                          (let ((second (cadr expr)))
                            (if (pair? (cddr expr))
                              (list 'let
                                    (list (list '_ second))
                                    (build-if '_
                                              '_
                                              (##cons 'or (cddr expr))))
                              second))
                          #f)
                        cont))

                 ;#; ;; support for cond special form
                 ((##eqv? first 'cond)
                  (comp cte
                        (if (pair? (##field1 expr))
                          (if (##eqv? (caadr expr) 'else)
                            (##cons 'begin (cdadr expr))
                            (build-if (caadr expr)
                                      (##cons 'begin (cdadr expr))
                                      (##cons 'cond (cddr expr))))
                          #f)
                        cont))

                 ((##eqv? first 'case)
                  (let ((key (##field0 (##field1 expr))))
                    (let ((clauses (##field1 (##field1 expr))))
                      (comp cte
                            (if (pair? clauses)
                              (let ((clause (##field0 clauses)))
                                (if (##eqv? (##field0 clause) 'else)
                                  (##cons 'begin (##field1 clause))
                                  (build-if (##cons 'memv (##cons key (list (list 'quote (##field0 clause)))))
                                            (##cons 'begin (##field1 clause))
                                            (##cons 'case (##cons key (##field1 clauses))))))
                              #f)
                            cont))))

                 (else
                   ;;                  #; ;; support for calls with only variable in operator position
                   ;;                  (comp-call cte
                   ;;                             (##field1 expr)
                   ;;                             (##cons first cont))
                   ;#; ;; support for calls with any expression in operator position
                   (let ((args (##field1 expr)))
                     (if (symbol? first)
                       (begin
                         #;(if-feature
                           (not no-err)
                           (if (not (procedure? (eval first)))
                             (crash (string-append
                                      "Cannot call: " 
                                      (symbol->string first)))))

                         (comp-call cte
                                    args
                                    (length args)
                                    (##cons first cont)))
                       (comp-bind cte
                                  '(_)
                                  (list first)
                                  ;;                                   #; ;; support for single expression in body
                                  ;;                                   (##cons '_ args)
                                  ;#; ;; support for multiple expressions in body
                                  (list (##cons '_ args))
                                  cte
                                  cont)))))))

        (else
          ;; self-evaluating
          (##rib const-op expr cont))))

;#; ;; support for and, or, cond special forms
(define (build-if a b c) (##cons 'if (list a b c)))

(define (expand-constant expr)
  (##qq-list 'quote expr))

(define (expand-qq expr)
  (let parse ((x expr) (depth 1))
    (cond 
      ((not (pair? x))
       (if (vector? x)
         (##qq-list '##qq-list->vector (parse (##field0 x) depth))
         (expand-constant x)))
      ((##eqv? (##field0 x) 'unquote)
       (if (##eqv? depth 1)
         (cadr x)
         (##qq-list '##qq-cons (expand-constant 'unquote) (parse (##field1 x) (##- depth 1)))))
      ((and (pair? (##field0 x)) (##eqv? (caar x) 'unquote-splicing))
       (if (##eqv? depth 1)
         (##qq-list '##qq-append (cadar x) (parse (##field1 x) depth))
         (##qq-list '##qq-cons (##qq-list '##qq-cons (expand-constant 'unquote-splicing) (parse (cdar x) (##- depth 1))) (parse (##field1 x) depth))))
      ((##eqv? (##field0 x) 'quasiquote)
       (##qq-list '##qq-cons (expand-constant 'quasiquote) (parse (##field1 x) (##+ depth 1))))
      (else
        (##qq-list '##qq-cons (parse (##field0 x) depth) (parse (##field1 x) depth))))))

(define (comp-bind cte vars exprs body body-cte cont)
  (if (pair? vars)
    (let ((var (##field0 vars))
          (expr (##field0 exprs)))
      (comp cte
            expr
            ;#; ;; support for multiple expressions in body
            (comp-bind (##cons #f cte)
                       (##field1 vars)
                       (##field1 exprs)
                       body 
                       (##cons var body-cte)
                       (if (##eqv? cont tail)
                         cont
                         (if-feature 
                           prim-no-arity
                           (##rib jump/call-op ;; call
                            '##arg2
                            cont)
                           (add-nb-args
                             2
                             (##rib jump/call-op ;; call
                              '##arg2
                              cont)))))))
    (comp-begin body-cte 
                body
                cont)))

(define (comp-begin cte exprs cont)
  (comp cte
        (##field0 exprs)
        (if (pair? (##field1 exprs))
          (if-feature 
            prim-no-arity
            (##rib jump/call-op ;; call
             '##arg1
             (comp-begin cte (##field1 exprs) cont))
            (add-nb-args
              2
              (##rib jump/call-op ;; call
               '##arg1
               (comp-begin cte (##field1 exprs) cont))))
            cont)))

(define (gen-call v cont)
  (if (##eqv? cont tail)
      (##rib jump/call-op v 0)      ;; jump
      (##rib jump/call-op v cont))) ;; call

(define (gen-assign v cont)
  (##rib set-op v 
       (if (and (##rib? cont) ;; starts with pop?
                (##eqv? (##field0 cont) jump/call-op) ;; call?
                (##eqv? (##field1 cont) '##arg1)
                (##rib? (##field2 cont)))
         (##field2 cont) ;; remove pop
         (##rib const-op 0 cont))))

;; (define (gen-noop cont)
;;   (if (and (rib? cont) ;; starts with pop?
;;            (eqv? (field0 cont) jump/call-op) ;; call?
;;            (eqv? (field1 cont) 'arg1)
;;            (rib? (field2 cont)))
;;       (field2 cont) ;; remove pop
;;       (##rib const-op 0 cont))) ;; add dummy value for set!

(define (comp-call cte exprs nb-args var-cont)
  (if (pair? exprs)
    (comp cte
          (##field0 exprs)
          (comp-call (##cons #f cte)
                     (##field1 exprs)
                     nb-args
                     var-cont))
    (let ((var (##field0 var-cont)))
      (let ((cont (##field1 var-cont)))
        (let ((v (lookup var cte 0)))
          (if-feature
            prim-no-arity
            ;; add-nb-args doesn't exist
            (##rib const-op
             nb-args
             (gen-call (if (integer? v) (##+ 1 v) v) cont))
            (add-nb-args
              nb-args
              (gen-call (if (integer? v) (##+ 1 v) v) cont))))))))

(define (lookup var cte i)
  (if (pair? cte)
      (if (##eqv? (##field0 cte) var)
          i
          (lookup var (##field1 cte) (##+ i 1)))
      var))

(define (extend vars cte)
  (if (pair? vars)
      (##cons (##field0 vars) (extend (##field1 vars) cte))
      cte))

(define tail
  (if-feature 
    prim-no-arity
    (##rib jump/call-op '##id 0)
    (add-nb-args 1 (##rib jump/call-op '##id 0)))) ;; jump

(define (eval expr)
  ((make-procedure (##rib 0 0 (comp '() expr tail)) '())))

(define (##repl-inner)
  (if-feature 
    (not quiet)
    (if-feature 
      frog-talk
      (display "\n @...@\n(-----) > ")
      (display "> ")))
  (let ((expr (read)))
    (if (eof-object? expr)
      (newline)
      (begin
        (write (eval expr))
        (newline)
        (##repl-inner)))))

(define (repl)
  (if-feature 
    (and (not hide-frog) (not quiet))
    (begin 
      (welcome-msg)
      (newline)))
  (##repl-inner)
  (##exit 0))


;; ---------------------- LOAD ---------------------- ;;

(define (load filename)
  (let ((port (open-input-file filename)))
    (let loop ((expr (read port)))
      (if (eof-object? expr)
        (begin
          (close-input-port port)
          '())
        (begin
          (eval expr)
          (loop (read port)))))))

