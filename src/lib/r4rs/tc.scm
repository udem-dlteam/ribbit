(define-macro 
  (tc-pair? o)
  `(and (##rib? ,o) (##eqv? (##field2 ,o) 0)))

(define (##tc-error . msgs)
  (let loop ((msgs msgs))
    (if (pair? msgs)
      (begin
        (set! assq (lambda (x y) #f))
        (##ntc-display (##field0 msgs))
        (loop (##field1 msgs)))
      (##exit 1))))


(define (##tc-append lst1 lst2)
  (if (pair? lst1)
    (##rib (##field0 lst1) (##tc-append (##field1 lst1) lst2) 0) ;; cons
    lst2))

(define (##tc-list . args) args)

(define (##params proc)
  (let ((nb-args-raw (##field0 (##field0 proc))))
    (cons (##ntc-odd? nb-args-raw) (##quotient nb-args-raw 2)))) ;; (cons variadic? nb-args)

(define (##can-call? proc nb-args)
  (let ((nb-params (##params proc)))
    (or (##eqv? nb-args (##field1 nb-params))
        (and (##field0 nb-params) (>= nb-args (##field1 nb-params))))))

;; ntc means "no type check"
(define-macro
  (define-signature proc args-info)
  (let ((ntc-proc (string->symbol (string-append "##ntc-" (symbol->string proc))))
        (variadic? #f))
    `(begin
       (set! ,ntc-proc ,proc)
       (define 
         ,(let loop ((args (list proc)) (rest args-info))
            (if (pair? rest)
              (let ((arg-name (caar rest))
                    (default (let ((maybe-default (memq 'default: (car rest))))
                               (and maybe-default (cadr maybe-default))))
                    (rest-param (memq 'rest-param: (car rest))))
                (if rest-param
                  (if default
                    (error "A rest param cannot have a default value. Got: " default)
                    (begin 
                      (set! variadic? #t)
                      (append (reverse args) arg-name)))
                  (loop (cons
                          (if default
                            (list arg-name default)
                            arg-name)
                          args) 
                        (cdr rest))))
              (reverse args)))
         ;; ,(if (not (eq? proc 'assq))
         ;;     `(display ',proc)
         ;;     '())
         ,@(let loop ((guards '()) (i 1) (rest args-info))
             (if (pair? rest)
               (let* ((arg-info (car rest))
                      (arg-name (car arg-info))
                      (guard (let ((maybe (memq 'guard: arg-info)))
                               (and maybe (cadr maybe))))
                      (expected (let ((maybe (memq 'expected: arg-info)))
                                  (and maybe (cadr maybe)))))
                 (if guard 
                   (if (not expected)
                     (error "You must define the 'expected' field when defining a guard")
                     (loop 
                       (cons `(if (##eqv? ,guard #f)  ;; not
                                (##tc-error "In procedure " ',proc ": (ARGUMENT " ,i ") " ,expected " expected."))
                             guards)
                       (+ i 1)
                       (cdr rest)))
                   (loop guards (+ i 1) (cdr rest))))
               (reverse guards)))
         ;; (let ((tc-proc ,proc))
         ;;   (set! ,proc ,ntc-proc)
         ;;
           ;; (let ((result 
           ,(if variadic? 
                            (let ((reverse-args (reverse args-info)))
                              `(##apply ,ntc-proc (##tc-append (##tc-list ,@(reverse (map car (cdr reverse-args)))) ,(caar reverse-args))))
                            `(,ntc-proc ,@(map car args-info)))
           ;; ))
             ;; (set! ,proc tc-proc)
             ;; result))
    ))))


(define-macro
  (define-signatures procs common-signature)
  `(begin 
     ,@(map (lambda (proc) `(define-signature ,proc ,common-signature)) procs)))


;; ########## Types (R4RS section 3.4 + others) ########## ;;

(define-signature
  string-append
  ((strs 
     rest-param:
     guard: (all string? strs)
     expected: "STRINGs")))

(define-signature
  char->integer 
  ((x
     guard: (char? x)
     expected: "CHARACTER")))

(define-signature
  integer->char 
  ((n
     guard: (integer? n)
     expected: "INTEGER")))

(define-signatures
  (list->string list->vector)
  ((lst 
     guard: (list? lst)
     expected: "LIST")))

(define-signatures
  (string->list string->symbol)
  ((str 
     guard: (string? str)
     expected: "STRING")))

(define-signature
  vector->list 
  ((x
     guard: (vector? x)
     expected: "VECTOR")))

(define-signature
  symbol->string 
  ((x
     guard: (symbol? x)
     expected: "SYMBOL")))

(define-signature 
  number->string 
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (radix 
     default: 10
     guard: (##ntc-memv radix '(2 8 10 16))
     expected: "Either 2, 8, 10, or 16")))

(define-signature 
  string->number 
  ((str
     guard: (string? str)
     expected: "STRING")
   (radix 
     default: 10
     guard: (##ntc-memv radix '(2 8 10 16))
     expected: "Either 2, 8, 10, or 16")))

;; ########## Pairs and lists (R4RS section 6.3) ########## ;;

(define-signatures
  (car cdr)
  ((x
     guard: (pair? x)
     expected: "PAIR")))

(define-signatures
  (set-car! set-cdr!)
  ((x
     guard: (pair? x)
     expected: "PAIR")
   (value)))

(define-signatures
  (length reverse)
  ((lst 
     guard: (list? lst)
     expected: "LIST")))

(define-signature 
  append
  ((lst 
     rest-param:
     guard: (or (null? lst) (all list? (##field1 (##ntc-reverse lst))))
     expected: "All LISTs except the last arg")))

(define-signatures
  (list-ref list-tail)
  ((lst 
     guard: (list? lst)
     expected: "LIST")
   (i
     guard: (and (integer? i) (< -1 i (##ntc-length lst)))
     expected: (##ntc-string-append "INTEGER between 0 and " (##ntc-number->string (##ntc-length lst))))))

(define-signatures
  (member memv memq)
  ((x)
   (lst 
     guard: (list? lst)
     expected: "LIST")))

(define-signatures
  (assoc assq assv)
  ((x)
   (lst 
     guard: (and (list? lst) (all pair? lst))
     expected: "LIST of PAIRs")))

;; ########## Numbers (R4RS section 6.5) ########## ;;

(define-signatures
  (+ *)
  ((args
     rest-param:
     guard: (all number? args)
     expected: "NUMBERs")))

(define-signatures
  (- /)
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (y
     rest-param:
     guard: (all number? y)
     expected: "NUMBERs")))

(define-signature 
  quotient 
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (y
     guard: (number? y)
     expected: "NUMBER")))

(define-signatures
  (= < > <= >=)
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (rest
     rest-param:
     guard: (all number? rest)
     expected: "NUMBERs")))

(define-signatures
  (zero? positive? negative? even? odd? exact? inexact?)
  ((x
     guard: (number? x)
     expected: "NUMBER")))

(define-signatures
  (max min)
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (rest
     rest-param:
     guard: (all number? rest)
     expected: "NUMBERs")))

(define-signature
  abs
  ((x
     guard: (number? x)
     expected: "NUMBER")))

(define-signatures
  (remainder modulo)
  ((x
     guard: (number? x)
     expected: "NUMBER")
   (y
     guard: (number? y)
     expected: "NUMBER")))

(define-signatures
  (lcm gcd)
  ((args
     rest-param:
     guard: (all number? args)
     expected: "NUMBERs")))

;; ########## Vectors (R4RS section 6.8) ########## ;; 

(define-signature
  vector-length
  ((x
     guard: (vector? x)
     expected: "VECTOR")))

(define-signature
  vector-ref
  ((vect 
     guard: (vector? vect)
     expected: "VECTOR")
   (i
     guard: (and (integer? i) (< -1 i (##ntc-vector-length vect)))
     expected: (##ntc-string-append "INTEGER between 0 and " (##ntc-number->string (##ntc-vector-length vect))))))

(define-signature
  vector-set!
  ((vect 
     guard: (vector? vect)
     expected: "VECTOR")
   (i
     guard: (and (integer? i) (< -1 i (##ntc-vector-length vect)))
     expected: (##ntc-string-append "INTEGER between 0 and " (##ntc-number->string (##ntc-vector-length vect))))
   (x)))

(define-signature
  make-vector
  ((k
     guard: (integer? k)
     expected: "INTEGER")))

;; ########## Characters (R4RS section 6.6) ########## ;;

(define-signatures
  (char=? char<? char>? char<=? char>=? char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?)
  ((ch1
     guard: (char? ch1)
     expected: "CHARACTER")
   (ch2
     guard: (char? ch2)
     expected: "CHARACTER")))

(define-signatures
  (char-upcase char-downcase char-alphabetic? char-numeric? char-lower-case? char-upper-case? char-whitespace?)
  ((ch
     guard: (char? ch)
     expected:"CHARACTER")))

;; ########## Strings (R4RS section 6.7) ########## ;;

(define-signature
  string-length
  ((x
     guard: (string? x)
     expected: "STRING")))

(define-signature
  string-ref
  ((str 
     guard: (string? str)
     expected: "STRING")
   (i
     guard: (< -1 i (##field1 str))
     expected: (##ntc-string-append "A NUMBER between 0 and " (##ntc-number->string (##field1 str))))))

(define-signature
  string-set!
  ((str 
     guard: (string? str)
     expected: "STRING")
   (i
     guard: (and (integer? i) (< -1 i (##field1 str)))
     expected: (##ntc-string-append "INTEGER between 0 and " (##ntc-number->string (##field1 str))))
   (ch
     guard: (char? ch)
     expected: "CHARACTER")))

(define-signature
  make-string
  ((k
     guard: (integer? k)
     expected: "INTEGER")
   (ch
     default: #\space
     guard: (char? ch)
     expected: "CHARACTER")))

(define-signature
  string
  ((chars 
     rest-param:
     guard: (all char? chars)
     expected: "CHARACTERs")))

(define-signatures
  (string=? string<? string>? string<=? string>=? string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?)
  ((str1 
     guard: (string? str1)
     expected: "STRING")
   (str2 
     guard: (string? str2)
     expected: "STRING")))

(define-signature
  substring
  ((str 
     guard: (string? str)
     expected: "STRING")
   (start 
     guard: (and (integer? start) (<= 0 start end))
     expected: (##ntc-string-append "INTEGER between 0 and the end value (" (##ntc-number->string end) ")"))
   (end 
     guard: (and (integer? end) (<= end (##field1 str)))
     expected: (##ntc-string-append "INTEGER between the start value (" 
                              (##ntc-number->string start) ") and " 
                              (##ntc-number->string  (##field1 str))))))


;; ########## I/O (R4RS section 6.10) ########## ;;

(define-signature
  open-input-file
  ((filename 
     guard: (and (string? filename) (file-exists? filename))
     expected: "STRING representing an existing file")))

(define-signature
  open-output-file
  ((filename 
     guard: (string? filename)
     expected: "STRING representing a file")))

(define-signature 
  close-input-port 
  ((port 
     guard: (input-port? port)
     expected: "INPUT-PORT")))

(define-signature 
  close-output-port 
  ((port 
     guard: (output-port? port)
     expected: "OUTPUT-PORT")))

(define-signature
  call-with-input-file
  ((filename 
     guard: (and (string? filename) (file-exists? filename))
     expected: "STRING representing an existing file")
   (proc
     guard: (and (procedure? proc) (##can-call? proc 1))
     expected: "PROCEDURE of one argument (the input port)")))

(define-signature
  call-with-output-file
  ((filename 
     guard: (string? filename)
     expected: "STRING representing a file")
   (proc
     guard: (and (procedure? proc) (##can-call? proc 1))
     expected: "PROCEDURE of one argument (the output port)")))

(define-signatures
  (read-char peek-char read)
  ((port 
     default: (current-input-port)
     guard: (input-port? port)
     expected: "INPUT-PORT")))

(define-signature 
  write-char 
  ((ch
     guard: (char? ch)
     expected: "CHARACTER")
   (port 
     default: (current-output-port)
     guard: (output-port? port)
     expected: "OUTPUT-PORT")))

(define-signature 
  newline 
  ((port 
     default: (current-output-port)
     guard: (output-port? port)
     expected: "OUTPUT-PORT")))

(define-signatures
  (display write)
  ((o)
   (port 
     default: (current-output-port)
     guard: (output-port? port)
     expected: "OUTPUT-PORT")))


;; ########## Control (R4RS section 6.9) ########## ;;

(define-signature
  apply
  ((f 
     guard: (and (procedure? proc) (##can-call? f (length args)))
     expected: (let ((params (##params f)))
                 (##ntc-string-append "PROCEDURE called with " 
                                (##ntc-number->string (##ntc-length args)) " and taking " 
                                (if (##field0 params) "at least " "")
                                (##ntc-number->string (##field1 params))
                                ". A PROCEDURE with a number of params equal to the number of args" )))

   (args 
     guard: (list? args)
     expected: "LIST")))

(define-signatures
  (map for-each)
  ((proc 
     guard: (and (procedure? proc) (##can-call? proc (length lsts)))
     expected: "A PROCEDURE that takes a number of args equal to the number of LISTs")

   (lsts 
     rest-param:
     guard: (or (null? lsts) 
                (and (all list? lsts) 
                     (##scan-until-false 
                      (lambda (lst1 lst2) (##eqv? (##ntc-length lst1) (##ntc-length lst2)))
                      (##field0 lsts)
                      #t
                      (##field1 lsts))))
     expected: "LISTs with the same length")))


(define-signature 
  call/cc
  ((receiver 
     guard: (and (procedure? receiver) (##can-call? receiver 1))
     expected: "A PROCEDURE that takes a one argument (a PROCEDURE)")))


