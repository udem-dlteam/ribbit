;;;
;;; compatibility.scm - compaibility layer for Ribbit Scheme
;;;
;;; The Ribbit Scheme Compiler (rsc) and all its files are distributed
;;; under the terms of the BSD 3-Clause License. See the file LICENSE
;;; for details.
;;;
;;; This file adds compatibility functions for all scheme compilers,
;;; including Ribbit.

;; Tested with Gambit v4.7.5 and above.
;; Others used to work, but need to be retested :
;;  ->  Guile 3.0.7, Chicken 5.2.0 and Kawa 3.1

;; Fix bug with R4RS symbols with Gambit
(cond-expand
  (gambit
    (|##meta-info| script-line "gsi -:r4rs"))
  ((and chicken compiling)

   (declare
     (block)
     (fixnum-arithmetic)
     (usual-integrations))))


;; shell-cmd and del-file
(cond-expand

  (gambit

   (define (shell-cmd command)
     (shell-command command))

   (define (del-file path)
     (delete-file path)))

  (guile

   (define (shell-cmd command)
     (system command))

   (define (del-file path)
     (delete-file path)))

  (chicken

   (import (chicken process) (chicken file))

   (define (shell-cmd command)
     (system command))

   (define (del-file path)
     (delete-file path)))

  (kawa

   (define (shell-cmd command)
     (system command))

   (define (del-file path)
     (delete-file path)))

  (ribbit
    (begin))

  (else
   (define (shell-cmd command)
     #f)

   (define (del-file path)
     #f)))

;; pipe-through
(cond-expand

  ((or gambit
       guile
       chicken
       kawa)

   (define (pipe-through program output)
     (let ((tmpin  "rsc.tmpin")
           (tmpout "rsc.tmpout"))
       (call-with-output-file
           tmpin
         (lambda (port) (display output port)))
       (shell-cmd (string-append
                   program
                   (string-append
                    " < "
                    (string-append
                     tmpin
                     (string-append " > " tmpout)))))
       (let ((out
              (call-with-input-file
                  tmpout
                (lambda (port) (read-line port #f)))))
         (del-file tmpin)
         (del-file tmpout)
         out))))


  (else

   (define (pipe-through program output)
     (display "*** Minification is not supported with this Scheme system\n")
     (display "*** so the generated code was not minified.\n")
     (display "*** You might want to try running ")
     (display program)
     (display " manually.\n")
     output)))

;; cmd-line
(cond-expand

  (chicken

   (import (chicken process-context))

   (define (cmd-line)
     (cons (program-name) (command-line-arguments))))

  (else

   (define (cmd-line)
     (command-line))))

;; exit-program-normally
(cond-expand

  (else

   ;; It seems "exit" is pretty universal but we put it in a
   ;; cond-expand in case some Scheme implementation does it
   ;; differently.

   (define (exit-program-normally)
     (exit 0))

   (define (exit-program-abnormally)
     (exit 1))))

;; with-output-to-str
(cond-expand

  (gambit

   (define (with-output-to-str thunk)
     (with-output-to-string "" thunk)))

  (chicken

   (import (chicken port))

   (define (with-output-to-str thunk)
     (with-output-to-string thunk)))

  (kawa

   (define (with-output-to-str thunk)
     (call-with-output-string
      (lambda (port)
        (parameterize ((current-output-port port))
          (thunk))))))

  (else

   (define (with-output-to-str thunk)
     (with-output-to-string thunk))))

; hash tables (make-table, table-ref, table-set!, table-length, table->list)
(cond-expand

 (gambit (begin))

 (kawa

  (import (rnrs hashtables))

  (define (make-table)
    (make-hashtable symbol-hash symbol=?))

  (define (table-ref table key default)
    (hashtable-ref table key default))

  (define (table-set! table key value)
    (hashtable-set! table key value))

  (define (table-length table)
    (hashtable-size table))

  (define (table->list table)
    (let-values (((keys entries) (hashtable-entries table)))
      (vector->list (vector-map cons keys entries)))))

 (else

     (define (make-table)
       (cons '() '()))

   (define (table-ref table key default)
     (let ((x (assoc key (car table))))
       (if x
           (cdr x)
           default)))

   (define (table-set! table key value)
     (let ((x (assoc key (car table))))
       (if x
           (set-cdr! x value)
           (set-car! table
                     (cons (cons key value) (car table))))))

   (define (table-length table)
     (length (car table)))

   (define (table->list table)
     (car table))))

;; symbol->str
(cond-expand

  ((or gambit chicken ribbit)

   (define (symbol->str symbol)
     (symbol->string symbol))

   (define (str->uninterned-symbol string)
     (string->uninterned-symbol string)))

  (kawa

   (define (symbol->str symbol)
     (symbol->string symbol))

   (define (str->uninterned-symbol string)
     (symbol string #f)))

  (else

   (define uninterned-symbols (make-table))

   (define (str->uninterned-symbol string)
     (let* ((name
             (string-append "@@@" ;; use a "unique" prefix
                            (number->string
                             (table-length uninterned-symbols))))
            (sym
             (string->symbol name)))
       (table-set! uninterned-symbols sym string) ;; remember "real" name
       sym))

   (define (symbol->str symbol)
     (table-ref uninterned-symbols symbol (symbol->string symbol)))))

;; rsc-path-extension, rsc-path-directory
(cond-expand

 (gambit

  (define (rsc-path-extension path)
    (path-extension path))

  (define (rsc-path-directory path)
    (path-directory path)))

 (chicken

  (import (chicken pathname))

  (define (rsc-path-extension path)
    (let ((ext (pathname-extension path)))
      (if ext (string-append "." ext) "")))

  (define (rsc-path-directory path)
    (let ((dir (pathname-directory path)))
      (if dir dir "")))

  (define (path-expand path dir)
    (make-pathname dir path)))

 (kawa

  (define (rsc-path-extension path)
    (let ((ext (path-extension path)))
      (if ext (string-append "." ext) "")))

  (define (rsc-path-directory path)
    (path-directory path))

  (define (path-expand path::string dir::string)
    (if (= (string-length dir) 0)
        path
        (let ((p (java.nio.file.Path:of dir path)))
          (p:toString)))))

 (else

   (define (rsc-path-extension path)
     (let loop ((i (- (string-length path) 1)))
       (if (< i 0)
           ""
           (if (eqv? (string-ref path i) #\.)
               (substring path i (string-length path))
               (loop (- i 1))))))

   (define (rsc-path-directory path)
     (let loop ((i (- (string-length path) 1)))
       (if (< i 0)
           "./"
           (if (eqv? (string-ref path i) #\/)
               (substring path 0 (+ i 1))
               (loop (- i 1))))))

   (define (string-end string)
     (string-ref string (- (string-length string) 1)))

   (define (path-directory path)
     (if (eqv? (string-end path) #\/)
       path
       (let ((len (string-length path)))
         (let loop ((i (- len 1)))
           (if (or (eqv? (string-ref path i) #\/) (eqv? i 0))
             (substring path 0 i)
             (loop (- i 1)))))))

   (define (path-expand path dir)
     (if (or (= (string-length dir) 0) (string-prefix? dir path))
         path
         (if (eqv? (string-ref dir (- (string-length dir) 1)) #\/)
             (string-append dir path)
             (string-append dir (string-append "/" path)))))))

(define (path-normalize path)
     (let loop ((path path))
       (let ((path (string-replace (string-replace path "//" "/") "/./" "/")))
         (if (string-prefix? "./" path)
           (loop (substring path 2 (string-length path)))
           (if (string-prefix? "../" path)
             (loop (substring path 3 (string-length path)))
             path)))))

;; read-line
(cond-expand

 (gambit (begin))

 (else

   (define (read-line port sep)
     (let loop ((rev-chars '()))
       (let ((c (read-char port)))
         (if (or (eof-object? c) (eqv? c sep))
             (list->string (reverse rev-chars))
             (loop (cons c rev-chars))))))

   (define (pp obj)
     (write obj)
     (newline))))

;; list-sort! and list-sort
(cond-expand

  ((and gambit (or enable-bignum disable-bignum))) ;; recent Gambit?

  (chicken

   (import (chicken sort))

   (define (list-sort! compare list)
     (sort! list compare))

   (define (list-sort compare list)
     (sort list compare)))

  (else

   (define (list-sort! compare list)

     ;; Stable mergesort algorithm

     (define (sort list len)
       (if (= len 1)
           (begin
             (set-cdr! list '())
             list)
           (let ((len1 (quotient len 2)))
             (let loop ((n len1) (tail list))
               (if (> n 0)
                   (loop (- n 1) (cdr tail))
                   (let ((x (sort tail (- len len1))))
                     (merge (sort list len1) x)))))))

     (define (merge list1 list2)
       (if (pair? list1)
           (if (pair? list2)
               (let ((x1 (car list1))
                     (x2 (car list2)))
                 (if (compare x2 x1)
                     (merge-loop list2 list2 list1 (cdr list2))
                     (merge-loop list1 list1 (cdr list1) list2)))
               list1)
           list2))

     (define (merge-loop result prev list1 list2)
       (if (pair? list1)
           (if (pair? list2)
               (let ((x1 (car list1))
                     (x2 (car list2)))
                 (if (compare x2 x1)
                     (begin
                       (set-cdr! prev list2)
                       (merge-loop result list2 list1 (cdr list2)))
                     (begin
                       (set-cdr! prev list1)
                       (merge-loop result list1 (cdr list1) list2))))
               (begin
                 (set-cdr! prev list1)
                 result))
           (begin
             (set-cdr! prev list2)
             result)))

     (let ((len (length list)))
       (if (= 0 len)
           '()
           (sort list len))))

   (define (list-sort compare list)
     (list-sort! compare (append list '())))))

;; list->string*
;;
;; Can be redefined by ribbit to make this function somewhat fast. It would
;; only be (rib lst len string-type)
(cond-expand
  (ribbit
    (define (list->string* lst len)
      (rib lst len string-type)))

  (else
    (define (list->string* lst len)
      (let ((str (make-string len (integer->char 48))))
        (let loop ((lst lst) (i 0))
          (if (< i len)
            (begin
              (string-set! str i (integer->char (car lst)))
              (loop (cdr lst) (+ i 1)))
            str))))))

(cond-expand
  (ribbit
    (define (string->list* str)
      (field0 str)))

  (else
    (define (string->list* str)
      (map char->integer (string->list str)))))

;; script-file, executable-path
(cond-expand

 (gambit (begin))

 (chicken

  (define (script-file)
    (program-name))

  (define (executable-path)
    (executable-pathname)))

 (ribbit
   (define (script-file)
     (car (command-line))))

 (else
   (define (script-file)
     (car (cmd-line)))

   (define (executable-path)
     "")))

;; string-concatenate
(cond-expand

  ((and gambit ;; hack to detect recent Gambit version
        (or enable-sharp-dot disable-sharp-dot)))

  (chicken

   (import (chicken string))

   (define (string-concatenate string-list separator)
     (string-intersperse string-list separator)))

  (kawa

   (define (string-concatenate string-list separator)
     (string-join string-list separator)))

  (ribbit (begin))

  (else

    (define (string-concatenate string-list separator)
      (if (pair? string-list)
        (let ((rev-string-list (reverse string-list))
              (sep (string->list separator)))
          (let loop ((lst (cdr rev-string-list))
                     (result (string->list (car rev-string-list))))
            (if (pair? lst)
              (loop (cdr lst)
                    (append (string->list (car lst))
                            (append sep
                                    result)))
              (list->string result))))
        ""))))

(define (string-find-char str char)
  (let ((len (string-length str)))
    (let loop ((i 0) (str (string->list str)))
      (if (< i len)
        (if (char=? (car str) char)
          i
          (loop (+ i 1) (cdr str)))
        #f))))

;; string-split
(cond-expand

  (ribbit (begin))

  (else

    (define (string-split str pattern)
      (let ((pattern? (if (char? pattern) (lambda (c) (char=? c pattern)) pattern))
            (final (list "")))
        (for-each
          (lambda (c)
            (if (pattern? c)
              (set! final (cons "" final))
              (set! final (cons (string-append (car final) (string c)) (cdr final)))))
          (string->list str))
        (reverse final)))

    ))


;;; Ribs emulation as vectors for other implementations than Ribbit. 
(cond-expand

  (ribbit
    (define (c-procedure? o) (and (c-rib? o) (eqv? (c-rib-next o) procedure-type))))

  (else

   (define pair-type      0)
   (define procedure-type 1)
   (define symbol-type    2)
   (define string-type    3)
   (define vector-type    4)
   (define singleton-type 5)
   (define char-type      6)

   (define (instance? type) (lambda (o) (and (rib? o) (eqv? (field2 o) type))))

   (define rib-tag (cons '() '())) ;; make unique tag

   (define (rib field0 field1 field2)
     (let ((r (make-vector 4)))
       (vector-set! r 0 field0)
       (vector-set! r 1 field1)
       (vector-set! r 2 field2)
       (vector-set! r 3 rib-tag)
       r))

   (define (rib? o) (and (vector? o) (= (vector-length o) 4) (eq? (vector-ref o 3) rib-tag)))
   (define (field0 o) (vector-ref o 0))
   (define (field1 o) (vector-ref o 1))
   (define (field2 o) (vector-ref o 2))
   (define (field0-set! o x) (vector-set! o 0 x) o)
   (define (field1-set! o x) (vector-set! o 1 x) o)
   (define (field2-set! o x) (vector-set! o 2 x) o)

   (define c-procedure? (instance? procedure-type))))


(define (c-make-procedure code env) (c-rib code env procedure-type))
(define (c-procedure-code proc) (c-rib-oper proc))
(define (c-procedure-env proc) (c-rib-opnd proc))





