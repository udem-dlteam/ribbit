#!/usr/bin/env gsi
;;!#;; satisfy guile

;;; The Ribbit Scheme Compiler (rsc). This file is distributed under the 
;;; terms of the BSD 3-Clause License. See the file LICENSE for details.

;;;----------------------------------------------------------
;;;================== COMPATIBILITY LAYER ===================
;;;----------------------------------------------------------

;; Tested with Gambit v4.7.5 and above.
;; Others used to work, but need to be retested :
;;  ->  Guile 3.0.7, Chicken 5.2.0 and Kawa 3.1

;; Fix bug with R4RS symbols with Gambit
(cond-expand
  (gambit
   (|##meta-info| script-line "gsi -:r4rs")))


(cond-expand
 ((and chicken compiling)

  (declare
   (block)
   (fixnum-arithmetic)
   (usual-integrations)))

 (else))

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

(cond-expand

  (chicken

   (import (chicken process-context))

   (define (cmd-line)
     (cons (program-name) (command-line-arguments))))

  (else

   (define (cmd-line)
     (command-line))))

(cond-expand

  (else

   ;; It seems "exit" is pretty universal but we put it in a
   ;; cond-expand in case some Scheme implementation does it
   ;; differently.

   (define (exit-program-normally)
     (exit 0))

   (define (exit-program-abnormally)
     (exit 1))))

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

(cond-expand

 (gambit (begin))

 ;; (ribbit
 ;;   (begin))

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

;; Can be redefined by ribbit to make this function somewhat fast. It would only be (rib lst len string-type)
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

;;;------------------------------------------------------------------------------

(define predefined (list '##rib 'false 'true 'nil)) ;; predefined symbols

(define default-primitives-lst
`((##rib         0  x  y  z)
  (##id          1  x)
  (##arg1        2  x  y)
  (##arg2        3  x  y)
  (##close       4  rib)
  (##rib?        5  o)
  (##field0      6  x)
  (##field1      7  x)
  (##field2      8  x)
  (##field0-set! 9  x  v)
  (##field1-set! 10 x  v)
  (##field2-set! 11 x  v)
  (##eqv?        12 o1 o2)
  (##<           13 x  y)
  (##+           14 x  y)
  (##-           15 x  y)
  (##*           16 x  y)
  (##quotient    17 x  y)
  (##getchar     18 )
  (##putchar     19 c)
  (##exit        20 code)))

(define default-primitives
  (map
    (lambda (p) `(primitive () ,(cons (car p) (cddr p)) (@@index ,(cadr p))))
    default-primitives-lst))


(define jump/call-op 'jump/call)
(define set-op       'set)
(define get-op       'get)
(define const-op     'const)
(define if-op        'if)

;;;----------------------------------------------------------------------------



;;; -----------------------------------------------
;;; ==================== DEBUG ====================
;;; -----------------------------------------------

;; Displays a rib to stdout given a depth
(define (display-rib rib depth)
  (if (> depth 0)
    (begin
      (display "[")
      (cond ((not (rib? rib))
             (display rib))
            ((rib? (field0 rib))
             (display-rib (field0 rib) (- depth 1)))
            (else
              (display (field0 rib))))
      (display " ")
      (cond ((not (rib? rib))
             (display rib))
            ((rib? (field1 rib))
             (display-rib (field1 rib) (- depth 1)))
            (else
              (display (field1 rib))))
      (display " ")
      (cond ((not (rib? rib))
             (display rib))
            ((rib? (field2 rib))
             (display-rib (field2 rib) (- depth 1)))
            (else
              (display (field2 rib))))
      (display "]"))
    (display "...")))


(define (pp-return foo . x)
  (let ((r (apply foo x)))
    (pp r)
    r))

(define (display-return foo . x)
  (let ((r (apply foo x)))
    (display r)
    (newline)
    r))

;;; -----------------------------------------------
;;; ==================== UTILS ====================
;;; -----------------------------------------------

;; Return the value (or false) of a key in an association list
(define (assq-value element lst)
  (let ((maybe-pair (assq element lst)))
    (and maybe-pair (cdr maybe-pair))))

(define (last-item lst)
  (if (pair? lst)
    (last-item (cdr lst))
    lst))

(define (improper-list? lst)
  (if (pair? lst)
    (improper-list? (cdr lst))
    (not (null? lst))))

(define (improper-length lst)
  (if (pair? lst)
    (+ 1 (improper-length (cdr lst)))
    0))

(define (improper-list->list lst1 lst2)
  (if (pair? lst1)
    (improper-list->list (cdr lst1) (cons (car lst1) lst2))
    (reverse (cons lst1 lst2))))


;;; -----------------------------------
;;; ====== HOST LANGUAGE CONTEXT ======
;;; -----------------------------------
;;; The host config is a data structure that holds any of the host-specific
;;; information. This includes the primitives, the features, and the locations.

;; these primitives are "forced first" meaning that they must exist before any
;;     other code is executed. This is because the compiler uses them when
;;     generating code. It's a hack.

(define forced-first-primitives (list '##- '##arg1))

;; host config definitions
(define (make-host-config live-features primitives feature-locations)
  (rib live-features (cons (list '##rib 0) primitives) feature-locations))

(define (host-config-features host-config) (field0 host-config))
(define (host-config-primitives host-config) (field1 host-config))
(define (host-config-locations host-config) (field2 host-config))

(define (host-config-features-set! host-config x)
  (field0-set! host-config x))

(define (host-config-primitives-set! host-config x)
  (field1-set! host-config x))

(define (host-config-locations-set! host-config x)
  (field2-set! host-config x))

(define next-prim-index 1)
(define (host-config-add-primitive-index! host-config prim code index)
  (let* ((primitives (host-config-primitives host-config))
         (prim-ref (assq prim primitives)))
    (if prim-ref
      (cadr prim-ref)
      (begin
        (host-config-primitives-set! host-config (cons (list prim index code) primitives))
        (set! next-prim-index (max next-prim-index index))
        index))))

(define (host-config-add-primitive! host-config prim code)
  (let* ((primitives (host-config-primitives host-config))
         (prim-ref (assq prim primitives)))
    (if (eqv? prim '##rib)
      (begin
        (set-cdr! (cdr prim-ref) (cons code '())) ;; hack to set code
        0)
      (if prim-ref
        (cadr prim-ref)
        (begin
          (host-config-primitives-set! host-config (cons (list prim next-prim-index code) primitives))
          (set! next-prim-index (+ 1 next-prim-index))
          (length primitives))))))

(define (host-config-add-location! host-config location feature)
  (let ((location-ref (assoc location (host-config-locations host-config))))
    (if location-ref
      (set-cdr! location-ref (append (cdr location-ref) feature))
      (begin
        (host-config-locations-set! host-config (cons (list location feature) (host-config-locations host-config)))
        #t))))


(define (host-config-feature-live? host-config feature)
  (assq-value feature (host-config-features host-config)))

(define (host-config-feature-add! host-config feature value)
  (if (host-config-feature-live? host-config feature)
    (error "Feature already defined" feature)
    (host-config-features-set! 
      host-config 
      (cons (cons feature value) (host-config-features host-config)))))

(define (host-config-is-primitive? host-config name)
  (or (memq name forced-first-primitives)
      ;; FIXME this code assumes that primitives are features
      (host-config-feature-live? host-config name)
      ;;(assoc name (host-config-primitives host-config))
      ))


(define (host-ctx-get-primitive-index host-ctx prim)
  (let ((prim-rib (assoc prim (host-ctx-primitive-order host-ctx))))
    (if prim-rib
      (cadr prim-rib)
      (error "Unknown primitive" prim))))



;;; ------------------------------
;;; ======= HASHABLE RIBs ========
;;; ------------------------------


(cond-expand

  (ribbit
    (define c-rib-type -1)

    (define hash-table-c-ribs (make-table))

    (define c-rib? (instance? c-rib-type))

    (define (make-c-rib field0 field1 field2 hash)
      (rib field0 (rib field1 hash field2) c-rib-type))

    (define (c-rib-oper c-rib) (field0 c-rib))
    (define (c-rib-opnd c-rib) (field0 (field1 c-rib)))
    (define (c-rib-next c-rib) (field2 (field1 c-rib)))
    (define (c-rib-hash c-rib) (field1 (field1 c-rib)))

    (define (c-rib-oper-set! c-rib v) (field0-set! c-rib v))
    (define (c-rib-opnd-set! c-rib v) (field0-set! (field1 c-rib) v))
    (define (c-rib-next-set! c-rib v) (field2-set! (field1 c-rib) v)))

  (else

    (define hash-table-c-ribs (make-table))

    (define c-rib? rib?)
    (define (make-c-rib field0 field1 field2 hash)
      (rib field0 (rib field1 hash 0) field2))

    (define (c-rib-oper c-rib) (field0 c-rib))
    (define (c-rib-opnd c-rib) (field0 (field1 c-rib)))
    (define (c-rib-next c-rib) (field2 c-rib))
    (define (c-rib-hash c-rib) (field1 (field1 c-rib)))

    (define (c-rib-oper-set! c-rib v) (field0-set! c-rib v))
    (define (c-rib-opnd-set! c-rib v) (field0-set! (field1 c-rib) v))
    (define (c-rib-next-set! c-rib v) (field2-set! c-rib v))))

;; Creates a rib that is unique and hashable
(define (c-rib field0 field1 field2)
  (let* ((hash-table hash-table-c-ribs)
         (hash (hash-c-rib field0 field1 field2))
         (hash-list (table-ref hash-table hash #f))
         (c-rib-ref (make-c-rib field0 field1 field2 hash)))

    (if hash-list
      (let search ((search-iter hash-list))
        (if (pair? search-iter)
          (if (c-rib-eq? c-rib-ref (car search-iter))
            (car search-iter)
            (search (cdr search-iter)))
          (begin
            (table-set! hash-table hash (cons c-rib-ref hash-list))
            c-rib-ref)))
      (begin
        (table-set! hash-table hash (cons c-rib-ref '()))
        c-rib-ref))))

;; Hash combine (taken from Gambit Scheme) https://github.com/gambit/gambit/blob/master/lib/_system%23.scm
;; The FNV1a hash algorithm is adapted to hash values, in
;; particular the hashing constants are used (see
;; https://tools.ietf.org/html/draft-eastlake-fnv-12).  Because the
;; hash function result is a fixnum and it needs to give the same
;; result on 32 bit and 64 bit architectures, the constants are
;; adapted to fit in a 32 bit fixnum.

;; FNV1a 32 bit constants
(define fnv1a-prime-32bits   16777619)
(define max-fixnum         4294967296)


(define (hash-combine a b)
  (modulo
    (* fnv1a-prime-32bits
       (+ a b))
    max-fixnum))

(define (hash-string str)
  (fold hash-combine 0 (string->list* str)))

(define (c-rib-eq? c-rib1 c-rib2)
  (let ((op1   (c-rib-oper c-rib1))
        (op2   (c-rib-oper c-rib2))
        (opnd1 (c-rib-opnd c-rib1))
        (opnd2 (c-rib-opnd c-rib2))
        (next1 (c-rib-next c-rib1))
        (next2 (c-rib-next c-rib2))
        (hash1 (c-rib-hash c-rib1))
        (hash2 (c-rib-hash c-rib2)))

    (and
      (or (not hash1)  ;; check if hashes are =. If not, we skip
          (not hash2)
          (eqv? hash1 hash2))

      (or
        (eqv? op1 op2) ;;test operand
        (and (c-rib? op1) (c-rib? op2) (c-rib-eq? op1 op2)))

      (or  ;; test opnd
        (eqv? opnd1 opnd2)
        (and (c-rib? opnd1) (c-rib? opnd2) (c-rib-eq? opnd1 opnd2)))
      (or ;; test next
        (eqv? next1 next2)
        (and (c-rib? next1) (c-rib? next2) (c-rib-eq? next1 next2))))))

(define table-hash-size 32000)


(define (hash-c-rib field0 field1 field2)

  ;; This is a really simple hashing function. I tested it on the 50-repl test and I got good results
  ;;   having at most 6 elements hashed to the same value with a 512 hash table size. Most hashes had one
  ;;   or two elements inside it.

  (define (op->hash op)
    (cond
      ((eq? op jump/call-op) 0)
      ((eq? op set-op)       1)
      ((eq? op get-op)       2)
      ((eq? op const-op)     3)
      ((eq? op if-op)        4)
      ((number? op)          (+ (abs op) 4))
      ((c-rib? op)           (c-rib-hash op))

      (else (error "Cannot hash the following instruction : " op))))

  (define (hash-combine-over-vector vec)
    (let ((len (vector-length vec)))
      (let loop ((i 0) (acc 0))
        (if (< i (vector-length vec))
          (loop (+ i 1) (hash-combine acc (opnd->hash (vector-ref vec i))))
          acc))))

  (define (opnd->hash opnd)
    (cond
      ((null? opnd)
       4)
      ((eqv? #f opnd)
       5)
      ((eqv? #t opnd)
       6)
      ((symbol? opnd)
       (hash-string (symbol->string opnd)))
      ((number? opnd)
       (abs opnd))
      ((string? opnd)
       (hash-string opnd))
      ((char? opnd)
       (char->integer opnd))
      ((list? opnd)
       (fold hash-combine 0 (map opnd->hash opnd)))
      ((vector? opnd)
       (hash-combine-over-vector opnd))
      ((pair? opnd)
       (hash-combine (opnd->hash (car opnd)) (opnd->hash (cdr opnd))))
      ((c-rib? opnd)
       (c-rib-hash opnd))
      (else (error "Cannot hash the following opnd in a c-rib" opnd))))

  (define (next->hash next)
    (cond
      ((number? next)
       0)
      ((c-rib? next)
       (c-rib-hash next))
      (else
        (error "Cannot hash the next of the following c-rib" next))))

  (modulo (hash-combine
            (hash-combine
              (opnd->hash field1)
              (op->hash field0))
            (next->hash field2))
          table-hash-size))
;; helper function to display the hash table
(define (display-c-rib c-rib)

  (define (display-obj obj)
    (if (rib? obj)
      (string-append
        "%"
        (number->string (c-rib-hash obj))
        "%")
      (object->string obj)))

  (let ((op   (c-rib-oper c-rib))
        (opnd (c-rib-opnd c-rib))
        (next (c-rib-next c-rib)))
    (string-append
      "["
      (display-obj op)
      " "
      (display-obj opnd)
      " "
      (display-obj next)
      "]")))


;;; --------------------------------------------------
;;; ============== RIBBIT's COMPILER =================
;;; --------------------------------------------------


;;; CONTEXT DEFINITIONS
;;; The ctx object contains information while compiling. This include the 
;;; variables that are in scope, the live features, the exports, and the
;;; live variables.
(define (make-ctx cte live exports live-features)
  (rib cte (rib live live-features #f) exports))

(define (ctx-cte ctx) (field0 ctx))
(define (ctx-live ctx) (field0 (field1 ctx)))
(define (ctx-live-features ctx) (field1 (field1 ctx)))


(define (ctx-live-feature? ctx feature)
  (assq-value feature (ctx-live-features ctx)))

(define (ctx-exports ctx) (field2 ctx))

(define (ctx-cte-set ctx x)
  (rib x (field1 ctx) (field2 ctx)))

(define (ctx-live-set! ctx x)
  (field0-set! (field1 ctx) x))

(define (use-symbol ctx sym)
  (ctx-live-set! ctx (add-live sym (ctx-live ctx)))
  sym)

(define (lookup var cte i)
  (if (pair? cte)
      (if (eqv? (car cte) var)
          i
          (lookup var (cdr cte) (+ i 1)))
      var))

(define (extend vars cte)
  (if (pair? vars)
      (cons (car vars) (extend (cdr vars) cte))
      cte))

(define jump/call-op 'jump/call)
(define set-op       'set)
(define get-op       'get)
(define const-op     'const)
(define if-op        'if)

(define tail (c-rib jump/call-op '##id 0)) ;; jump


(define (comp ctx expr cont)
  ;; (cond-expand (ribbit (pp expr)) (else ""))
  (cond ((symbol? expr)
         (let ((v (lookup expr (ctx-cte ctx) 0)))
           (if (eqv? v expr) ;; global?
               (let ((g (live? expr (ctx-live ctx))))
                 (if (and g (constant? g)) ;; constant propagated?
                     (c-rib const-op (cadr (cadr g)) cont)
                     (c-rib get-op v cont)))
               (c-rib get-op v cont))))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (c-rib const-op (cadr expr) cont))

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (let ((val (caddr expr)))
                      (let ((v (lookup var (ctx-cte ctx) 1)))
                        (if (eqv? v var) ;; global?
                            (let ((g (live? var (ctx-live ctx))))
                              (if g
                                  (if (and (constant? g)
                                           (not (assoc var (ctx-exports ctx))))
                                      (begin
;;                                        (pp `(*** constant propagation of ,var = ,(cadr g))
;;                                             (current-error-port))
                                        (gen-noop ctx cont))
                                      (comp ctx val (gen-assign ctx v cont)))
                                  (begin
;;                                    (pp `(*** removed dead assignment to ,var)
;;                                         (current-error-port))
                                    (gen-noop ctx cont))))
                            (comp ctx val (gen-assign ctx v cont)))))))

                 ((eqv? first 'define-primitive)
                  (let* ((name (caadr expr))
                         (prim-index (soft-assoc '@@index (cdr expr))))
                    ;; (pp (list 'define-primitive name (host-config-feature-live? host-config name)))
                    (if (host-config-feature-live? host-config name)
                      (let ((index (if prim-index
                                     (host-config-add-primitive-index! host-config name expr (cadr prim-index))
                                     (host-config-add-primitive! host-config name expr))))
                        (if (or (memq name forced-first-primitives) (eqv? name '##rib))
                          (gen-noop ctx cont)
                          (comp ctx `(set! ,name (##rib ,index 0 ,procedure-type)) cont)))
                      (gen-noop ctx cont))))

                 ((eqv? first 'define-feature)
                  (let* ((feature-expr (cadr expr))
                         (locations (cadddr expr)))
                    (begin
                      (if (eval-feature feature-expr (host-config-features host-config))
                        (for-each
                          (lambda (location)
                            (host-config-add-location!
                              host-config
                              (car location)
                              (cadr location)))
                          locations))
                      (gen-noop ctx cont))))

                 ((eqv? first 'if-feature)
                  (let* ((feature-expr (cadr expr))
                         (then-expr (caddr expr))
                         (else-expr (cadddr expr)))
                    (if (eval-feature feature-expr (host-config-features host-config))
                      (comp ctx then-expr cont)
                      (comp ctx else-expr cont))))

                 ((eqv? first 'use-feature)
                  ;; skip use-feature construct
                  (gen-noop ctx cont))

                 ((eqv? first 'if)
                  (let ((cont-false (comp ctx (cadddr expr) cont)))
                    (let ((cont-true (comp ctx (caddr expr) cont)))
                      (let ((cont-test (c-rib if-op cont-true cont-false)))
                        (comp ctx (cadr expr) cont-test)))))

                 ((eqv? first 'lambda)
                  (let* ((params (cadr expr))
                         (variadic (or (symbol? params) (not (null? (last-item params)))))
                         (nb-params
                           (if variadic
                             (improper-length params)
                             (length params)))
                         (params
                           (if variadic
                             (improper-list->list params '())
                             params)))
                    (c-rib const-op
                           (c-make-procedure
                             (c-rib (+ (* 2 nb-params) (if variadic 1 0))
                                    0
                                    (comp-begin (ctx-cte-set
                                                  ctx
                                                  (extend params
                                                          (cons #f
                                                                (cons #f
                                                                      (ctx-cte ctx)))))
                                                (cddr expr)
                                                tail))
                             '())
                           (if (null? (ctx-cte ctx))
                             cont
                             (add-nb-args
                               #t
                               ctx
                               1
                               (gen-call (use-symbol ctx '##close)
                                         cont))))))

                 ((eqv? first 'begin)
                  (comp-begin ctx (cdr expr) cont))

                 ((eqv? first 'let)
                  (let ((bindings (cadr expr)))
                    (let ((body (cddr expr)))
                      (comp-bind ctx
                                 (map car bindings)
                                 (map cadr bindings)
                                 body
                                 cont))))

                 (else
                   (let ((args (cdr expr)))
                     (if (symbol? first)
                       (begin
                         (let ((call-sym (assoc first call-stats)))
                           (if call-sym
                             (set-car! (cdr call-sym) (+ 1 (cadr call-sym)))
                             (set! call-stats (cons (list first 1) call-stats))))
                         (comp-call ctx
                                    args
                                    (lambda (ctx)
                                      (let ((v (lookup first (ctx-cte ctx) 0)))
                                        (add-nb-args (host-config-is-primitive? host-config first)
                                                     ctx
                                                     (length args)
                                                     (gen-call
                                                       (if (and (number? v) (arity-check? ctx first))
                                                         (+ v 1)
                                                         v)
                                                       cont))))))
                       (comp-bind ctx
                                  (list '_)
                                  (cons first '())
                                  (cons (cons '_ args) '())
                                  cont)))))))

        (else
         ;; self-evaluating
         (c-rib const-op expr cont))))

(define call-stats '())

(define (gen-call v cont)
  (if (eqv? cont tail)
      (c-rib jump/call-op v 0)      ;; jump
      (c-rib jump/call-op v cont))) ;; call

(define (gen-assign ctx v cont)
  (c-rib set-op v (gen-noop ctx cont)))

(define (arity-check? ctx name)
  (and (ctx-live-feature? ctx 'arity-check)
       (not (and
              (ctx-live-feature? ctx 'prim-no-arity)
              (host-config-is-primitive? host-config name)))))

(define (is-call? ctx name cont)
  (and (rib? cont)
       (if (arity-check? ctx name)
           (and (eqv? (c-rib-oper cont) const-op)
                (rib? (c-rib-next cont))
                (eqv? (c-rib-oper (c-rib-next cont)) jump/call-op)
                (eqv? (c-rib-opnd (c-rib-next cont)) name))
           (and (eqv? (c-rib-oper cont) jump/call-op)
                (eqv? (c-rib-opnd cont) name)))))

(define (gen-noop ctx cont)
  (if (is-call? ctx '##arg1 cont)
      (if (arity-check? ctx '##arg1)
        (c-rib-next (c-rib-next cont)) ;; remove const and pop
        (c-rib-next cont)) ;; remove pop
      (c-rib const-op 0 cont))) ;; add dummy value for set!

(define (comp-bind ctx vars exprs body cont)
  (comp-bind* ctx vars exprs ctx body cont))

(define (comp-bind* ctx vars exprs body-ctx body cont)
  (if (pair? vars)
      (let ((var (car vars))
            (expr (car exprs)))
        (comp ctx
              expr
              (comp-bind* (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
                          (cdr vars)
                          (cdr exprs)
                          (ctx-cte-set body-ctx (cons var (ctx-cte body-ctx)))
                          body
                          (gen-unbind ctx cont))))
      (comp-begin body-ctx
                  body
                  cont)))

(define (add-nb-args prim? ctx nb-args tail)
  (if (and 
        (ctx-live-feature? ctx 'arity-check)
        (not (and prim? (ctx-live-feature? ctx 'prim-no-arity ))))
    (c-rib const-op
         nb-args
         tail)
    tail))

(define (gen-unbind ctx cont)
  (if (eqv? cont tail)
    cont
    (add-nb-args
      #t
      ctx
      2
      (c-rib jump/call-op ;; call
             (use-symbol ctx '##arg2)
             cont))))

(define (add-live var live-globals)
  (if (live? var live-globals)
      live-globals
      (let ((g (cons var '())))
        (cons g live-globals))))

(define (live? var lst)
  (if (pair? lst)
    (let ((x (car lst)))
      (if (eqv? var (car x))
        x
        (live? var (cdr lst))))
    #f))


(define (comp-begin ctx exprs cont)
  (comp ctx
        (car exprs)
        (if (pair? (cdr exprs))
          (add-nb-args
            #t
            ctx
            2
            (c-rib jump/call-op ;; call
                   (use-symbol ctx '##arg1)
                   (comp-begin ctx (cdr exprs) cont)))
          cont)))

(define (comp-call ctx exprs k)
  ;(pp (list 'comp-call (ctx-cte ctx) exprs))
  (if (pair? exprs)
      (comp ctx
            (car exprs)
            (comp-call (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
                       (cdr exprs)
                       k))
      (k ctx)))



;;;----------------------------------------------------------------------------



(define (extract-exports program)
  ;; By default all symbols are exported when the program contains
  ;; no (export ...) form.
  (let loop ((lst program) (rev-exprs '()) (exports #f))
    (if (pair? lst)
        (let ((first (car lst)))
          (if (and (pair? first) (eqv? (car first) 'export))
              (loop (cdr lst)
                    rev-exprs
                    (append (cdr first) (or exports '())))
              (loop (cdr lst)
                    (cons first rev-exprs)
                    exports)))
        (cons (reverse rev-exprs) exports))))

(define (exports->alist exports)
  (if (pair? exports)
      (map (lambda (x)
             (if (symbol? x)
                 (cons x x)
                 (cons (car x) (cadr x))))
           exports)
      exports))


(define (host-feature->scheme host-features)
  (fold 
    (lambda (prim acc)
      (let ((recursive-parse (cadr prim))
            (name (caddr prim))
            (use-clause (if (pair? (cdddr prim))
                          (cadddr prim)
                          '(use))))
        (case (car prim)
          ((primitive)
           `((define-primitive 
               ,name
               ,use-clause
               ,recursive-parse)
             . 
             ,acc))
          ((feature)
           (if (pair? (cdr use-clause))
             `((define-feature ,name ,use-clause ()) . ,acc)
             acc))
          (else
            (error "Cannot handle host feature " prim)))))

    '()
    host-features))

(define host-config #f)

(define (compile-program verbosity debug-info parsed-vm features program)
  (let* ((exprs-and-exports
           (extract-exports program))
         (exprs
           (car exprs-and-exports))
         (exprs
           (if (pair? exprs) exprs (list #f)))

         (host-defines-primitives?
           (and parsed-vm (soft-assoc 'primitives parsed-vm)))

         (host-features
           (extract-features
             (if host-defines-primitives?
               parsed-vm
               (if parsed-vm
                 (append default-primitives parsed-vm)
                 default-primitives))))

         (expansion
           `(begin
              ,@(host-feature->scheme host-features) ;; add host features
              ,(expand-begin exprs (make-mtx '() '()))))

         (exports
           (exports->alist (cdr exprs-and-exports)))

         (live-globals-and-features
           (liveness-analysis expansion features exports))

         (live-globals
           (car live-globals-and-features))

         (live-features
           (cdr live-globals-and-features))

         (exports
           (or (and (not (assq 'debug live-features)) exports) ;; export everything when debug is activated
               (map (lambda (v)
                      (let ((var (car v)))
                        (cons var var)))
                    live-globals)))
         (host-config-ctx (make-host-config live-features '() '()))
         (ctx (make-ctx '() live-globals exports live-features))
         (return (make-vector 3)))

    (set! host-config host-config-ctx)

    (set! tail (add-nb-args #t ctx 1 tail))

    (vector-set!
      return
      0
      (c-make-procedure
        (c-rib 0 ;; 0 parameters
               0
               (comp ctx
                     expansion
                     tail))
        '()))
    (vector-set! return 1 exports)
    (vector-set! return 2 host-config)

    ;(pp
    ;  (list-sort
    ;    (lambda (x y) (< (car x) (car y)))
    ;    (map (lambda (pair)
    ;           (cons (car pair) (map display-c-rib (cdr pair)))) (table->list hash-table-c-ribs))))

    (if (or (>= verbosity 3) (memq 'expansion debug-info))
      (begin
        (display "*** Code expansion: \n")
        (pp expansion)))

    (if (or (>= verbosity 3) (memq 'hash-table debug-info))
      (begin
        (display "*** hash-consing table: \n")
        (pp
          (list-sort
            (lambda (x y) (< (car x) (car y)))
            (map (lambda (pair)
                   (list (car pair) (length (cdr pair)))) (table->list hash-table-c-ribs))))))

    (if (or (>= verbosity 2) (memq 'rvm-code debug-info))
      (begin
        (display "*** RVM code:\n")
        (pp (vector-ref return 0))))
    (if (or (>= verbosity 3) (memq 'exports debug-info))
      (begin
        (display "*** exports:\n")
        (pp (vector-ref return 1))))

    (if (or (>= verbosity 2) (memq 'host-config debug-info))
      (begin
        (display "*** HOST CONFIG ***\n")
        (display "*** features :\n")
        (pp (host-config-features (vector-ref return 2)))
        (display "*** primitive order :\n")
        (pp (host-config-primitives (vector-ref return 2)))
        (display "*** feature location :\n")
        (pp (host-config-locations (vector-ref return 2)))))
    ;; (if (>= verbosity 2)
    ;;   (begin
    ;;     (display "*** primitive order:\n")
    ;;     (pp (vector-ref return 2))))
    ;; (if (>= verbosity 3)
    ;;   (begin
    ;;     (display "*** live-features:\n")
    ;;     (pp (vector-ref return 3))))
    return))

;;;----------------------------------------------------------------------------

;; Expansion of derived forms, like "define", "cond", "and", "or".

(define defined-features '()) ;; used as parameters for expand-functions

(cond-expand
  (ribbit
   (define (current-directory) (path-directory (car (cmd-line)))))

  (else
    (begin)))

;; For includes
(define current-resource `(file ,(current-directory) "MAIN"))
(define resource-type car)
(define resource-dir cadr)
(define resource-file caddr)

(define (make-resource type file)
  `(,type ,(path-directory file) ,file))

(define included-resources '())

(define (expand-expr expr mtx)

  (cond ((symbol? expr)
         (let ((expander (mtx-search mtx expr))
               (expand-func (lambda (expr) (expand-begin (list expr) mtx))))
           (if expander
             (expander expr expand-func)
             expr)))

        ((pair? expr)
         (let* ((first (car expr))
                (expander (mtx-search mtx first)))

           (cond
             (expander
               (expander expr (lambda (expr) (expand-begin (list expr) mtx))))

             ((eqv? first 'quote)
              (expand-constant (cadr expr)))

             ((eqv? first 'quasiquote)
              (expand-quasiquote (cadr expr) mtx))

             ((eqv? first 'set!)
              (let* ((var (cadr expr))
                     (expander (mtx-search mtx var)))
                (if expander
                  (expander expr (lambda (expr) (expand-begin (list expr) mtx)))
                  (list 'set! var (expand-expr (caddr expr) mtx)))))

             ((eqv? first 'if)
              (cons 'if
                    (cons (expand-expr (cadr expr) mtx)
                          (cons (expand-expr (caddr expr) mtx)
                                (cons (if (pair? (cdddr expr))
                                        (expand-expr (cadddr expr) mtx)
                                        #f)
                                      '())))))

             ((eqv? first 'lambda)
              (let* ((params (cadr expr))
                     (opt-params '())
                     (required-params '())
                     (variadic (or (symbol? params) (not (eq? (last-item params) '()))))
                     (nb-params (if variadic (improper-length params) (length params))))
                ;; Gather all optional params from the parameter list
                (let loop ((i 0) (params params))
                  (if (< i nb-params)
                    (let ((param (car params)))
                      (cond
                        ((and (symbol? param) (null? opt-params))
                         (set! required-params (append required-params (list param)))
                         (loop (+ 1 i) (cdr params)))

                        ((pair? param)
                         (set! opt-params (append opt-params (list param)))
                         (loop (+ 1 i) (cdr params)))

                        (else (error "Cannot put non-optional arguments after optional ones."))))))
                (if (null? opt-params)
                  (cons 'lambda
                        (cons params
                              (cons (expand-body (cddr expr) (mtx-shadow mtx (if (pair? params) (improper-list->list params '()) (list params)))) '())))
                  ;; Add the check for the optional params
                  (let ((vararg-name (if variadic (last-item params) '##vararg))
                        (opt-params-body '()))
                    (if (pair? required-params)
                      (set-cdr! (list-tail required-params (- (length required-params) 1)) vararg-name)
                      (set! required-params vararg-name))

                    (for-each
                      (lambda (opt-param)
                        (set! opt-params-body
                          (append opt-params-body
                                  (expand-opt-param
                                    (car opt-param)
                                    (cadr opt-param)
                                    vararg-name
                                    mtx))))
                      opt-params)

                    (expand-expr
                      `(lambda
                         ,required-params
                         (let* ,opt-params-body
                           ,@(cddr expr)))
                      mtx)))))

             ((eqv? first 'let)
              (let ((x (cadr expr)))
                (if (symbol? x) ;; named let?
                  (expand-expr
                    (let ((bindings (caddr expr)))
                      (cons
                        (cons
                          'letrec
                          (cons (cons
                                  (cons x
                                        (cons (cons 'lambda
                                                    (cons (map car bindings)
                                                          (cdddr expr)))
                                              '()))
                                  '())
                                (cons x
                                      '())))
                        (map cadr bindings)))
                    mtx)
                  (let ((bindings x))
                    (if (pair? bindings)
                      (cons 'let
                            (cons (map (lambda (binding)
                                         (cons (car binding)
                                               (cons (expand-expr
                                                       (cadr binding)
                                                       mtx)
                                                     '())))
                                       bindings)
                                  (cons (expand-body (cddr expr)
                                                     (mtx-shadow mtx
                                                                 (map car bindings)))
                                        '())))
                      (expand-body (cddr expr)
                                   mtx))))))

             ((eqv? first 'let*)
              (let ((bindings (cadr expr)))
                (expand-expr
                  (cons 'let
                        (if (and (pair? bindings) (pair? (cdr bindings)))
                          (cons (cons (car bindings) '())
                                (cons (cons 'let*
                                            (cons (cdr bindings)
                                                  (cddr expr)))
                                      '()))
                          (cdr expr)))
                  mtx)))

             ((eqv? first 'letrec)
              (let ((bindings (cadr expr)))
                (expand-expr
                  (cons 'let
                        (cons (map (lambda (binding)
                                     (cons (car binding) (cons #f '())))
                                   bindings)
                              (append (map (lambda (binding)
                                             (cons 'set!
                                                   (cons (car binding)
                                                         (cons (cadr binding)
                                                               '()))))
                                           bindings)
                                      (cddr expr))))
                  mtx)))

             ((eqv? first 'begin)
              (expand-begin (cdr expr) mtx))

             ((eqv? first 'define)
              (if (not (pair? (cdr expr)))
                (report-error "Ill defined form of 'define")
                (let ((pattern (cadr expr)))
                  (if (pair? pattern)
                    (cons 'set!
                          (cons (car pattern)
                                (cons (expand-expr
                                        (cons 'lambda
                                              (cons (cdr pattern)
                                                    (cddr expr)))
                                        mtx)
                                      '())))
                    (cons 'set!
                          (cons pattern
                                (cons (expand-expr
                                        (caddr expr)
                                        mtx)
                                      '())))))))

             ((eqv? first 'if-feature)
              (cons 'if-feature
                    (cons (cadr expr)
                          (cons (expand-expr
                                  (caddr expr)
                                  mtx)
                                (cons (if (pair? (cdddr expr))
                                        (expand-expr
                                          (cadddr expr)
                                          mtx)
                                        #f)
                                      '())))))


             ((eqv? (car expr) 'define-primitive) ;; parse arguments as source file
              (let* ((primitive (cadr expr))
                     (use-clause (if (and (pair? (cddr expr))
                                          (pair? (caddr expr))
                                          (eqv? 'use (caaddr expr)))
                                   (caddr expr)
                                   '(use)))
                     (code (filter string? (cdr expr)))
                     (parsed-code
                       (cons "" ;; Add empty head for defined-primitives
                             (parse-host-file
                               (fold
                                 (lambda (x acc)
                                   (append acc (string-split x #\newline)))
                                 '()
                                 code)))))
                `(define-primitive
                   ,primitive
                   ,use-clause
                   ,parsed-code)))

             ((eqv? (car expr) 'define-feature) ;; parse arguments as a source file
              (let* ((feature-condition (cadr expr))
                     (use-clause (and (pair? (cddr expr))
                                      (pair? (caddr expr))
                                      (eqv? 'use (caaddr expr))
                                      (caddr expr)))
                     (bindings 
                       (if use-clause 
                         (if (null? (cdddr expr))
                           '()
                           (cadddr expr))
                         (caddr expr)))

                     (bindings 
                       (map
                         (lambda (x)
                           `(,(car x) ,(parse-host-file (string-concatenate (cdr x) "\n"))))
                         bindings)))
                `(define-feature
                   ,feature-condition
                   ,(or use-clause '(use))
                   ,bindings)
                ))

             ((eqv? first 'and)
              (expand-expr
                (if (pair? (cdr expr))
                  (if (pair? (cddr expr))
                    (cons 'if
                          (cons (cadr expr)
                                (cons (cons 'and
                                            (cddr expr))
                                      (cons #f
                                            '()))))
                    (cadr expr))
                  #t)
                mtx))

             ((eqv? first 'or)
              (expand-expr
                (if (pair? (cdr expr))
                  (if (pair? (cddr expr))
                    (cons
                      'let
                      (cons
                        (cons (cons '_
                                    (cons (cadr expr)
                                          '()))
                              '())
                        (cons
                          (cons 'if
                                (cons '_
                                      (cons '_
                                            (cons (cons 'or
                                                        (cddr expr))
                                                  '()))))
                          '())))
                    (cadr expr))
                  #f)
                mtx))

             ((eqv? first 'cond)
              (expand-expr
                (if (pair? (cdr expr))
                  (let* ((clause (cadr expr))
                         (condition (car clause))
                         (body (cdr clause)))
                    (if (eqv? 'else condition)
                      (cons 'begin body)
                      (cond
                        ((null? body) `(or ,condition (cond . ,(cddr expr))))
                        ((eq? '=> (car body))
                         `(let ((_ ,condition))
                            (if _
                              (,(cadr body) _)
                              (cond . ,(cddr expr)))))
                        (else
                          `(if ,condition
                             (begin . ,body)
                             (cond . ,(cddr expr)))))))
                  #f)
                mtx))

             ((eqv? first 'case) ;; FIXME: transform into a (let (...) (cond ...)) that evaluate key one time
              (let ((key (cadr expr)))
                (let ((clauses (cddr expr)))
                  (if (pair? clauses)
                    (expand-expr
                      `(let ((##case-tmp ,key))
                         (cond
                           ,@(map
                               (lambda (clause)
                                 (if (eqv? (car clause) 'else)
                                   clause
                                   `((memv ##case-tmp ',(car clause))
                                     ;(or ,@(map (lambda (val) `(eqv? ##case-tmp ',val)) (car clause)))
                                     ,@(cdr clause))))
                               clauses)))
                      mtx)
                    #f))))

             ((eqv? first '##include-str)
              (expand-expr (read-str-resource (parse-resource (cadr expr))) mtx))

             (else
               (expand-list expr mtx)))))

        (else
          (expand-constant expr))))

(define (expand-constant x)
  (if (or (number? x) (string? x) (char? x))
    x
    (list 'quote x)))

(define (expand-quasiquote rest mtx)
  (let parse ((x rest) (depth 1))
    (cond
      ((not (pair? x))
       (if (vector? x)
         (list '##qq-list->vector (parse (vector->list x) depth))
         (expand-constant x)))
      ((eqv? (car x) 'unquote)
       (if (= depth 1)
         (if (pair? (cdr x))
           (expand-expr (cadr x) mtx)
           (error "unquote: bad syntax"))
         (list '##qq-cons (expand-constant 'unquote) (parse (cdr x) (- depth 1)))))
      ((and (pair? (car x)) (eqv? (caar x) 'unquote-splicing))
       (if (= depth 1)
         (list '##qq-append (expand-expr (cadar x) mtx) (parse (cdr x) depth))
         (list '##qq-cons (list '##qq-cons (expand-constant 'unquote-splicing) (parse (cdar x) (- depth 1))) (parse (cdr x) depth))))
      ((eqv? (car x) 'quasiquote)
       (list '##qq-cons (expand-constant 'quasiquote) (parse (cdr x) (+ depth 1))))
      (else
        (list '##qq-cons (parse (car x) depth) (parse (cdr x) depth))))))


(define (expand-body exprs mtx)
  (let loop ((exprs exprs) (defs '()) (mtx mtx))
    (if (pair? exprs)
        (let ((expr (car exprs)))
          (cond
            ((and (pair? expr) (eqv? 'define (car expr)) (pair? (cdr expr)))
             (let ((pattern (cadr expr)))
               (if (pair? pattern)
                 (loop (cdr exprs)
                       (cons (cons (car pattern)
                                   (cons (cons 'lambda
                                               (cons (cdr pattern)
                                                     (cddr expr)))
                                         '()))
                             defs)
                       mtx)
                 (loop (cdr exprs)
                       (cons (cons pattern
                                   (cddr expr))
                             defs)
                       mtx))))
            ((and (pair? expr) (eqv? 'define-expander (car expr)) (pair? (cdr expr)))
             (let ((pattern (cadr expr)))
               (if (pair? pattern)
                 (loop (cdr exprs)
                       defs
                       (mtx-add-cte
                         mtx
                         (caadr expr)
                         (eval `(lambda (,@(cdadr expr))
                                  ,@(cddr expr)))))

                 (loop (cdr exprs)
                       defs
                       (let ((expander-name (cadr expr))
                             (expander-body (caddr expr)))
                         (if (not (eq? (car expander-body) 'lambda))
                           (error "*** define-expander: expected lambda expression" expander-body))

                         (mtx-add-cte
                           mtx
                           expander-name
                           (eval expander-body)))))))
            ((and (pair? expr) (mtx-search mtx (car expr)))
             (let ((expander (mtx-search mtx (car expr))))
               (loop (cons
                       (expander expr (lambda (expr) (expand-expr expr mtx)))
                       (cdr exprs))
                     defs
                     mtx)))

            (else
              (expand-body-done defs exprs mtx))))
        (expand-body-done defs (list 0) mtx))))

(define (expand-body-done defs exprs mtx)
  (if (pair? defs)
      (expand-expr
       (cons 'letrec
             (cons (reverse defs)
                   exprs))
       mtx)
      (expand-begin exprs mtx)))

(define (expand-begin exprs mtx)
  (let ((x (expand-begin* exprs '() mtx)))
    (if (pair? x)
        (if (pair? (cdr x))
            (cons 'begin x)
            (car x))
        (expand-constant 0)))) ;; unspecified value

(define (report-error err-type . args)
  (error (string-append "Error: "
                        err-type
                        " in "
                        (resource-file current-resource)
                        ". "
                        (string-concatenate (map object->string args) " "))))



(define (make-mtx global-macro cte)  ;; macro-contex object
  (rib global-macro cte 0))

(define mtx-global      field0)
(define mtx-global-set! field0-set!)

(define (add-included-resource! resource)
  (set! included-resources (cons resource included-resources)))

(define mtx-cte      field1)
(define mtx-cte-set! field1-set!)

(define (mtx-cte-set mtx cte)
  (make-mtx (mtx-global mtx) cte))

(define (included? resource)
  (member resource included-resources))

(define (mtx-add-global! mtx macro-name macro-value)
  (mtx-global-set! mtx (cons (list macro-name macro-value) (mtx-global mtx))))

(define (mtx-add-cte mtx macro-name macro-value)
  (mtx-cte-set mtx (cons (list macro-name macro-value) (mtx-cte mtx))))

(define (mtx-search mtx macro-name)
  (let ((macro-value (assq macro-name (append (mtx-cte mtx) (mtx-global mtx)))))
    (and macro-value (cadr macro-value))))


;; Shadow macro by a variable
(define (mtx-shadow mtx variable-names)
  (mtx-cte-set
    mtx
    (append
      (map (lambda (variable-name)
             (list variable-name #f))
           variable-names)
      (mtx-cte mtx))))


(define (expand-begin* exprs rest mtx)
  (if (pair? exprs)
      (let* ((expr (car exprs))
             (r '())
             (expanded-expr
               (cond ((and (pair? expr) (eqv? (car expr) 'begin))
                      (expand-begin* (cdr expr) r mtx))

                     ((and (pair? expr) (eqv? (car expr) 'cond-expand))
                      (expand-cond-expand-clauses (cdr expr) r mtx))

                     ((and (pair? expr)
                           (eqv? (car expr) '##include))
                      (cons (expand-resource (parse-resource (cadr expr)) mtx) r))

                     ((and (pair? expr)
                           (eqv? (car expr) '##include-once))

                      (let ((resource (parse-resource (cadr expr))))
                        (if (included? resource)
                          r
                          (begin
                            (add-included-resource! resource)
                            (cons (expand-resource resource mtx) r)))))

                     ((and (pair? expr)
                           (eqv? (car expr) 'define-expander))
                      (if (pair? (cadr expr))
                        (mtx-add-global!
                          mtx
                          (caadr expr)
                          (eval `(lambda (,@(cdadr expr))
                                   ,@(cddr expr))))
                        (let ((expander-name (cadr expr))
                              (expander-body (caddr expr)))
                          (if (not (eq? (car expander-body) 'lambda))
                            (error "*** define-expander: expected lambda expression" expander-body)
                            (mtx-add-global!
                              mtx
                              expander-name
                              (eval expander-body)))))
                      r)

                     (else
                       (cons (expand-expr expr mtx) r)))))

        (append expanded-expr (expand-begin* (cdr exprs) rest mtx)))
      rest))

(define (cond-expand-eval expr)
  (cond ((and (pair? expr) (eqv? (car expr) 'not))
         (not (cond-expand-eval (cadr expr))))
        ((and (pair? expr) (eqv? (car expr) 'and))
         (not (memv #f (map cond-expand-eval (cdr expr)))))
        ((and (pair? expr) (eqv? (car expr) 'or))
         (not (not (memv #t (map cond-expand-eval (cdr expr))))))
        ((and (pair? expr) (eqv? (car expr) 'host))
         (eqv? (cadr expr) (string->symbol target)))
        (else
         (eqv? expr 'ribbit))))

(define (expand-cond-expand-clauses clauses rest mtx)
  (if (pair? clauses)
      (let ((clause (car clauses)))
        (if (or (eqv? 'else (car clause))
                (cond-expand-eval (car clause)))
            (expand-begin* (cdr clause) rest mtx)
            (expand-cond-expand-clauses (cdr clauses) rest mtx)))
      rest))

(define (expand-list exprs mtx)
  (if (pair? exprs)
      (cons (expand-expr (car exprs) mtx)
            (expand-list (cdr exprs) mtx))
      '()))


(define (expand-opt-param param-name param-default vararg-name mtx)
  ;; If this part is not performant enough, replace the set! with a
  ;; (vararg (if (eqv? vararg '()) '() (field1 vararg)))
  ;; after every optional arg clause
  `((,param-name (if (##eqv? ,vararg-name '())
                    ,(expand-expr param-default mtx)
                    (let ((value (##field0 ,vararg-name)))
                      (set! ,vararg-name (##field1 ,vararg-name))
                      value)))))
  ;; (list
  ;;   (list param-name
  ;;         (list 'if (list '##eqv? vararg-name '())
  ;;               (expand-expr param-default)
  ;;               (list 'let (list (list 'value (list '##field0 vararg-name)))
  ;;                     (list 'set! vararg-name (list '##field1 vararg-name))
  ;;                     'value
  ;;                     )
  ;;               )
  ;;         )))


;;;----------------------------------------------------------------------------

;; Resource reading and parsing.

(define resource-str-reader-table
  `((ribbit
      ,(lambda (resource-path)
         (let* ((resource-paths 
                  (fold 
                    (lambda (lib-path acc) 
                      (let ((path (path-normalize (path-expand resource-path lib-path))))
                        (append 
                          (list (string-append path ".scm") path)
                          acc)))
                    '()
                    ribbit-path))
                (valid-resource-path
                  (filter file-exists? resource-paths)))
          (if (null? valid-resource-path)
             (error "Error while trying to include library, file doesn't exist." resource-paths)
             (open-input-file (car valid-resource-path))))))

    (file ,(lambda (resource-path)
             (if (not (file-exists? resource-path))
               (error "Error while trying to include library, file doesn't exist." resource-path)
               (open-input-file resource-path))))))

(define (add-resource-str-reader! name reader)
  (if (or (not (symbol? name)) (memq #\: (string->list (symbol->string name))))
    (error "The resource reader name must be a symbol that doesn't contain #\\:"))
  (if (assq name resource-str-reader-table)
    (error "The resource reader already exists:" name))
  (set! resource-str-reader-table (append resource-str-reader-table (list (list name reader)))))

(define (parse-resource include-path)
  (let ((resource-type 
          (if (pair? include-path) 
            (car include-path) 
            (car current-resource)))
        (path 
          (if (pair? include-path) 
            (path-normalize (cadr include-path))
            (path-normalize 
              (path-expand 
                (path-normalize include-path) 
                (resource-dir current-resource))))))
    (make-resource resource-type path)))

(define (get-resource-port resource)
  (let ((resource-path (resource-file resource))
        (reader (assq (resource-type resource) resource-str-reader-table)))
    (if reader
      ((cadr reader) resource-path)
      (error "No resource reader found for resource-type:" (resource-type resource)))))

(define (expand-resource resource mtx)
  (let ((old-current-resource current-resource))
    (set! current-resource resource)
    (let ((result (expand-begin (read-all (get-resource-port resource)) mtx)))
      (set! current-resource old-current-resource)
      result)))


;;; =========================================
;;; ---------- LIVENESS ANALYSIS ------------
;;; =========================================

(define (liveness-analysis expr features exports)
  (let* ((live-env (liveness-analysis-aux expr features '()))
         (live-env-result
           (if (live-env-live? live-env 'symtbl)
             (liveness-analysis-aux expr features exports)
             live-env)))
    (cons (live-env-globals live-env-result)
          (live-env-features live-env-result))))


;; Environnement for liveness analysis.

(define (make-live-env live-globals features forced-features)
  (rib live-globals (rib features forced-features 0) #f)) ;; last one is a dirty bit


(define (live-env-globals live-env)
  (field0 live-env))

(define (live-env-features live-env)
  (field0 (field1 live-env)))

(define (live-env-forced-features live-env)
  (field1 (field1 live-env)))

(define (live-env-dirty? live-env)
  (field2 live-env))

(define (live-env-clean? live-env)
  (not (live-env-dirty? live-env)))

(define (live-env-set-dirty! live-env)
  (field2-set! live-env #t))

(define (live-env-set-clean! live-env)
  (field2-set! live-env #f))

(define (live-env-set-globals! live-env live-globals)
  (live-env-set-dirty! live-env)
  (field0-set! live-env live-globals))

(define (live-env-set-features! live-env features)
  (live-env-set-dirty! live-env)
  (field0-set! (field1 live-env) features))

;; Other usefull procedures

(define (live-env-reset-defs live-env)

  (define (reset-defs lst)

    (let loop ((lst lst))
      (if (pair? lst)
        (begin
          (set-cdr! (car lst) '())
          (loop (cdr lst)))
        #f)))
  (reset-defs (live-env-globals live-env)))

(define (live-env-forced-feature? live-env feature)
  (assq feature (live-env-forced-features live-env)))

(define (live-env-add-live! live-env var)

  (if (live-env-live? live-env var)
    live-env
    (begin
      (live-env-set-globals!
        live-env
        (let ((g (cons var '())))
          (cons g (live-env-globals live-env)))))))

(define (live-env-live? live-env var)
  (assq var (live-env-globals live-env)))

(define (live-env-live-feature? live-env feature)
  (let ((feature-pair (assq feature (live-env-features live-env))))
    (and feature-pair (cdr feature-pair))))

(define (live-env-add-feature! live-env var)
  (live-env-add-feature-value! live-env var #t))

(define (live-env-add-feature-value! live-env var value)
  (if (live-env-live-feature? live-env var)
    var
    (and
      (not (live-env-forced-feature? live-env var)) ;; check not forced
      (live-env-set-features! live-env (cons (cons var value) (live-env-features live-env))))))


(define (constant? g)
  (and (pair? g)
       (pair? (cdr g))
       (null? (cddr g))
       (pair? (cadr g))
       (eqv? 'quote (car (cadr g)))))

(define (in? var cte)
  (not (eqv? var (lookup var cte 0))))


(define (liveness-analysis-aux expr forced-features exports)
  (let* ((env (make-live-env '() forced-features forced-features)))

    ;; ##rib is always needed
    (live-env-add-live! env '##rib)

    ;; It's hard to detect in the liveness analysis if we need ##id or not.
    ;; We will add it by default.
    (live-env-add-live! env '##id)

    (and exports
         (for-each
           (lambda (x) (live-env-add-live! env (car x)))
           exports))

    (let loop ()
      (live-env-reset-defs env)
      (live-env-set-clean! env)
      (liveness expr env (not exports))
      (if (live-env-clean? env)
        env
        (loop)))))



(define (liveness expr env export-all?)
  (define export-all (live-env-live-feature? env 'export-all))

  (define (add-val val)
    (cond ((symbol? val)
           (live-env-add-live! env val))
          ((pair? val)
           (add-val (car val))
           (add-val (cdr val)))
          ((vector? val)
           (for-each add-val (vector->list val)))))

  (define (liveness expr cte top?)

    (cond ((symbol? expr)
           (if (in? expr cte) ;; local var?
             #f
             (live-env-add-live! env expr))) ;; mark the global variable as "live"

          ((integer? expr)
           (if (< expr 0)
             ;; If the integer is negative, we need to
             ;;   use ##- to create a binding
             (live-env-add-live! env '##-))
           #f)

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eqv? first 'quote)
                    (let ((val (cadr expr)))
                      (add-val val)))

                   ((eqv? first 'set!)
                    (let ((var (cadr expr)))
                      (let ((val (caddr expr)))
                        (if (in? var cte) ;; local var?
                          (liveness val cte #f)
                          (begin
                            (let ((g (or export-all (live-env-live? env var)))) ;; variable live?
                              (if g
                                (begin
                                  (set-cdr! g (cons val (cdr g)))
                                  (liveness val cte #f))
                                #f)))))))

                   ((eqv? first 'if)
                    (liveness (cadr expr) cte #f)
                    (liveness (caddr expr) cte #f)
                    (liveness (cadddr expr) cte #f))

                   ((eqv? first 'let)
                    ;; Let uses ##arg2 for creating bindings
                    (live-env-add-live! env '##arg2)
                    (let ((bindings (cadr expr)))
                      (liveness-list (map cadr bindings) cte)
                      (liveness (caddr expr) (append (map car bindings) cte) #f)))

                   ((eqv? first 'begin)
                    ;; Begin uses ##arg1 to get rid of the value on the stack when
                    ;;  chaining
                    (live-env-add-live! env '##arg1)
                    (liveness-list (cdr expr) cte))

                   ((eqv? first 'lambda)
                    ;; ##close is used by lambda constructs
                    (live-env-add-live! env '##close)
                    (let ((params (cadr expr)))
                      (if (improper-list? params)
                        (begin
                          (live-env-add-feature! env 'rest-param) ;; detect rest-params
                          (live-env-add-feature! env 'arity-check)))

                      (if (symbol? params) ;; this is the case in lambdas with rest params
                        (begin
                          (live-env-add-feature! env 'rest-param) ;; detect rest-params
                          (live-env-add-feature! env 'arity-check)
                          (liveness (caddr expr) (cons params cte) #f))
                        (liveness (caddr expr) (extend params cte) #f))))

                   ((eqv? first 'define-feature)
                    (let ((feature-expr (cadr expr))
                          (use  (caddr expr)))
                      (if (not (eq? (car use) 'use))
                        (error "Ill-formed define-feature construct. Expected 'use' keyword." use))

                      (if (eval-feature feature-expr (live-env-features env))
                        (for-each (lambda (x) (live-env-add-feature! env x)) (cdr use))
                        #f)))

                   ((eqv? first 'define-primitive)
                    (let ((name (caadr expr))
                          (use  (caddr expr)))

                      (if (not (eq? (car use) 'use))
                        (error "Ill-formed define-primitive construct. Expected 'use' keyword." use))

                      (if (or (live-env-live-feature? env name) (live-env-live? env name))
                        (begin
                          (live-env-add-feature! env name)
                          (for-each (lambda (x) (live-env-add-feature! env x)) (cdr use)))
                        #f)))

                   ((eqv? first 'if-feature)
                    (let ((feature-expr (cadr expr)))
                      (if (eval-feature feature-expr (live-env-features env))
                        (liveness (caddr expr) cte #f)
                        (liveness (cadddr expr) cte #f))))

                   ((eqv? first 'use-feature)
                    (for-each 
                      (lambda (x)
                        (cond
                          ;; Just a symbol = (feature . #t)
                          ((symbol? x)
                           (live-env-add-feature! env x))

                          ;; '(feature value)
                          ((and 
                             (pair? x)
                             (symbol? (car x))
                             (pair? (cdr x))
                             (symbol? (cadr x)))
                           (live-env-add-feature-value! env (car x) (cadr x)))

                          ;; '(feature . value) unchanged
                          ((and
                             (pair? x)
                             (symbol? (car x))
                             (symbol? (cdr x)))
                           (live-env-add-feature-value! env (car x) (cdr x)))
                          (else
                            (error 
                              "Ill-formed use-feature construct. Expected feature or feature-value pair." 
                              x))))
                      (cdr expr)))

                   (else
                     ;; Calls in first position create a binding, thus needing ##arg2
                     (if (pair? first)
                       (live-env-add-live! env '##arg2))
                     (liveness-list expr cte)))))

          (else
           #f)))

  (define (liveness-list exprs cte)
    (if (pair? exprs)
        (begin
          (liveness (car exprs) cte #f)
          (liveness-list (cdr exprs) cte))
        #f))

  (liveness expr '() #t))

;;;----------------------------------------------------------------------------

;; RVM code encoding.

;; New encoding, for each instruction. Each instruction is taken separately 'int' et 'sym' and 'const' :
;;    - Number of short encodings
;;    - Number of long encodings
;; New instruction 'skip'.
;;
;; + lonely instruction (if)

;; Old encoding :
;;    - Assumes 3 long encoding for each pair of intruction. Each pair of instruction has a priority
;;         either 'int' or 'sym'. Depending on the priority, the short encoding will be for the 'int'
;;         or 'sym' variant. The 'const' and 'if' is special.
;;    - 'if' has only one code. It pops the two values on top of the stack and creates the if. Its a merge.
;;    - 'const' also has the 'proc' variante. This variante allows to push a rib directly.


(define (calculate-start encoding-table)
  (define counter 0)

  (map
    (lambda (lst)
      (let* ((sym (car lst))
             (size (cadr lst))
             (return-val (list sym size counter)))
        (set! counter (+ counter size))
        return-val))
    encoding-table))


(define encoding-original-92
  (calculate-start
    '(
      ;; jump
      ((jump sym short) 20)
      ((jump int long)  1)
      ((jump sym long)  2)

      ((jump int short) 0)

      ;; call
      ((call sym short) 30)
      ((call int long)  1)
      ((call sym long)  2)

      ((call int short) 0)

      ;; set
      ((set int long)   1)
      ((set sym long)   2)

      ((set sym short)  0)
      ((set int short)  0)

      ;; get
      ((get int short)  10)
      ((get int long)   1)
      ((get sym long)   2)

      ((get sym short)  0)

      ;; const
      ((const int short)  11)
      ((const int long)   1)
      ((const sym long)   2)

      ((const proc short) 4)
      ((const proc long)  1)

      ((const sym short) 0)

      (if 1))))



(define encoding-skip-92
  (calculate-start
    '(
      ;; jump
      ((jump sym short) 20)
      ((jump int long)  1)
      ((jump sym long)  2)

      ((jump int short) 0)

      ;; call
      ((call sym short) 20)
      ((call int long)  1)
      ((call sym long)  2)

      ((call int short) 0)

      ;; set
      ((set int long)   1)
      ((set sym long)   2)

      ((set sym short)  0)
      ((set int short)  0)

      ;; get
      ((get int short)  10)
      ((get int long)   1)
      ((get sym long)   2)

      ((get sym short)  0)

      ;; const
      ((const int short)  11)
      ((const int long)   1)
      ((const sym long)   2)

      ((const proc short) 4)
      ((const proc long)  1)

      ((const sym short) 0)

      ((skip int short) 9)
      ((skip int long)  1)

      (if 1)

      )))


(define (encoding-inst-size encoding entry)
  (cadr (encoding-inst-get encoding entry)))

(define (encoding-inst-start encoding entry)
  (caddr (encoding-inst-get encoding entry)))

(define (encoding-inst-get encoding entry)
  (assoc entry encoding))

(define (encoding-size encoding)
  (fold + 0 (map cadr encoding)))


(define predefined (list '##rib 'false 'true 'nil)) ;; predefined symbols

(define (encode-constants proc host-config)

  (define built-constants '())

  (define (add-nb-args prim? nb-args tail)
    (if (and (host-config-features host-config)
             (host-config-feature-live? host-config 'arity-check)
             (not (and prim? (host-config-feature-live? host-config 'prim-no-arity))))
      (c-rib const-op
             nb-args
             tail)
      tail))

  (define (build-constant o tail)
    (cond ((or (memv o '(#f #t ()))
               (assq o built-constants))
           (let ((v (constant-global-var o)))
             (c-rib get-op
                    v
                    tail)))
          ((symbol? o)
           (c-rib const-op
                  o
                  tail))
          ((number? o)
           (if (< o 0)
             (begin

               (if (not (host-config-feature-live? host-config '##-))
                 (host-config-feature-add! host-config '##- #t))

               (c-rib const-op
                      0
                      (c-rib const-op
                             (- o)
                             (add-nb-args
                               #t
                               2
                               (c-rib jump/call-op
                                      '##-
                                      tail)))))
               (c-rib const-op
                      o
                      tail)))
          ((char? o)
           (if (and (host-config-features host-config)
                    (memq 'no-chars (host-config-features host-config)))
             (c-rib const-op
                    (char->integer o)
                    tail)
             (c-rib const-op
                    (char->integer o)
                    (c-rib const-op
                           0
                           (c-rib const-op
                                  char-type
                                  (add-nb-args
                                    #t
                                    3
                                    (c-rib jump/call-op
                                           '##rib
                                           tail)))))))
          ((pair? o)
           (build-constant (car o)
                           (build-constant (cdr o)
                                           (c-rib const-op
                                                  pair-type
                                                  (add-nb-args
                                                    #t
                                                    3
                                                    (c-rib jump/call-op
                                                           '##rib
                                                           tail))))))
          ((string? o)
           (let ((chars (string->list* o)))
             (build-constant chars
                             (build-constant (length chars)
                                             (c-rib const-op
                                                    string-type
                                                    (add-nb-args
                                                      #t
                                                      3
                                                      (c-rib jump/call-op
                                                             '##rib
                                                             tail)))))))
          ((vector? o)
           (let ((elems (vector->list o)))
             (build-constant elems
                             (build-constant (length elems)
                                             (c-rib const-op
                                                    vector-type
                                                    (add-nb-args
                                                      #t
                                                      3
                                                      (c-rib jump/call-op
                                                             '##rib
                                                             tail)))))))

          (else
           (error "can't build constant" o))))

  (define (build-constant-in-global-var o v)
    (let ((code (build-constant o 0)))
      (set! built-constants (cons (cons o (cons v code)) built-constants))
      v))

  (define (add-init-primitives tail)

    (define (prim-code sym tail)
      (let ((prim (assq sym (host-config-primitives host-config))))
        (if (not prim)
          (error "Error, primitive needed by the compiler was not found in host :" sym))
        (let ((index (cadr prim)))
          (c-rib const-op
                 index
                 (c-rib const-op
                        0
                        (c-rib const-op
                               procedure-type
                               (add-nb-args
                                 #t
                                 3
                                 (c-rib jump/call-op
                                        '##rib
                                        (c-rib set-op
                                               sym
                                               tail)))))))))

    ;; skip rib primitive that is predefined
    (let loop ((lst (filter
                      (lambda (x) (host-config-feature-live? host-config x))
                      forced-first-primitives))
               (tail tail))
      (if (pair? lst)
          (loop (cdr lst)
                (let* ((sym (car lst)))
                  (prim-code sym tail)))
          tail)))

  (define (append-code code tail)
    (if (eqv? code 0)
        tail
        (c-rib (c-rib-oper code) (c-rib-opnd code) (append-code (c-rib-next code) tail))))

  (define (add-init-constants tail)
    (let loop ((lst built-constants) (tail tail))
      (if (pair? lst)
          (let* ((x (car lst))
                 (o (car x))
                 (v (cadr x))
                 (code (cddr x)))
            (loop (cdr lst)
                  (append-code code (c-rib set-op v tail))))
          tail)))

  (define (add-init-code proc)
    (let* ((code (c-rib-oper proc))
           (new-code (add-init-primitives (add-init-constants (c-rib-next code)))))
      (c-rib (c-rib
               (c-rib-oper code)
               (c-rib-opnd code)
               new-code)
             (c-rib-opnd proc)
             (c-rib-next proc))))


  (define constant-counter 0)

  (define (constant-global-var o)
    (cond ((eqv? o #f)
           'false)
          ((eqv? o #t)
           'true)
          ((eqv? o '())
           'nil)
          (else
           (let ((x (assq o built-constants)))
             (if x
                 (cadr x)
                 (let ((v (string->symbol
                           (string-append "_"
                                          (number->string constant-counter)))))
                   (set! constant-counter (+ constant-counter 1))
                   (build-constant-in-global-var o v)
                   v))))))

  (traverse-code
    (c-rib-oper proc)
    (lambda (code traverse)
      (let ((op (c-rib-oper code))
            (o  (c-rib-opnd code)))
        (cond ((eqv? op if-op)
               (traverse o))
              ((eqv? op const-op)
               (if (c-procedure? o)
                 (traverse (c-rib-next (c-rib-oper o))))
               (if (not (or (symbol? o)
                            (c-procedure? o)
                            (and (number? o) (>= o 0))))
                   (let ((constant (constant-global-var o)))
                     (c-rib-oper-set! code get-op)
                     (c-rib-opnd-set! code constant))))))))

  (add-init-code proc))

(define (traverse-code code func)
  (let ((traverse (lambda (code) (traverse-code code func))))
    (if (rib? code)
      (begin
        (func code traverse)
        (traverse-code (c-rib-next code) func)))))

(define (encode-symtbl proc exports host-config call-sym-short-size)
  (define syms (make-table))

  (define (scan-proc proc)
    (scan (c-rib-next (c-procedure-code proc))))

  (define (scan-opnd o pos)
    (scan-opnd-aux o pos)
    o)

  (define (scan-opnd-aux o pos)
    (cond ((symbol? o)
           (let ((descr
                  (or (table-ref syms o #f)
                      (let ((descr (rib 0 0 0)))
                        (table-set! syms o descr)
                        descr))))
             (cond ((= pos 0)
                    (field0-set! descr (+ 1 (field0 descr))))
                   ((= pos 1)
                    (field1-set! descr (+ 1 (field1 descr))))
                   ((= pos 2)
                    (field2-set! descr (+ 1 (field2 descr)))))))
          ((c-procedure? o)
           (scan-proc o))))

  (define (scan code)
    (if (rib? code)
        (begin
          (scan-instr code)
          (scan (c-rib-next code)))))

  (define (scan-instr code)
    (let ((op (c-rib-oper code))
          (o  (c-rib-opnd code)))
      (cond ((eqv? op if-op)
             (scan o))
            ((eqv? op jump/call-op)
             (scan-opnd o 0)) ;; 0 = jump/call
            ((eqv? op get-op)
             (scan-opnd o 1)) ;; 1 = get
            ((eqv? op const-op)
             (if (or
                   (symbol? o)
                   (c-procedure? o)
                   (and (number? o) (>= o 0)))
                 (scan-opnd o 2) ;; 2 = const
                 (error "Cannot encode constant with opnd " o)))
            ((eqv? op set-op)
             (scan-opnd o 3))))) ;; 3 = set

  (define (ordering sym-descr)
    (let ((sym (car sym-descr)))
      (let ((pos (member sym predefined)))
        (if pos
            (+ 9999999 (length pos))
            (let ((descr (cdr sym-descr)))
              (field0 descr))))))

  (for-each (lambda (sym) (scan-opnd sym 3)) predefined)

  (scan-proc proc)

    (let ((lst
            (list-sort
              (lambda (a b)
                (< (ordering b) (ordering a)))
              (table->list syms))))

      (let loop1 ((i 0) (lst lst) (symbols '()))
        (if (and (pair? lst) (< i call-sym-short-size))
          (let ((s (car lst)))
            (let ((sym (car s)))
              (let ((descr (cdr s)))
                (let ((x (assq sym exports)))
                  (let ((symbol (if x (cdr x) (str->uninterned-symbol ""))))
                    (field0-set! descr i)
                    (loop1 (+ i 1) (cdr lst) (cons symbol symbols)))))))
          (let loop2 ((i i) (lst2 lst) (symbols symbols))
            (if (pair? lst2)
              (let ((s (car lst2)))
                (let ((sym (car s)))
                  (let ((x (assq sym exports)))
                    (if x
                      (let ((symbol (cdr x)))
                        (let ((descr (cdr s)))
                          (field0-set! descr i)
                          (loop2 (+ i 1) (cdr lst2) (cons symbol symbols))))
                      (loop2 i (cdr lst2) symbols)))))
              (let loop3 ((i i) (lst3 lst) (symbols symbols))
                (if (pair? lst3)
                  (let ((s (car lst3)))
                    (let ((sym (car s)))
                      (let ((x (assq sym exports)))
                        (if x
                          (loop3 i (cdr lst3) symbols)
                          (let ((symbol (str->uninterned-symbol "")))
                            (let ((descr (cdr s)))
                              (field0-set! descr i)
                              (loop3 (+ i 1) (cdr lst3) (cons symbol symbols))))))))
                  (let loop4 ((symbols* symbols))
                    (if (and (pair? symbols*)
                             (string=? (symbol->str (car symbols*)) ""))
                      (loop4 (cdr symbols*))
                      (cons syms symbols*)))))))))))

(define (get-maximal-encoding encodings stats encoding-size)

    (define encoding-size-counter encoding-size)

    (define (get-running-sum lst)
      (reverse
        (fold
          (lambda (x acc)
            (cons (+ x (car acc)) acc))
          (list 0)
          lst)))

    (define (normalize lst)
      (map
        (lambda (x y)
          (if (eqv? y 0)
            0
            (/ x y)))
        lst
        (iota (length lst))))

    (define (calculate-gain-short value-table instruction max offset current-encoding-table encoding-size)
      (let loop ((index offset) (lst '()))
        (if (< index (- max offset))
          (let* ((byte-count-optimal 1)
                 (byte-count-current (get-byte-count instruction index current-encoding-table encoding-size))
                 (gain               (* (- byte-count-current byte-count-optimal) (table-ref value-table index 0)))
                 (new-index          (+ index 1)))
            (loop
              new-index
              (cons gain lst)))
          (reverse lst))))

    (define (calculate-gain-long value-table instruction max offset current-encoding-table encoding-size)
      (let ((current-table-value (sum-byte-count value-table (reverse instruction) current-encoding-table encoding-size)))
        (let loop ((index (+ offset 1))
                   (old-gain current-table-value)
                   (lst '()))
          (if (< index (- max offset))
            (let* ((optimal-table
                     (let ((optimal (table-copy current-encoding-table)))
                       (table-ref optimal (append instruction (list 'long)))
                       (table-set! optimal (append instruction (list 'long)) index)
                       optimal))
                   (optimal-table-value (sum-byte-count value-table (reverse instruction) optimal-table encoding-size))
                   (gain               (- old-gain optimal-table-value))
                   (new-old-gain       optimal-table-value)
                   (new-index          (+ index 1)))
              (loop
                new-index
                new-old-gain
                (cons gain lst)))
            (reverse lst)))))


    (define solution (make-table))
    (define running-sums (make-table))


    (define (recalculate)
      (for-each
        (lambda (encoding)
          (if (and (pair? encoding) 
                   (table-ref stats (car encoding) #f) 
                   ;; FIXME: this check is needed because the instruction might be missing
                   (table-ref (table-ref stats (car encoding)) (cadr encoding) #f)) 
            (table-set!
              running-sums
              encoding
              (normalize
                (get-running-sum
                  ((if (memq 'short encoding)
                     calculate-gain-short
                     calculate-gain-long)
                   (table-ref
                     (table-ref stats (car encoding))
                     (cadr encoding) #f)
                   (list (car encoding)
                         (cadr encoding))
                   encoding-size-counter
                   (table-ref solution encoding)
                   solution
                   encoding-size
                   ))))))
        encodings))

    (define (select-winner)
      (let ((winner-inst 0)
            (winner-value 0)
            (winner-index 0))

        (for-each
          (lambda (sum)
            (let ((instruction (car sum)))
              (let loop ((index 0) (lst (cdr sum)))
                (if (pair? lst)
                  (begin
                    (if (> (car lst) winner-value)
                      (begin
                        (set! winner-inst  instruction)
                        (set! winner-value (car lst))
                        (set! winner-index index)))
                    (loop
                      (+ 1 index)
                      (cdr lst)))))))
          (table->list running-sums))
        (list winner-inst winner-index winner-value)))


    ;; starting, set size 1 for long encodings
    (for-each
      (lambda (encoding)
        (table-set!
          solution
          encoding
          (if (and (pair? encoding) (memq 'short encoding))
            0
            (begin (set! encoding-size-counter (- encoding-size-counter 1)) 1))))
      encodings)


    (if (< encoding-size-counter 0)
      (error "Encoding size is not big enough to fit all encodings" encoding-size-counter))

    (let loop ()
      (recalculate)
      (let ((winner (select-winner)))
        (if (not (eqv? (car winner) 0))
          (begin
            (table-set! solution (car winner) (+ (cadr winner) (table-ref solution (car winner))))
            (set! encoding-size-counter (- encoding-size-counter (cadr winner)))
            (if (< 0 encoding-size-counter)
              (loop))))))

    solution)


(define (encoding-table->encoding-list encoding-table)
  (map (lambda (x) (list (car x) (cdr x))) (table->list encoding-table)))

(define (encoding-list->encoding-table encoding-list)
  (let ((table (make-table)))
    (for-each (lambda (x) (table-set! table (car x) (cadr x))) encoding-list)
    table))

(define (lzss-variable-cost encoding-size tag)
  (define (bit-in-num x)
    (if (eqv? x 0)
      1
      (ceiling (log (+ x 1) (quotient encoding-size 2)))))

  (define cost
    (lambda (x)
      (if (pair? x)
        (+
          (bit-in-num (car x))
          (bit-in-num (cadr x))
          1)
        (if tag
          1
          2))))
  cost)

;(define (decompress-lzss-2b stream bit-header length-header offset-header)
;  (define header-tag (if (eqv? bit-header 2) 192 128))
;
;  (define ones (lambda (x) (- (arithmetic-shift 1 x) 1)))
;
;  ;; gives a number with a n 1 followed by m 0 in the binary representation
;  (define mask (lambda (n m) (arithmetic-shift (ones n) m)))
;
;
;
;  (define (decode stream tail)
;    (if (pair? stream)
;      (if (>= (car stream) header-tag)
;        (let ((first-bit (car stream))
;              (second-bit (cadr stream)))
;          (decode
;            (cddr stream)
;            (cons
;              (list
;                (+
;                  (* 256 (bitwise-and first-bit (mask (- offset-header 8) 0)))
;                  second-bit)
;                (+ 3 (arithmetic-shift (bitwise-and first-bit (mask length-header (- offset-header 8))) (- 8 offset-header))))
;              tail)))
;        (decode
;          (cdr stream)
;          (cons (car stream) tail)))
;
;
;
;      tail))
;
;  (decode stream '()))

(define (decode-lzss-2b stream compression-range-size size-base byte-base )

  (define ribn-base (- byte-base compression-range-size))

  (define (decode stream tail)
    (if (pair? stream)
      (if (>= (car stream) ribn-base)
        (let* ((first-code (car stream))
               (second-code (cadr stream))
               (combined (+ (* (- first-code ribn-base) byte-base) second-code))
               (decoded-first (quotient combined size-base))
               (decoded-second (+ 3 (modulo combined size-base))))

          (decode
            (cddr stream)
            (cons
              (list
                decoded-first
                decoded-second)
              tail)))
        (decode
          (cdr stream)
          (cons (car stream) tail)))



      tail))

  ;(pp (list 'decode-lzss-2b 'stream compression-range-size size-base byte-base ))

  (decode stream '()))

(define (encode-lzss-2b stream compression-range-size size-base byte-base host-config)

  (define ribn-base (- byte-base compression-range-size))

  (define (encode stream tail)
    (if (pair? stream)
      (let ((code (car stream)))
        (if (pair? code)
          (let* ((offset (car code))
                 (len (cadr code))
                 (n   (+ (- len 3) (* offset size-base)))
                 (first (quotient n byte-base))
                 (second  (modulo n byte-base)))
            (encode
              (cdr stream)
              (cons
                (+ ribn-base first)
                (cons
                  second
                  tail))))
          (encode
            (cdr stream)
            (cons code tail))))
      tail))

  (let* ((encoded-stream
           (LZSS
             stream
             (- (quotient (* compression-range-size byte-base) size-base) 1)
             size-base
             byte-base
             (lambda (x) (if (pair? x) 2 1))))
         (return (encode
                   encoded-stream
                   '()))
         ;(_ (pp (list 'encode-lzss-2b 'stream compression-range-size size-base byte-base 'host-config '=> (length stream) (length return))))
         (dec (decode-lzss-2b
                return
                compression-range-size
                size-base
                byte-base)))

    ;(pp (map list dec encoded-stream return))
    ;(pp (length dec))
    ;(pp (length encoded-stream))
    ;(pp (filter (lambda (x) (not (equal? (car x) (cadr x)))) (map list dec encoded-stream)))

    (if (not (equal? dec
                encoded-stream))
      (error "Decompression failed"))

    (list (length stream) (length return) return)))


(define (encode-lzss-with-tag stream encoding-size host-config)

  (define encoding-size/2 (quotient encoding-size 2))

  (define (encode stream tail tag replacement)
    (cond
      ((not (pair? stream))
       tail)
      ((pair? (car stream))
       (let ((elem (car stream)))
         (encode
           (cdr stream)
           (cons
             tag
             (encode-n
               (car elem)
               (encode-n
                 (cadr elem)
                 tail
                 encoding-size/2)
               encoding-size/2))
           tag
           replacement)))
      ((eqv? (car stream) tag)
       (encode
         (cdr stream)
         (append
           replacement
           tail)
         tag
         replacement))

      ((pair? stream)
        (encode (cdr stream)
                (cons (car stream)
                      tail)
                tag
                replacement))

      (else
        (error "idk how you did end up here" stream))))

  (define (find-tag stream)
    (define value-table (make-table))

    (let loop ((stream stream))
      (if (pair? stream)
        (begin
          (table-set! value-table (car stream) (+ 1 (table-ref value-table (car stream) 0)))
          (loop (cdr stream)))))

    (caar (list-sort
           (lambda (x y)
             (< (cdr x) (cdr y)))
           (table->list value-table))))


  (define (add-variables! host-config tag)
    (let ((tag-as-string (stream->string (list tag))))
      (host-config-feature-add!
        host-config
        'compression/lzss/tag-as-code
        tag)
      (host-config-feature-add!
        host-config
        'compression/lzss/tag-as-byte
        (char->integer (string-ref tag-as-string 0)))
      (host-config-feature-add!
        host-config
        'compression/lzss/tag-as-string
        tag-as-string)))


  (let* ((tag (find-tag stream))
         (encoded-stream
           (LZSS
             stream
             9999999999999999
             9999999999999999
             encoding-size
             (lzss-variable-cost encoding-size tag))))

  (add-variables! host-config tag)

  (encode
    encoded-stream
    '()
    tag
    (list tag 0))))


;; Inspired from the section 2.6.4 of https://ir.canterbury.ac.nz/bitstream/handle/10092/8411/bell_thesis.pdf?sequence=1&isAllowed=y
;;
;;   cost-func : function that calculates cost of encoding a value. A value can be a backward pointer
;;        or the integer value. This function must return an integer that represent the number of bytes
;;        encoded by its parameter
;;

(define (LZSS stream N F encoding-size cost-func)
  (define already-encoded-size N)
  (define look-ahead-buffer-size F)

  (define (find-matches stream match already-encoded-size look-ahead-buffer-size)
    (let loop ((stream stream)
               (index 0)
               (matched
                 (list
                   (car match))) ;; '((index length (first-char-didnt-match | pair to match) ))
               (matching
                 '()))

      (if (and (pair? stream)
               (< index (+ look-ahead-buffer-size already-encoded-size)))
        (let ((matched-matching
                (let loop2 ((matched matched) (matching '()) (iter matching))
                  (if (pair? iter)
                    (let* ((curr (car iter))
                           (index (car curr))
                           (length (cadr curr))
                           (rest (caddr curr)))
                      (if (and
                            (pair? rest) ;; The last char of rest cannot match
                            (< length look-ahead-buffer-size)
                            (eqv? (car stream) (car rest)))
                        (loop2
                          matched
                          (cons
                            (list
                              index
                              (+ 1 length)
                              (cdr rest))
                            matching)
                          (cdr iter))
                        (loop2
                          (cons
                            (list
                              index
                              length)
                            matched)
                          matching
                          (cdr iter))))

                    (list matched matching)))))

          (loop
            (cdr stream)
            (+ 1 index)
            (car matched-matching)
            (append
              (if (and (eqv? (car stream) (car match))
                       (< index already-encoded-size))
                (list
                  (list
                    (abs (- index already-encoded-size))
                    1
                    (cdr match)))
                '())
              (cadr matched-matching))))
        matched)))

    (define (gain x)
      (let ((cost (cost-func x))
            (gain (if (pair? x)
                    (cadr x)
                    1)))
        (- gain cost)))

    (define (get-longest-match matchings)
      (let ((sorted
              (list-sort
                (lambda (x y)
                  (if (= (gain x) (gain y))
                    (not (pair? x))
                    (> (gain x) (gain y))))

                matchings)))
        (car sorted)))


    (let loop ((stream stream)
               (look-ahead-buffer stream)
               (offset 0)
               (output '()))
      (if (pair? look-ahead-buffer)
        (let* ((longest-match
                 (get-longest-match
                   (find-matches
                     stream
                     look-ahead-buffer
                     offset
                     look-ahead-buffer-size)))
               ;(_ (pp longest-match))
               (match-length (if (pair? longest-match) (cadr longest-match) 1))
               (new-offset-temp (+ offset match-length))
               (new-offset (min new-offset-temp already-encoded-size))
               (offset-overflow (- new-offset-temp new-offset))
               (new-stream
                 (list-tail stream offset-overflow))
               (new-look-ahead-buffer
                 (list-tail look-ahead-buffer match-length)))
          (loop
            new-stream
            new-look-ahead-buffer
            new-offset
            (cons
              longest-match
              output)))
        output)))


(define (get-or-create table val)
  (let ((x (table-ref table val #f)))
    (if x
      x
      (begin
        (let ((new-table (make-table)))
          (table-set! table val new-table)
          new-table)))))

(define stats (make-table))

(define (add-stat stats op-arg-sym arg)
  (let loop ((arg-table stats) (keys op-arg-sym))
    (if (pair? keys)
      (loop (get-or-create arg-table (car keys)) (cdr keys))
      (table-set! arg-table arg (+ 1 (table-ref arg-table arg 0))))))


(define (get-byte-count arg-list arg encoding-table encoding-size)

  (if (equal? arg-list '(if))
    1
    (let* ((short-key   (append arg-list '(short)))
           (long-key    (append arg-list '(long)))
           (short-size  (table-ref encoding-table short-key))
           (long-size   (table-ref encoding-table long-key)))
      (if (< arg short-size)
        1
        (+ 2 (floor
               (log
                 (max 1 (- arg (* (quotient encoding-size 2) (- long-size 1))))
                 (quotient encoding-size 2))))))))

(define (sum-byte-count table keys encoding-table encoding-size)
  (fold
    (lambda (pair acc)
      (let ((value (cdr pair)))
        (if (table? value)
          (+ acc (sum-byte-count value (cons (car pair) keys) encoding-table encoding-size))
          (+ acc (* (cdr pair) (get-byte-count (reverse keys) (car pair) encoding-table encoding-size))))))
    0
    (table->list table)))

(define (display-stats-aux stats level max-level encoding-table)
  (define (sort-numbers lst)
    (if (and (pair? (car lst))
             (number? (caar lst)))
      (list-sort (lambda (x y) (< (car x) (car y))) lst)
      lst))


  (if (< (length level) max-level)
    (for-each
      (lambda (pair)
        (let* ((key (car pair))
               (value (cdr pair))
               (level (cons key level))
               (int-value (if (table? value)
                            (sum-byte-count value level (encoding-list->encoding-table encoding-table) (encoding-size encoding-table))
                            (* value (get-byte-count (reverse (cdr level)) key (encoding-list->encoding-table encoding-table) (encoding-size encoding-table)))))
               (spacing (make-string (* 2 (length level)) #\space)))
          (display
            (string-append
              spacing
              (if (number? key)
                (number->string key)
                (symbol->string key))

              (if (number? key)
                (string-append " : " (number->string value))
                "")
              " [ "
              (number->string int-value)
              " bytes ]"))
          (newline)
          (if (table? value)
            (display-stats-aux
              value
              level
              max-level
              encoding-table))))

      (sort-numbers (table->list stats)))))


(define (display-stats stats max-value encoding-table)
  (display-stats-aux stats '() max-value encoding-table))

(define (get-stat-from-raw stats stream)
  (if (rib? stream)
    (let ((f0 (field0 stream))
          (f1 (field1 stream))
          (f2 (field2 stream)))
      (add-stat
        stats
        (if (symbol? f0)
          (list f0)
          f0)
        f1)

      (get-stat-from-raw stats f2))
    stats))

(define (symtbl->string symtbl symbols* encoding-size)
  (string-append
    (stream->string
    (encode-n
      (- (table-length symtbl)
         (length symbols*))
      '()
      (quotient encoding-size 2)))
    (string-concatenate
      (map (lambda (s)
             (let ((str (symbol->str s)))
               (list->string
                 (reverse (string->list str)))))
           symbols*)
      ",")
    ";"))

(define (symtbl->stream symtbl symbols* ribn-base byte-base)
  (encode-n
    (- (table-length symtbl)
       (length symbols*))
    (append
      (string->stream
        (string-concatenate
          (map (lambda (s)
                 (let ((str (symbol->str s)))
                   (list->string
                     (reverse (string->list str)))))
               symbols*)
          ",")
        byte-base)
      (string->stream ";" byte-base))
    (quotient ribn-base 2)))

(define (string->stream string encoding-size)
  (cond
    ((eqv? encoding-size 256)
     (map char->integer (string->list string)))
    ((eqv? encoding-size 92)
     (map
      (lambda (c)
        (let ((n (char->integer c)))
          (if (= n 33)
            (- 92 35)
            (- n 35))))
      (string->list string)))
    (else
      (error "Cannot transform string to stream, wrong encoding" encoding-size))))

;; supposes the 92 encoding scheme
(define (stream->string stream)
  (list->string
   (map (lambda (n)
          (let ((c (+ n 35)))
            (integer->char (if (= c 92) 33 c))))
        stream)))

(define (encoding-optimal-order encoding)
  (define order
    '((jump int short)  ; 0
      (jump int long)   ; 1
      (jump sym short)  ; 2
      (jump sym long)   ; 3
      (call int short)  ; 4
      (call int long)   ; 5
      (call sym short)  ; 6
      (call sym long)   ; 7
      (set  int short)  ; 8
      (set  int long)   ; 9
      (set  sym short)  ; 10
      (set  sym long)   ; 11
      (get  int short)  ; 12
      (get  int long)   ; 13
      (get  sym short)  ; 14
      (get  sym long)   ; 15
      (const int short) ; 16
      (const int long)  ; 17
      (const sym short) ; 18
      (const sym long)  ; 19
      (const proc short); 20
      (const proc long) ; 21
      (skip int short)  ; 22
      (skip int long)   ; 23
      if))              ; 24

  (map (lambda (value)
        (assoc value encoding))
       order))


(define (encoding-optimal-add-variables encoding host-config)
  (host-config-feature-add! host-config 'encoding/optimal/sizes (map caddr encoding))
  (host-config-feature-add! host-config 'encoding/optimal/start (map cadr encoding)))

(define (encode-hyperbyte stream)
  (let loop ((stream stream) (result '()))
    (if (pair? stream)
      (if (pair? (cdr stream))
        (loop (cddr stream)
              (cons (+ (* 16 (car stream)) (cadr stream)) result))
        (cons (car stream) result))
      result)))

(define (encode proc exports host-config byte-stats encoding-name byte-base)

  ;; 1 = 128 codes reserved for compression (128-255)
  ;; 2 = 64  codes reserved for compression (192-255)

  (define compression-range-size 0)
  (define compression-range-size-min 70) ;; must be even
  (define compression-range-size-max 70)
  (define size-base 0)
  (define size-base-min 7)
  (define size-base-max 13)

  (define (ribn-base) 
    (- byte-base compression-range-size))

  ;; state
  (let ((encoding      #f) ;; chosen encoding
        (stream        #f) ;; stream of encoded bytes (output)
        (symtbl        #f) ;; symbol table
        (stream-symtbl #f));; symbols at the beginning of the RIBN

    ;; define phases
    (define (p/enc-const)
      (set! proc (encode-constants proc host-config)))

    (define (p/enc-prog)
      (set!
        stream
        (encode-program
          proc
          symtbl
          encoding
          (encoding-inst-get encoding (list 'skip 'int 'long))
          (ribn-base))))

    (define (p/enc-symtbl)
      (let* ((symtbl-and-symbols* (encode-symtbl proc exports host-config (encoding-inst-size encoding (list 'call 'sym 'short))))
             (symbol* (cdr symtbl-and-symbols*)))
        (set! symtbl   (car symtbl-and-symbols*))
        (set! stream-symtbl (symtbl->stream symtbl symbol* (ribn-base) byte-base))))

    (define (p/comp-tag)
      (set! stream
        (encode-lzss-with-tag
          stream
          (ribn-base)
          host-config)))

    (define (p/comp-2b)
      (set! compression-range-size compression-range-size-min)
      (set! encoding (optimal-encoding))
      (p/enc-symtbl)
      (p/enc-prog)
      (p/merge-prog-sym)
      (let loop1 ((crs compression-range-size-min) (best-compression (list 99999999999 99999999999)))
        (if (<= crs compression-range-size-max)
            (let loop2 ((sb size-base-min) (best-compression best-compression))
              (if (<= sb size-base-max)
                  (begin
                    (set! compression-range-size crs)
                    (set! size-base sb)

                    (let* ((compression
                            (encode-lzss-2b
                             stream
                             compression-range-size
                             size-base
                             byte-base
                             host-config))
                           (ribn-size
                            (car compression))
                           (compressed-ribn-size
                            (cadr compression))
                           (stream
                            (caddr compression)))
                      (loop2 (+ sb 1)
                             (if (< compressed-ribn-size (cadr best-compression))
                                 (begin
                                   ;(pp (list 'encode-lzss-2b 'stream compression-range-size size-base byte-base 'host-config '=> ribn-size compressed-ribn-size))
                                   (append compression (list sb crs encoding)))
                                 best-compression))))
                  (begin
                    (let ((new-crs(+ crs 2) ))
                      (set! compression-range-size new-crs)
                      (set! encoding (optimal-encoding))
                      (p/enc-symtbl)
                      (p/enc-prog)
                      (p/merge-prog-sym)
                      (loop1 new-crs best-compression)))))
            (let ()

              ;;TODO: fixme!


              (define (add-variables! host-config ribn-size compressed-ribn-size)
                (host-config-feature-add!
                 host-config
                 'compression/lzss/2b/byte-base
                 byte-base)
                (host-config-feature-add!
                 host-config
                 'compression/lzss/2b/size-base
                 size-base)
                (host-config-feature-add!
                 host-config
                 'compression/lzss/2b/ribn-base
                 (ribn-base))
                (host-config-feature-add!
                 host-config
                 'compression/lzss/2b/ribn-size
                 ribn-size)
                (host-config-feature-add!
                 host-config
                 'compression/lzss/2b/compressed-ribn-size
                 compressed-ribn-size))

              (set! size-base (cadddr best-compression))
              (set! compression-range-size (car (cddddr best-compression)))
              (set! encoding (cadr (cddddr best-compression)))

              (add-variables! host-config (car best-compression) (cadr best-compression))

              (set! stream (caddr best-compression))))))

    (define (p/merge-prog-sym)
      (set! stream (append stream-symtbl stream)))


    (define (optimal-encoding)
      (let* ((symtbl-and-symbols* (encode-symtbl proc exports host-config 20)) ;; we assume 20 shorts, will be re-evaluated
             (raw-stream (encode-program proc (car symtbl-and-symbols*) 'raw #t (ribn-base)))
             (stats (get-stat-from-raw (make-table) raw-stream))
             (encoding
               (calculate-start
                 (encoding-optimal-order
                   (encoding-table->encoding-list
                     (get-maximal-encoding
                       (map car encoding-skip-92)
                       stats
                       (ribn-base)))))))
        ;(encoding-optimal-add-variables encoding host-config)
        encoding))



    ;; dispatch

    (define (live? . syms)
      (let loop ((syms syms))
        (if (null? syms)
          #f
          (if (host-config-feature-live? host-config (car syms))
            #t
            (loop (cdr syms))))))

    (let*
      ;; options
      ((compression/2b? (live? 'compression/lzss/2b))
       (compression/tag? (live? 'compression/lzss/tag 'compression/lzss 'compression))
       (compression? (or compression/2b? compression/tag?))
       (hyperbyte? (live? 'encoding/hyperbyte)))


      ;; Dispatch logic
      (p/enc-const) ;; always encode constants

      ;; Choose encoding
      (set! encoding
        (cond
          ((and (string=? "original" encoding-name)
                (eqv? (ribn-base) 92))
           encoding-original-92)
          ((and (string=? "skip" encoding-name)
                (eqv? (ribn-base) 92))
           encoding-skip-92)
          ((string=? "optimal" encoding-name)
           (optimal-encoding))
          (else
            (error "Cannot find encoding (or number of byte not supported) :" encoding-name))))



      (p/enc-symtbl)
      (p/enc-prog)

      ;; Apply hyperbyte
      (if hyperbyte?
        (set! stream (encode-hyperbyte stream)))

      ;; compression on 92 applies only on the program (not symtbl)
      (if (and compression/2b? (eqv? byte-base 92))
        (p/comp-2b))

      ;; merge symtbl and prog
      (p/merge-prog-sym)

      ;; apply compression
      (if compression/tag?
        (p/comp-tag))

      (if (and compression/2b? (eqv? byte-base 256))
        (p/comp-2b))


      (host-config-feature-add!
        host-config
        'encoding/ribn-base
        (ribn-base))

      (host-config-feature-add!
        host-config
        'encoding/half-ribn-base
        (quotient (ribn-base) 2))

      (if (string=? "optimal" encoding-name)
        (encoding-optimal-add-variables encoding host-config))

      stream)))

(define (encode-n n stream encoding-size/2)
  (encode-n-aux n stream stream encoding-size/2))

(define (encode-n-aux n stream end encoding-size/2)
  (let ((q (quotient n encoding-size/2)))
    (let ((r (- n (* q encoding-size/2))))
      (let ((t (cons (if (eqv? stream end) r (+ r encoding-size/2)) stream)))
        (if (= q 0)
          t
          (encode-n-aux q t end encoding-size/2))))))



(define (encode-program proc syms encoding skip-optimization? encoding-size)

  (define skip-optimization (if (eqv? encoding 'raw)
                              #t
                              skip-optimization?))


  (define encoding-size/2 (quotient encoding-size 2))

  (define (encode-sym o)
    (let ((descr (table-ref syms o #f)))
      (field0 descr)))

  (define (encode-long1 code n stream)
    (cons code (encode-n n stream encoding-size/2)))


  (define (encode-long2 code0 n stream)
    (let ((s (encode-n n stream encoding-size/2)))
      (let ((x (car s)))
        (if (= x (+ encoding-size/2 1))
          (cons (+ code0 1) (cdr s))
          (cons code0 s)))))

  (define (encode-long code long-size n stream)
    (let ((s (encode-n n stream encoding-size/2)))
      (let ((x (car s)))
        (let ((overflow (- x encoding-size/2)))
          (if (and (> overflow 0)
                   (< overflow long-size))
            (cons (+ code overflow) (cdr s))
            (cons code s))))))



  (define (sublist-eq? left right result)
    (if (and (pair? left)
             (pair?  right)
             (eqv? (c-rib-oper (car left))
                   (c-rib-oper (car right)))
             (eqv? (c-rib-opnd (car left))
                   (c-rib-opnd (car right))))
      (sublist-eq? (cdr left) (cdr right) (cons (car left) result))
      (reverse result)))



  (define (enc-inst arg op-sym arg-sym encoding-table stream)
    (if (eq? encoding-table 'raw)
      (rib (list op-sym arg-sym) arg stream)
      (let* ((short-key   (list op-sym arg-sym 'short))
             (long-key    (list op-sym arg-sym 'long))
             (short-size  (encoding-inst-size encoding-table short-key))
             (long-size   (encoding-inst-size encoding-table long-key))
             (short-start (encoding-inst-start encoding-table short-key))
             (long-start  (encoding-inst-start encoding-table long-key)))

        (add-stat stats (list op-sym arg-sym) arg)

        (if (< arg short-size)
          (cons (+ short-start arg)
                stream)
          (cond
            ;; 1 and 2 are there because the 3rd branch is untested yet
            ((eqv? long-size 1)
             (encode-long1 long-start arg stream))
            ((eqv? long-size 2)
             (encode-long2 long-start arg stream))
            (else
              (encode-long long-start long-size arg stream)))))))


  (define (enc-proc arg encoding limit stream)
    (let ((code (c-procedure-code arg)))
      (let ((nparams (c-rib-oper code)))
        (if (or (eq? limit #f) (> limit 0))
          (enc (c-rib-next code)
               encoding
               (and limit (- limit 1))
               (enc-inst nparams 'const 'proc encoding stream))
          stream))))

  (define (reverse-code code tail)
    (if (rib? (c-rib-next code))
      (reverse-code (c-rib-next code) (cons code tail))
      (cons code tail)))

  (define (enc code encoding limit stream)
    (cond
      ((not (rib? code)) (error "Rib expected, got :" code))
      ((and limit (<= limit 0)) stream)
      (else
        (let* ((op      (c-rib-oper code))
               (arg     (c-rib-opnd code))
               (next-op (c-rib-next code))
               (op-sym
                 (cond ((eqv? op jump/call-op)
                        (if (eqv? 0 next-op)
                          'jump
                          'call))
                       ((eqv? op set-op)
                        'set)
                       ((eqv? op get-op)
                        'get)
                       ((eqv? op const-op)
                        'const)
                       (else
                         'special)))
               (arg-sym
                 (cond ((eqv? 'special op-sym)
                        'special)
                       ((number? arg)
                        'int)
                       ((symbol? arg)
                        'sym)
                       ((and (c-procedure? arg) (eqv? 'const op-sym))
                        'proc)
                       (else
                         (error (string-append "can't encode " (symbol->string op-sym))
                                code)))))
          (cond
            ((and (eq? op-sym 'const) ;; special case for encoding procedures
                  (eq? arg-sym 'proc))
             (enc next-op encoding (and limit (- limit 1)) (enc-proc arg encoding #f stream)))
            ((not (eq? 'special op-sym)) ;; "normal" encoding
             (let ((encoded-inst
                     (enc-inst
                       (if (eq? arg-sym 'sym)
                         (encode-sym arg)
                         arg)
                       op-sym
                       arg-sym
                       encoding
                       stream)))
               (if (eq? 'jump op-sym)
                 encoded-inst
                 (enc next-op
                      encoding
                      (and limit (- limit 1))
                      encoded-inst))))
            ((eqv? op if-op) ;; special case for if
             (add-stat stats '(if) 0)
             (if skip-optimization ;; if optimization
               (let* ((rev-next (reverse-code (c-rib-next code) '()))
                      (rev-opnd (reverse-code (c-rib-opnd code) '()))
                      (sublist  (sublist-eq? rev-next rev-opnd '()))
                      (sublist-length (length sublist))
                      (opnd-different-length (- (length rev-opnd) sublist-length))
                      (next-different-length (- (length rev-next) sublist-length))
                      (tail     (if (eqv? encoding 'raw)
                                  (rib 'if 0 stream)
                                  (cons (encoding-inst-start encoding 'if) stream))))
                 (enc
                   (c-rib-next code)
                   encoding
                   (and limit (- limit 1))
                   (if (pair? sublist)
                     (enc-inst next-different-length
                               'skip
                               'int
                               encoding
                               (enc (c-rib-opnd code)
                                    encoding
                                    opnd-different-length
                                    tail))
                     (enc (c-rib-opnd code)
                          encoding
                          #f
                          tail))))

               (enc (c-rib-next code)
                    encoding
                    (and limit (- limit 1))
                    (enc (c-rib-opnd code)
                         encoding
                         #f
                         (cons (encoding-inst-start encoding 'if) stream)))))
            (else
              (error "Cannot encode instruction" code)))))))

  (enc-proc proc encoding #f '()))



(define (string->codes string)
  (map char->integer (string->list string)))

;;;----------------------------------------------------------------------------

;; Source code reading.

(define (root-dir)
  (rsc-path-directory (or (script-file) (executable-path))))

(define (ribbit-root-dir) ;; TODO: make it work (maybe with a primitive or a env variable)
  (root-dir))

; Path where to find the ribbit libraries
(define ribbit-path (list (path-expand "lib" (ribbit-root-dir))))

(define (read-from-file path)

  (if (not (file-exists? path))
    (error "*** Cannot find source file at location" path))
  
  (let* ((port (open-input-file path))
         (first-line (read-line port #\newline))
         (port (if (and (not (eof-object? first-line)) (string-prefix? "#!" first-line))
                 port
                 (begin
                   (close-input-port port)
                   (open-input-file path)))))
    (read-all port)))

(define (read-library lib-path)
  `((##include-once (ribbit ,lib-path))))

(define (read-program lib-path src-path)
  (append (apply append (map read-library lib-path))
          (if (equal? src-path "-")
              (read-all)
              (read-from-file src-path))))

;;;----------------------------------------------------------
;;;=================== HOST FILE PARSING ====================
;;;----------------------------------------------------------

;; PARSE_HOST_FILE
;; inputs: a string that represents a host-file
;; outputs: a pair (lines-left-to-parse .  parsed-host-file)
;; 
;; The returned parsed-host file follows this grammar:
;;   <HOST_FILE> ::= (<HOST_FILE_STATEMENT>*)
;;   <HOST_FILE_STATEMENT> ::= <CONTENT> | <MACRO>
;;   <CONTENT> ::= "string"
;;   <MACRO> ::= (<MACRO_NAME> <HOST_FILE> . <MACRO_ARGUMENTS>)
;;   <MACRO_NAME> ::= 'symbol
;;   <MACRO_ARGUMENTS> ::= any sexp list, ex: ('I < 3 "scheme")
;;
;; Look at host-tests under tests/hosts/* for examples.
;; 
;; Note that the embedded macros can be nested. For example, 
;; 
;; // @@(feature a
;; //   @@(feature b
;; some_statement
;; //   )@@
;; // )@@

(define (parse-host-file file-content)
  (let loop ((lines 
               (if (pair? file-content) 
                 file-content 
                 (string-split file-content #\newline)))
             (parsed-file '())
             (cur-section '()))

    (if (pair? lines)
      (let* ((cur-line (car lines))
             (macro (detect-macro cur-line))
             (macro-type  (car macro))
             (macro-value (cadr macro))
             (next-section
               (cons cur-line cur-section)))
        
        (case macro-type
          ((none)
           (loop (cdr lines) parsed-file next-section))

          ((end-outer)
           (let ((body (reverse `(,@next-section . ,parsed-file))))
             (cons (cdr lines)
                   body)))
          ((inner)
           (let* ((macro-value (string-append "(" macro-value ")"))
                  (macro-sexp  (read (open-input-string macro-value))))
             (loop (cdr lines)
                   `(,@macro-sexp ,@cur-section . ,parsed-file)
                   '())))

          ((start-outer start-end-outer)
           (let* ((macro-value (string-append "(" macro-value ")"))
                  (macro-sexp (read (open-input-string macro-value)))
                  (body-pair (if (eqv? macro-type 'start-outer)
                               (parse-host-file (cdr lines)) ; recursive call
                               (cons (cdr lines) '())))
                  (lines-after-body (car body-pair))
                  (parsed-subfile  (cons (car lines) (cdr body-pair))))
             (loop lines-after-body
                   `((,(car macro-sexp) ,parsed-subfile ,@(cdr macro-sexp))
                     ,@cur-section
                     . 
                     ,parsed-file)
                   '())))
          (else
           (error "Unknown macro type" macro-type))))
      (reverse `(,@cur-section . ,parsed-file)))))

;; DETECT-MACRO
;; Host file expression parsing, evalutation and substitution

;; Detects a macro on a given line.
;; In the following, assume "..." is any sexp. There are 3 types of macros :
;;   - start-outer     : '@@( ...' or '@@( ... @@'
;;   - end-outer       : '... )@@'
;;   - start-end-outer : '@@( ... )@@'
;;
;; Note: @@ can force the end of the parsing on the same line. This is to support
;;  hosts that have an ending comments symbols such as ML that needs comments inside (* ... *).
(define (detect-macro line)
  (let ((len (string-length line)))
    (let loop ((cur (string->list line))
               (i 0)
               (start #f)
               (end #f)
               (macro-type 'none))
      (if (<= (- len i) 2)
        (let ((start (or start 0))
              (end (or end len)))
          (if (eq? macro-type 'none)
            (list 'none #f)
            (list macro-type (substring line start end))))
        (let* ((match-start-outer (list-prefix? (string->list "@@(") cur)) ;  ) (
               (match-end-outer   (list-prefix? (string->list ")@@") cur)) ;
               ;(match-start-inner (list-prefix? (string->list "@..") cur))
               ;(match-end-inner   (list-prefix? (string->list "..@") cur))
               (match-end-outer-same-line
                 (and (list-prefix? (string->list "@@") cur)
                      (not match-start-outer)
                      (not match-end-outer)))
               (match-start       match-start-outer)
               (match-end         (or
                                    match-end-outer
                                    match-end-outer-same-line))
               (match-any         (or match-start match-end)))
          (if (or (not match-any) end) ;; no match or already found the end
            (loop (cdr cur) (+ i 1) start end macro-type)
            (begin

              (if (and start match-start)
                (error "Cannot start two macros @@( on the same line")) ; )

              ;(if (and (not start) match-end-inner)
              ;  (error "The ..@ macro must start with @.."))

              ;(if (and (eq? macro-type 'inner) (or match-start-outer match-end-outer))
              ;  (error "Cannot mix and match outer and inner macros"))

              ;(if (and (not (or (eq? macro-type 'inner) (eq? macro-type 'none)))
              ;         (or match-start-inner match-end-inner))
              ;  (error "Cannot mix and match inner or outer macros macros"))

              (loop
                (cdddr cur)
                (+ i 3)
                (if match-start (+ i 3) start)
                (if match-end i end)
                (cond
                  (match-start-outer 'start-outer)
                  (match-end-outer   (if start 'start-end-outer 'end-outer))
                  (else macro-type))))))))))

;; EXTRACT
;; inputs: a walker function, a parsed file, and a base value
;; outputs: the result of the walker function
;;
;; This is a healper similar to fold, but that can be called recursively
;;   easily. This is usefull for parsing recursive structures, such as the
;;   parsed host file.
(define (extract walker parsed-file base)
  (letrec ((func
             (lambda (prim acc)
               (let ((rec (lambda (base body) (fold func base body))))
                 (walker prim acc rec)))))
    (fold
      func
      base
      parsed-file)))

(define (find predicate lst)
  (if (pair? lst)
    (if (predicate (car lst))
      (car lst)
      (find predicate (cdr lst)))
    #f))

(define (soft-assoc sym lst)
  (find (lambda (e) (and (pair? e) (eq? (car e) sym)))
        lst))

(define (all predicate lst)
  (not (find (lambda (x) (not (predicate x))) lst)))

(define (extract-feature-names parsed-file)
  (define (extract-name name)
    (if (pair? name)
      (let ((first (car name)))
        (case first
          ((and or not) (apply append (map extract-name (cdr name))))
          (else name)))
      (list name)))

  (let ((features (extract-features parsed-file)))
    (fold
      (lambda (prim acc)
        (if (eqv? (car prim) 'feature)
         (unique (append acc (extract-name (caddr prim))))
         acc))
      '()
      features)))

(define (extract-features parsed-file)
  (extract
    (lambda (prim acc rec)
      (if (pair? prim)
        (case (car prim)
          ((primitives)
           (let ((primitives (rec '() (cadr prim))))
             (append primitives acc)))

          ((primitive feature)
           (let* ((inner-features (rec '() (cadr prim)))
                  (inner-features ;; add `and` conditions on inner features
                    (map
                      (lambda (x) 
                        `(,(car x)  ;; primitive or feature
                          ,(cadr x) ;; recursive 
                          (and ,(caddr prim) ,(caddr x))
                           .
                           ,(cdddr x)))
                      inner-features)))
             `(,prim ,@inner-features . ,acc)))
          (else
            acc))
        acc))
    parsed-file
    '()))


(define (list-prefix? prefix lst)
  (if (pair? prefix)
    (if (pair? lst)
      (if (eqv? (car prefix) (car lst))
        (list-prefix? (cdr prefix) (cdr lst))
        #f)
      #f)
    #t))


(define (unique-aux lst1 lst2)
  (if (pair? lst1)
    (if (memq (car lst1) lst2)
      (unique-aux (cdr lst1) lst2)
      (unique-aux (cdr lst1) (cons (car lst1) lst2)))
    lst2))

(define (unique lst)
  (unique-aux lst '()))

(define (eval-feature expr true-values)
  (cond ((and (pair? expr) (eq? (car expr) 'not))
         (not (eval-feature (cadr expr) true-values)))
        ((and (pair? expr) (eq? (car expr) 'and))
         (not (memv #f (map (lambda (x) (eval-feature x true-values)) (cdr expr)))))
        ((and (pair? expr) (eqv? (car expr) 'or))
         (not (not (memv #t (map (lambda (x) (eval-feature x true-values)) (cdr expr))))))
        (else
          (not (not (assq-value expr true-values))))))

(define (filter-pair predicate lst)
  (let loop ((lst lst) (lst-true '()) (lst-false '()))
    (if (pair? lst)
      (if (predicate (car lst))
        (loop
          (cdr lst)
          (cons (car lst) lst-true)
          lst-false)
        (loop
          (cdr lst)
          lst-true
          (cons (car lst) lst-false)))
      (cons lst-true lst-false))))

(define (used-features features live-symbols features-enabled features-disabled)
  (let* ((primitives (filter (lambda (x) (eq? (car x) 'primitive)) features))
         (live-primitives
           (filter (lambda (prim)
                     (let ((name (caadr prim)))
                       (or (memq name live-symbols)
                           (memq name features-enabled))))
                   primitives))
         (live-features-symbols (append (cons '##rib '()) ;; always add rib
                                        (filter (lambda (x) (not (memq x features-disabled)))
                                                (append features-enabled (map caadr live-primitives))))))

    (let loop ((used-features live-features-symbols)
               (features features))
      (let* ((current-features-pair
               (filter-pair
                 (lambda (feature)
                   (case (car feature)
                     ((primitive)
                      (memq (caadr feature) used-features))
                     ((feature)
                      (eval-feature (cadr feature) used-features))
                     (else (error "Cannot have a feature that is not a primitive or a feature"))))
                 features))
             (current-features (car current-features-pair))
             (not-processed (cdr current-features-pair))
             (current-uses
               (fold
                 (lambda (curr-feature acc)
                   (let ((use (soft-assoc 'use curr-feature)))
                     (if use
                       (append
                         acc
                         (filter
                           (lambda (x) (not (memq x features-disabled)))
                           (cdr use)))
                       acc)))
                 '()
                 current-features)))
        (if (pair? current-features)
          (loop
            (unique (append current-uses used-features))
            not-processed)
          used-features)))))

(define (find-primitive prim-name features)
  (if (pair? features)
    (if (and (eq? (car (car features)) 'primitive)
             (eq? prim-name (caadr (car features))))
      (car features)
      (find-primitive prim-name (cdr features)))
    #f))


(define (list->host lst prefix sep suffix)
  (string-append
    prefix
    (string-concatenate
      (map object->string lst)
      sep)
    suffix))

(define (rvm-code-to-bytes rvm-code sep)
  (string-concatenate
   (map (lambda (c) (number->string (char->integer c)))
        (string->list rvm-code))
   sep))

(define (replace-eval expr encode host-config)

  (define (encode-as-string encoding-size)
    (stream->string (encode encoding-size)))

  (define (encode-as-bytes encoding-size prefix sep suffix)
    (list->host
      (cond
        ((eqv? encoding-size 92)
         (string->list* (stream->string (encode encoding-size))))

        ((equal? encoding-size "auto")
         (let ((optimal? (host-config-feature-live? host-config 'encoding/optimal)))
          (if optimal?
           (encode 256) ;; if optimal is enabled, it supports 256 encoding
           (string->list* (stream->string (encode 92)))))) ;; else, original encoding supports 92
        (else
         (encode encoding-size)))
     prefix
     sep
     suffix))


  (define functions
    `((encode ,encode-as-string 2)
      (encode-as-bytes ,encode-as-bytes 5)
      (encode-as-string ,encode-as-string 2)
      (rvm-code-to-bytes ,rvm-code-to-bytes 3)
      (list->host ,list->host 5)))


  (let ((func (and (pair? expr) (assoc (car expr) functions)))
        (replace-eval (lambda (expr) (replace-eval expr encode host-config))))

    (cond ((and
             func
             (eqv? (length expr) (caddr func)))
           (apply (cadr func) (map replace-eval (cdr expr))))
          ((and
             func
             (not (eqv? (length expr) (caddr func))))
           (error "Wrong number of arguments in replace" expr))
          ((string? expr)
           expr)
          ((number? expr)
           expr)
          ((symbol? expr)
           (let ((feature-value (host-config-feature-live? host-config expr)))
             (if feature-value
               feature-value
               (error "Error while generating rvm, feature with empty value : " expr))))
          (else
           (error "Cannot evaluate expression in replace" expr)))))



(define (generate-file parsed-file host-config encode)

  (define live-features (host-config-features host-config))

  (define primitives (list-sort
                       (lambda (x y) (< (cadr x) (cadr y)))
                       (host-config-primitives host-config)))

  (define locations (host-config-locations host-config))

  ;; cache result of select replace evaluations
  (define cached-functions
    '(encode encode-as-bytes encode-as-string))

  (define cache (make-table))

  (extract
    (lambda (prim acc rec)

      (if (pair? prim)

        (case (car prim)
          ((feature)
           (let ((condition (caddr prim)))
             (if (eval-feature condition live-features)
               (rec "" (cadr prim))
               acc)))

          ((replace)
           (let ((expr (cadddr prim)))
             (if (and (pair? expr)
                      (memq (car expr) cached-functions)
                      (not (table-ref cache expr #f)))
               (table-set! cache expr (replace-eval expr encode host-config)))))

          (else acc))
        acc))
    parsed-file
    "")


  (letrec ((extract-func
            (lambda (prim acc rec)
              (if (string? prim)
                (cons prim acc)
                
                (case (car prim)
                  ((feature)
                   (let ((condition (caddr prim)))
                     (if (eval-feature condition live-features)
                       (append (rec '() (cadr prim)) acc)
                       acc)))

                  ((primitives)
                   (let* ((gen-pair 
                            (if (and 
                                  (pair? (cdr prim)) 
                                  (pair? (cddr prim)) 
                                  (pair? (caddr prim)) 
                                  (eq? (caaddr prim) 'gen))
                              (caddr prim)
                              (error 
                                "Ill-formed @@(primitives ...)@@ macro inside host file. Cannot find 'gen' clause"
                                prim)))
                          (gen-pattern (cdr gen-pair))
                          (generate-one
                            (lambda (prim acc)
                              (let* ((name (car prim))
                                     (index (cadr prim))
                                     (primitive (caddr prim))
                                     (primitive-signature (cadr primitive))
                                     (primitive-use (caddr primitive))
                                     (primitive-body (cadddr primitive))
                                     (whole-body (reverse (rec '() primitive-body)))
                                     (head (car whole-body))
                                     (body (cdr whole-body))
                                     (body (if (null? body) (list head) body))
                                     (body-str (string-concatenate body "\n")))

                                (cons
                                  (let loop ((gen gen-pattern)
                                             (return ""))
                                    (if (pair? gen)
                                      (loop
                                        (cdr gen)
                                        (cond 
                                          ((string? (car gen)) 
                                           (string-append return (car gen)))
                                          ((eq? (car gen) 'index)
                                           (string-append 
                                             return
                                             (if (pair? index)
                                               (number->string (car index))
                                               (number->string index))
                                             ))
                                          ((eq? (car gen) 'body) 
                                           (string-append return body-str ))
                                          ((eq? (car gen) 'head) 
                                           (string-append return head))))
                                      return))
                                  acc)))))
                     (append
                       (fold generate-one '() primitives)
                       acc)))

                  ((use)
                   (append (rec '() (cadr prim)) acc))

                  ((location)
                   (let* ((name (caddr prim))
                          (feature-pair (assoc name locations))
                          (matched-features (if feature-pair (cadr feature-pair) '())))
                     (extract extract-func matched-features acc)))

                  ((replace)
                   (let* ((pattern (caddr prim))
                          (pattern (if (symbol? pattern)
                                     (symbol->string pattern)
                                     pattern))
                          (replace-expr (cadddr prim))
                          (replacement-text
                            (or
                              (table-ref cache replace-expr #f)
                              (replace-eval replace-expr encode host-config)))
                          (replacement-text
                            (cond ((symbol? replacement-text)
                                   (symbol->string replacement-text))
                                  ((number? replacement-text)
                                   (number->string replacement-text))
                                  ((string? replacement-text)
                                   replacement-text)
                                  (else
                                    (error "Error while replacing doesn't support the type : " replacement-text)))))
                     (append
                       (map 
                         (lambda (line) (string-replace line pattern replacement-text))
                         (rec '() (cadr prim)))
                       acc)))

                  (else
                    (error "Unhandled expression in file parsing" prim)))))))

    (string-concatenate
      (reverse
        (extract
          extract-func
          parsed-file
          '()))
      "\n")))




;;;----------------------------------------------------------------------------

;; Target code generation.

(define (string-from-file path)
  (let ((file-content (call-with-input-file path (lambda (port) (read-line port #f)))))
       (if (eof-object? file-content) "" file-content)))

(define (generate-code 
          target
          verbosity
          debug-info
          input-path
          rvm-path
          exe-output-path
          output-path minify?
          host-file
          encoding-name
          byte-stats
          proc-exports-and-features)

  (let* ((proc
           (vector-ref proc-exports-and-features 0))
         (exports
           (vector-ref proc-exports-and-features 1))
         (host-config
           (vector-ref proc-exports-and-features 2))
         (encode*
          (lambda (byte-base)
            (let ((input
                   (append
                    (encode
                     proc
                     exports
                     host-config
                     byte-stats
                     encoding-name
                     byte-base)
                    (if input-path
                        (string->list* (string-from-file input-path))
                        '()))))
              (if (or (>= verbosity 1) (memq 'rvm-len debug-info))
                  (begin
                    (display "*** RVM code length: ")
                    (display (length input))
                    (display " bytes\n")))
              input))))

    (let* ((target-code-before-minification
            (if (equal? target "rvm")
                (encode* 92)   ;; NOTE: 92 is the number of code in the encoding.
                (generate-file host-file host-config encode*)))
           (target-code
            (if (or (not minify?) (equal? target "rvm"))
                target-code-before-minification
                (pipe-through
                 (path-expand
                  (string-append
                   "host/"
                   (string-append target "/minify"))
                  (root-dir))
                 target-code-before-minification))))


      (write-target-code output-path exe-output-path rvm-path target-code)

      target-code)))


(define (string-replace str pattern replacement)
  (let ((len-pattern (string-length pattern))
        (len-replacement (string-length replacement)))
    (let loop1 ((i 0) (j 0) (out '()))
      (if (<= (+ j len-pattern) (string-length str))
          (let loop2 ((k (- len-pattern 1)))
            (if (< k 0)
                (let ((end (+ j len-pattern)))
                  (loop1 end
                         end
                         (cons replacement (cons (substring str i j) out))))
                (if (char=? (string-ref str (+ j k)) (string-ref pattern k))
                    (loop2 (- k 1))
                    (loop1 i
                           (+ j 1)
                           out))))
          (string-concatenate
           (reverse (cons (substring str i (string-length str)) out))
           "")))))

(define (write-target-code output-path exe-output-path rvm-path target-code)
  (if (equal? output-path "-")
      (display target-code)
      (call-with-output-file output-path
        (lambda (port)
          (display target-code port))))

  (if exe-output-path
    (let ((status
            (shell-cmd
              (string-append
                (path-directory rvm-path)
                "mk-exe "
                output-path
                " "
                exe-output-path))))
      (if (not (equal? status 0))
        (error "Error generating executable\n")))))

;;;----------------------------------------------------------------------------

;; Progress status

(define progress-status #f)

(define (report-first-status msg cont)
  (if progress-status
    (begin (display msg) (display "...\n")))
  cont)

(define (report-done)
  (if progress-status
    (display "...Done.\n")))

(define-macro (report-status msg cont)
  `(if progress-status
     (begin
       (report-done)
       (report-first-status ,msg #f)
       ,cont)
     (begin ,cont)))

;;;----------------------------------------------------------------------------

;; Compiler entry points.


;; This version of the compiler reads the program and runtime library
;; source code from files and it supports various options.  It can
;; merge the compacted RVM code with the implementation of the RVM
;; for a specific target and minify the resulting target code.



(define target "rvm")
(define (fancy-compiler src-path
                        output-path
                        exe-output-path
                        rvm-path
                        _target
                        input-path
                        lib-path
                        minify?
                        verbosity
                        debug-info
                        _progress-status
                        features
                        encoding-name
                        byte-stats
                        call-stats?)


  (set! target _target) ;; hack to propagate the target to the expansion phase
  (set! progress-status _progress-status)
  (set! ribbit-path 
    (cons
      (path-expand 
        (string-append "host/" _target "/lib")  
        (ribbit-root-dir))
      ribbit-path))

  (let* ((vm-source
           (if (equal? _target "rvm")
             #f
             (string-from-file
               (path-expand rvm-path
                            (root-dir)))))
         (host-file
           (if (equal? _target "rvm")
             #f
             (report-first-status
               "Parsing host file"
               (parse-host-file vm-source))))

         (_ (if (memq 'host-expansion debug-info)
              (begin
                (display "*** HOST FILE EXPANSION: ")
                (newline)
                (pp host-file))))

         (encoding-name (if (equal? encoding-name "auto")
                            (let* ((available-features (extract-feature-names host-file))
                                   (encoding-order '((encoding/optimal "optimal") (encoding/original "original")))
                                   (encoding-names (map car encoding-order)))
                              (let loop ((encoding-order encoding-order))
                                (if (or (and (pair? encoding-order) (not (pair? (cdr encoding-order))))
                                        (memq (caar encoding-order) available-features))
                                  (cadar encoding-order)
                                  (loop (cdr encoding-order)))))
                            encoding-name))
         (features
           (cons
             (cons
               (string->symbol
                 (string-append "encoding/" encoding-name))
               #t)
             features))

         (program-read
           (report-status
             "Reading program source code"
             (read-program lib-path src-path)))

         (program-compiled
           (report-status
             "Compiling program"
             (compile-program
               verbosity
               debug-info
               host-file
               features
               program-read))))

      (if call-stats?
        (pp (list-sort (lambda (stat1 stat2) (> (cadr stat1) (cadr stat2))) call-stats)))

      (report-status
        "Generating target code"
        (generate-code
          _target
          verbosity
          debug-info
          input-path
          rvm-path
          exe-output-path
          output-path
          minify?
          host-file
          encoding-name
          byte-stats
          program-compiled))

      (report-done)))

(define (parse-cmd-line args)
  (define usage 
"`rsc` - The Ribbit Scheme Compiler

SYNOPSIS
`rsc` [OPTION]... [FILE]

COMPILATION OPTIONS
  `-t`, `--target TARGET`
  Specify the compilation target. Can be any of: `rvm`, `js`, `py`, etc..

  `-o`, `--output PATH`
  Define the output path for the compiled file.

  `-x`, `--exe`, `--executable PATH`
  Specify an executable output path. If no path is given, default naming is used.

  `-l`, `--library PATH`
  Add a library path.

  `-m`, `--minify`
  Enable minification of the output.

  `-e`, `--encoding NAME`
  Set the encoding name. Default is 'auto'.

  `-r`, `--rvm PATH`
  Define the RVM path. Defaults to ./host/`target`/rvm.`target`

  `-f+`, `--enable-feature FEATURE`
  Enable a specific feature.

  `-f-`, `--disable-feature FEATURE`
  Disable a specific feature.

  `-i`, `--input PATH_TO_FILE`
  Set an stdin buffer at compile time included in the resulting RVM. This is usefull to compress data as
  some information could be kept in the stdin and processed later on.

DEBUGING OPTIONS

  `-v`, `-vv`, `-vvv`
  Set verbosity level. Multiple 'v's increase verbosity.

  `-di`, `--debug-info INFO`
  Displays debug information to stdout.
  Info can be any of : `host-expansion` `expansion`, `rvm-code`, `hash-table`, `exports` and `host-config`.

  `-ps`, `--progress-status`
  Show progress status during compilation.

  `-bs`, `--byte-stats STATS`
  Set byte statistics.

  `-cs`, `--call-stats`
  Enable call statistics.

  `-q`
  Silently ignore this option (specific to Chicken Scheme).

  `-h`, `--help`
  Displays this help message.

EXAMPLE
`rsc -t c -l r4rs source.scm -o output.c -x run-output.exe -m -v`
This command compiles `source.scm` to C with verbosity set to 1 and minification enabled. 
The output is written to output.c, with an executable compiled to run-output.exe (with gcc).")

  (let ((verbosity 0)
        (debug-info '())
        (target "rvm")
        (input-path #f)
        (output-path #f)
        (exe-output-path #f)
        (lib-path '())
        (src-path #f)
        (minify? #f)
        (features '())
        (rvm-path #f)
        (progress-status #f)
        (byte-stats #f)
        (call-stats #f)
        (encoding-name "auto"))

    (let loop ((args (cdr args)))
      (if (pair? args)
        (let ((arg (car args))
              (rest (cdr args)))
          (cond ((and (pair? rest) (member arg '("-t" "--target")))
                 (set! target (car rest))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-i" "--input")))
                 (set! input-path (car rest))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-o" "--output")))
                 (set! output-path (car rest))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-l" "--library")))
                 (set! lib-path (cons (car rest) lib-path))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-m" "--minify")))
                 (set! minify? #t)
                 (loop rest))
                ((and (pair? rest) (member arg '("-e" "--encoding")))
                 (set! encoding-name (car rest))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-r" "--rvm")))
                 (set! rvm-path (car rest))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-f+" "--enable-feature")))
                 (set! features 
                   (cons 
                     (cons (string->symbol (car rest)) #t)
                     features))
                 (loop (cdr rest)))
                ((and (pair? rest) (member arg '("-f-" "--disable-feature")))
                 (set! features 
                   (cons 
                     (cons (string->symbol (car rest)) #f)
                     features))
                 (loop (cdr rest)))

                ((and (pair? rest) (pair? (cdr rest)) (member arg '("-f=" "--set-feature")))
                 (set! features 
                   (cons 
                     (cons (string->symbol (car rest)) 
                           (read (open-input-string (cadr rest)))) ;; use reader to transform information
                     features))
                 (loop (cddr rest)))

                ((and (pair? rest) (member arg '("-bs" "--byte-stats")))
                 (set! byte-stats (string->number (car rest)))
                 (loop (cdr rest)))

                ((and (pair? rest) (member arg '("-di" "--debug-info")))
                 (set! debug-info (cons (string->symbol (car rest)) debug-info))
                 (loop (cdr rest)))

                ((member arg '("-h" "--help"))
                 (display usage)
                 (exit 0))

                ((member arg '("-x" "--exe" "--executable"))
                 (if (and (pair? rest)
                          (not (string=? (substring (car rest) 0 1) "-")))
                   (begin
                     (set! exe-output-path (car rest))
                     (loop (cdr rest)))
                   (begin
                     (set! exe-output-path #t)
                     (loop rest))))

                ((member arg '("-cs" "--call-stats"))
                 (set! call-stats #t)
                 (loop rest))

                ((member arg '("-v" "--v"))
                 (set! verbosity (+ verbosity 1))
                 (loop rest))
                ((member arg '("-vv" "--vv"))
                 (set! verbosity (+ verbosity 2))
                 (loop rest))
                ((member arg '("-vvv" "--vvv"))
                 (set! verbosity (+ verbosity 3))
                 (loop rest))
                ((member arg '("-ps" "--progress-status"))
                 (set! progress-status #t)
                 (loop rest))
                ((member arg '("-q")) ;; silently ignore Chicken's -q option
                 (loop rest))
                (else
                  (if (and (>= (string-length arg) 2)
                           (string=? (substring arg 0 1) "-"))
                    (begin
                      (display "*** ignoring option ")
                      (display arg)
                      (newline)
                      (loop rest))
                    (begin
                      (set! src-path arg)
                      (loop rest))))))))

    (if (not src-path)

      (begin
        (display "*** a Scheme source file must be specified\n")
        (exit-program-abnormally))

      (fancy-compiler
        src-path
        (or output-path
            (if (or (equal? src-path "-") (equal? target "rvm"))
              "-"
              (string-append
                src-path
                (string-append "." target))))
        (and exe-output-path
             (if (string? exe-output-path)
               exe-output-path
               (string-append
                 src-path
                 (string-append "." target ".exe"))))
        (or rvm-path
            (path-expand
              (string-append
                "host/"
                (string-append
                  target
                  (string-append "/rvm." target)))
              (path-directory (car (cmd-line)))))
        target
        input-path
        (if (null? lib-path) '("empty") lib-path)
        minify?
        verbosity
        debug-info
        progress-status
        features
        encoding-name
        byte-stats
        call-stats))))

(parse-cmd-line (cmd-line))

(exit-program-normally)

;;;----------------------------------------------------------------------------
